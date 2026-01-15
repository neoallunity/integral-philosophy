#!/usr/bin/env python3
"""
Batch Processing System for Integral Philosophy publishing system.
Implements scalable batch processing for multiple publications with parallel processing.
"""

import asyncio
import json
import time
import concurrent.futures
from pathlib import Path
from typing import Dict, List, Any, Optional, Callable, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import logging
from enum import Enum
import threading
from queue import Queue
import multiprocessing

from .validators import BaseValidator, ValidationResult
from .epub3_validator import EPUB3Validator
from .pdf_validator import PDFValidator
from .docx_validator import DOCXValidator
from .wcag_validator import WCAGValidator

logger = logging.getLogger(__name__)


class JobStatus(Enum):
    """Status of batch processing jobs."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


class JobPriority(Enum):
    """Priority levels for batch jobs."""

    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class BatchJob:
    """Represents a batch processing job."""

    job_id: str
    files: List[Path]
    validators: List[str]
    priority: JobPriority = JobPriority.MEDIUM
    max_workers: int = 4
    status: JobStatus = JobStatus.PENDING
    created_at: datetime = field(default_factory=datetime.now)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    results: Dict[str, ValidationResult] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)
    progress: float = 0.0
    total_files: int = 0
    processed_files: int = 0
    failed_files: int = 0

    def __post_init__(self):
        """Initialize computed fields."""
        self.total_files = len(self.files)


@dataclass
class BatchResult:
    """Results of batch processing operation."""

    job_id: str
    total_files: int
    successful_validations: int
    failed_validations: int
    total_errors: int
    total_warnings: int
    processing_time: timedelta
    results: Dict[str, ValidationResult]
    summary: Dict[str, Any]
    performance_metrics: Dict[str, Any]


class BatchProcessor:
    """High-performance batch processing system for publications."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.max_concurrent_jobs = self.config.get("max_concurrent_jobs", 3)
        self.default_max_workers = self.config.get(
            "default_max_workers", multiprocessing.cpu_count()
        )
        self.queue_timeout = self.config.get("queue_timeout", 30.0)

        self._validators = {
            "epub3": EPUB3Validator,
            "pdf": PDFValidator,
            "docx": DOCXValidator,
            "wcag": WCAGValidator,
        }

        self._job_queue = Queue()
        self._active_jobs = {}
        self._completed_jobs = {}
        self._worker_threads = []
        self._shutdown_event = threading.Event()

        # Start worker threads
        self._start_workers()

    def _start_workers(self):
        """Start worker threads for processing jobs."""
        for i in range(self.max_concurrent_jobs):
            worker = threading.Thread(
                target=self._worker_loop, name=f"BatchProcessor-{i + 1}", daemon=True
            )
            worker.start()
            self._worker_threads.append(worker)
        logger.info(f"Started {self.max_concurrent_jobs} batch processor workers")

    def _worker_loop(self):
        """Main worker loop for processing jobs."""
        while not self._shutdown_event.is_set():
            try:
                # Get job from queue with timeout
                job = self._job_queue.get(timeout=self.queue_timeout)
                if job is None:
                    break

                self._process_job(job)

            except Exception as e:
                logger.error(f"Worker thread error: {e}")
                continue

    def submit_job(
        self,
        files: Union[List[Path], str],
        validators: Union[List[str], str],
        priority: JobPriority = JobPriority.MEDIUM,
        max_workers: Optional[int] = None,
    ) -> str:
        """Submit a batch processing job."""

        # Normalize inputs
        if isinstance(files, str):
            if files.endswith(".json"):
                # Load file list from JSON
                file_list = self._load_file_list(files)
            else:
                # Single file
                file_list = [Path(files)]
        else:
            file_list = [Path(f) for f in files]

        if isinstance(validators, str):
            validator_list = [validators]
        else:
            validator_list = validators

        # Validate inputs
        self._validate_inputs(file_list, validator_list)

        # Create job
        job_id = (
            f"batch_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{len(self._active_jobs)}"
        )

        job = BatchJob(
            job_id=job_id,
            files=file_list,
            validators=validator_list,
            priority=priority,
            max_workers=max_workers or self.default_max_workers,
        )

        # Add to active jobs
        self._active_jobs[job_id] = job

        # Add to queue
        self._job_queue.put(job)

        logger.info(f"Submitted batch job {job_id} with {len(file_list)} files")
        return job_id

    def _validate_inputs(self, files: List[Path], validators: List[str]):
        """Validate job inputs."""
        if not files:
            raise ValueError("No files provided for batch processing")

        if not validators:
            raise ValueError("No validators specified for batch processing")

        # Check validator names
        invalid_validators = set(validators) - set(self._validators.keys())
        if invalid_validators:
            raise ValueError(f"Unknown validators: {invalid_validators}")

        # Check files exist
        missing_files = []
        for file_path in files:
            if not file_path.exists():
                missing_files.append(str(file_path))

        if missing_files:
            raise ValueError(f"Files not found: {missing_files}")

    def _load_file_list(self, json_file: str) -> List[Path]:
        """Load file list from JSON configuration."""
        try:
            with open(json_file, "r") as f:
                config = json.load(f)

            files = []
            for file_entry in config.get("files", []):
                if isinstance(file_entry, str):
                    files.append(Path(file_entry))
                elif isinstance(file_entry, dict):
                    path = file_entry.get("path")
                    if path:
                        files.append(Path(path))

            return files

        except Exception as e:
            raise ValueError(f"Failed to load file list from {json_file}: {e}")

    def _process_job(self, job: BatchJob):
        """Process a single batch job."""
        try:
            job.status = JobStatus.RUNNING
            job.started_at = datetime.now()
            logger.info(f"Starting processing for job {job.job_id}")

            # Process files in parallel
            with concurrent.futures.ThreadPoolExecutor(
                max_workers=job.max_workers
            ) as executor:
                # Submit tasks for each file
                future_to_file = {}

                for file_path in job.files:
                    future = executor.submit(
                        self._process_single_file, file_path, job.validators
                    )
                    future_to_file[future] = file_path

                # Process results as they complete
                completed_count = 0
                for future in concurrent.futures.as_completed(future_to_file):
                    file_path = future_to_file[future]

                    try:
                        result = future.result()
                        job.results[str(file_path)] = result

                        if result.is_valid:
                            job.processed_files += 1
                        else:
                            job.failed_files += 1

                    except Exception as e:
                        error_msg = f"Error processing {file_path}: {e}"
                        job.errors.append(error_msg)
                        job.failed_files += 1
                        logger.error(error_msg)

                    completed_count += 1
                    job.progress = completed_count / len(job.files) * 100

            job.status = JobStatus.COMPLETED
            job.completed_at = datetime.now()

            # Move to completed jobs
            self._completed_jobs[job.job_id] = job
            del self._active_jobs[job.job_id]

            logger.info(
                f"Completed processing for job {job.job_id}: {job.processed_files}/{len(job.files)} files processed"
            )

        except Exception as e:
            job.status = JobStatus.FAILED
            job.completed_at = datetime.now()
            job.errors.append(f"Job processing failed: {e}")

            # Move to completed jobs
            self._completed_jobs[job.job_id] = job
            del self._active_jobs[job.job_id]

            logger.error(f"Job {job.job_id} failed: {e}")

    def _process_single_file(
        self, file_path: Path, validators: List[str]
    ) -> ValidationResult:
        """Process a single file with specified validators."""
        combined_errors = []
        combined_stats = {}
        is_valid = True

        for validator_name in validators:
            try:
                validator_class = self._validators[validator_name]
                validator = validator_class()

                result = validator.validate(file_path)

                if not result.is_valid:
                    is_valid = False

                combined_errors.extend(result.errors)
                combined_stats[f"{validator_name}_stats"] = result.stats

            except Exception as e:
                is_valid = False
                logger.error(f"Validator {validator_name} failed for {file_path}: {e}")

        return ValidationResult(
            is_valid=is_valid,
            errors=combined_errors,
            stats=combined_stats,
            file_path=str(file_path),
            timestamp=datetime.now(),
        )

    def get_job_status(self, job_id: str) -> Optional[BatchJob]:
        """Get status of a specific job."""
        if job_id in self._active_jobs:
            return self._active_jobs[job_id]
        elif job_id in self._completed_jobs:
            return self._completed_jobs[job_id]
        return None

    def cancel_job(self, job_id: str) -> bool:
        """Cancel a pending or running job."""
        if job_id in self._active_jobs:
            job = self._active_jobs[job_id]
            job.status = JobStatus.CANCELLED
            job.completed_at = datetime.now()

            # Move to completed jobs
            self._completed_jobs[job_id] = job
            del self._active_jobs[job_id]

            logger.info(f"Cancelled job {job_id}")
            return True
        return False

    def get_job_results(self, job_id: str) -> Optional[BatchResult]:
        """Get detailed results for a completed job."""
        job = self._completed_jobs.get(job_id)
        if not job or job.status != JobStatus.COMPLETED:
            return None

        # Calculate statistics
        total_errors = 0
        total_warnings = 0

        for result in job.results.values():
            errors_by_severity = {"error": [], "warning": [], "info": []}
            for error in result.errors:
                errors_by_severity[error.severity].append(error)

            total_errors += len(errors_by_severity["error"])
            total_warnings += len(errors_by_severity["warning"])

        processing_time = job.completed_at - job.started_at

        # Create batch result
        batch_result = BatchResult(
            job_id=job.job_id,
            total_files=job.total_files,
            successful_validations=job.processed_files,
            failed_validations=job.failed_files,
            total_errors=total_errors,
            total_warnings=total_warnings,
            processing_time=processing_time,
            results=job.results,
            summary=self._generate_summary(job),
            performance_metrics=self._calculate_performance_metrics(
                job, processing_time
            ),
        )

        return batch_result

    def _generate_summary(self, job: BatchJob) -> Dict[str, Any]:
        """Generate summary of job results."""
        summary = {
            "job_id": job.job_id,
            "status": job.status.value,
            "total_files": job.total_files,
            "processed_files": job.processed_files,
            "failed_files": job.failed_files,
            "success_rate": (job.processed_files / job.total_files * 100)
            if job.total_files > 0
            else 0,
            "validators_used": job.validators,
            "processing_time": str(job.completed_at - job.started_at)
            if job.completed_at
            else "N/A",
        }

        # Validator-specific statistics
        validator_stats = {}
        for validator_name in job.validators:
            validator_stats[validator_name] = {
                "files_processed": 0,
                "errors_found": 0,
                "warnings_found": 0,
            }

        for file_path, result in job.results.items():
            for validator_name in job.validators:
                validator_stats[validator_name]["files_processed"] += 1

                # Count errors and warnings for this validator
                validator_key = f"{validator_name}_stats"
                if validator_key in result.stats:
                    validator_stats[validator_name]["errors_found"] += result.stats.get(
                        "error_count", 0
                    )
                    validator_stats[validator_name]["warnings_found"] += (
                        result.stats.get("warning_count", 0)
                    )

        summary["validator_statistics"] = validator_stats
        return summary

    def _calculate_performance_metrics(
        self, job: BatchJob, processing_time: timedelta
    ) -> Dict[str, Any]:
        """Calculate performance metrics for the job."""
        processing_seconds = processing_time.total_seconds()

        metrics = {
            "total_processing_time": processing_seconds,
            "files_per_second": job.total_files / processing_seconds
            if processing_seconds > 0
            else 0,
            "average_time_per_file": processing_seconds / job.total_files
            if job.total_files > 0
            else 0,
            "workers_used": job.max_workers,
            "throughput": job.total_files / job.max_workers / processing_seconds
            if processing_seconds > 0
            else 0,
            "success_rate": (job.processed_files / job.total_files * 100)
            if job.total_files > 0
            else 0,
        }

        # Efficiency metrics
        if job.max_workers > 1:
            theoretical_time = processing_seconds * job.max_workers
            metrics["parallel_efficiency"] = (
                (theoretical_time / processing_seconds) if processing_seconds > 0 else 0
            )

        return metrics

    def list_active_jobs(self) -> List[BatchJob]:
        """List all active jobs."""
        return list(self._active_jobs.values())

    def list_completed_jobs(self, limit: int = 50) -> List[BatchJob]:
        """List completed jobs."""
        jobs = list(self._completed_jobs.values())
        # Sort by completion time (most recent first)
        jobs.sort(key=lambda j: j.completed_at or datetime.min, reverse=True)
        return jobs[:limit]

    def generate_report(self, job_id: str, output_file: Optional[Path] = None) -> str:
        """Generate detailed report for a completed job."""
        batch_result = self.get_job_results(job_id)
        if not batch_result:
            raise ValueError(f"Job {job_id} not found or not completed")

        # Generate report
        report = {
            "job_summary": batch_result.summary,
            "performance_metrics": batch_result.performance_metrics,
            "detailed_results": {},
            "errors_by_type": {},
            "recommendations": [],
        }

        # Process detailed results
        for file_path, result in batch_result.results.items():
            report["detailed_results"][file_path] = {
                "is_valid": result.is_valid,
                "error_count": len([e for e in result.errors if e.severity == "error"]),
                "warning_count": len(
                    [e for e in result.errors if e.severity == "warning"]
                ),
                "info_count": len([e for e in result.errors if e.severity == "info"]),
                "errors": [
                    {
                        "severity": e.severity,
                        "message": e.message,
                        "rule_id": e.rule_id,
                        "file_path": e.file_path,
                    }
                    for e in result.errors
                ],
                "stats": result.stats,
            }

        # Generate recommendations
        if batch_result.total_errors > 0:
            report["recommendations"].append(
                "Address critical errors before publication"
            )

        if batch_result.total_warnings > 10:
            report["recommendations"].append(
                "High number of warnings detected - review quality standards"
            )

        if batch_result.performance_metrics["parallel_efficiency"] < 0.5:
            report["recommendations"].append(
                "Consider optimizing parallel processing for better performance"
            )

        report_str = json.dumps(report, indent=2, default=str)

        # Save to file if specified
        if output_file:
            with open(output_file, "w") as f:
                f.write(report_str)

        return report_str

    def cleanup_completed_jobs(self, older_than_hours: int = 24):
        """Clean up old completed jobs."""
        cutoff_time = datetime.now() - timedelta(hours=older_than_hours)

        jobs_to_remove = []
        for job_id, job in self._completed_jobs.items():
            if job.completed_at and job.completed_at < cutoff_time:
                jobs_to_remove.append(job_id)

        for job_id in jobs_to_remove:
            del self._completed_jobs[job_id]

        logger.info(
            f"Cleaned up {len(jobs_to_remove)} completed jobs older than {older_than_hours} hours"
        )

    def shutdown(self, wait_for_completion: bool = True):
        """Shutdown the batch processor."""
        logger.info("Shutting down batch processor...")

        if wait_for_completion:
            # Wait for active jobs to complete
            while self._active_jobs:
                time.sleep(1)

        # Signal shutdown
        self._shutdown_event.set()

        # Add None to queue to unblock workers
        for _ in range(len(self._worker_threads)):
            self._job_queue.put(None)

        # Wait for workers to finish
        for worker in self._worker_threads:
            worker.join(timeout=5)

        logger.info("Batch processor shutdown complete")
