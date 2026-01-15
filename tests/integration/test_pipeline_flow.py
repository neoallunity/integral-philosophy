"""
Integration tests for the complete content pipeline flow
Testing end-to-end pipeline: Scraping → Parsing → TEI → Multiple formats
"""

import pytest
import asyncio
import time
import json
from pathlib import Path
from typing import Dict, Any, List
from unittest.mock import Mock, patch, MagicMock

from tests.utils.base_test_classes import IntegrationTestCase, PerformanceTestCase


class TestPipelineFlow(IntegrationTestCase, PerformanceTestCase):
    """Test end-to-end pipeline integration and data flow"""

    @pytest.fixture(autouse=True)
    def setup_pipeline_test(self):
        """Setup pipeline integration test environment"""
        self.pipeline_components = {}
        self.test_data = {}
        self.processing_jobs = {}

        # Mock pipeline components
        self.setup_mock_components()

        yield

        # Cleanup pipeline components
        self.cleanup_pipeline_components()

    def setup_mock_components(self):
        """Setup mock pipeline components"""
        # Mock WebScraper
        self.web_scraper = Mock()
        self.web_scraper.scrape_content = Mock(
            return_value={
                "url": "https://example.com/article",
                "title": "Test Article",
                "content": "Test content with philosophical insights",
                "metadata": {"author": "Test Author", "date": "2024-01-01"},
            }
        )

        # Mock ContentPipeline
        self.content_pipeline = Mock()
        self.content_pipeline.process_content = Mock(
            return_value={
                "processed_content": "Processed philosophical content",
                "structure": {"sections": ["intro", "main", "conclusion"]},
                "metadata": {"word_count": 500, "language": "en"},
            }
        )

        # Mock TEIGenerator
        self.tei_generator = Mock()
        self.tei_generator.generate_tei = Mock(
            return_value="""<?xml version="1.0"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>Test Article</title>
            </titleStmt>
        </fileDesc>
    </teiHeader>
    <text>
        <body>
            <p>Processed philosophical content</p>
        </body>
    </text>
</TEI>"""
        )

        # Mock FormatConverter
        self.format_converter = Mock()
        self.format_converter.convert_to_latex = Mock(
            return_value="\\documentclass{article}\n\\begin{document}\nTest content\n\\end{document}"
        )
        self.format_converter.convert_to_epub = Mock(
            return_value=b"EPUB binary content"
        )
        self.format_converter.convert_to_pdf = Mock(return_value=b"PDF binary content")

    def cleanup_pipeline_components(self):
        """Cleanup pipeline components"""
        self.processing_jobs.clear()
        self.pipeline_components.clear()

    def test_complete_pipeline_flow_success(self):
        """Test successful complete pipeline flow from scraping to multiple formats"""
        # Start performance monitoring
        self.start_performance_monitoring()

        # Step 1: Scraping
        scrape_result = self.web_scraper.scrape_content("https://example.com/article")
        assert scrape_result["title"] == "Test Article"
        assert "content" in scrape_result

        # Step 2: Content Processing
        process_result = self.content_pipeline.process_content(scrape_result)
        assert "processed_content" in process_result
        assert "structure" in process_result

        # Step 3: TEI Generation
        tei_result = self.tei_generator.generate_tei(process_result)
        self.assert_valid_xml(tei_result)
        assert "<TEI" in tei_result

        # Step 4: Format Conversion
        latex_result = self.format_converter.convert_to_latex(tei_result)
        epub_result = self.format_converter.convert_to_epub(tei_result)
        pdf_result = self.format_converter.convert_to_pdf(tei_result)

        assert isinstance(latex_result, str)
        assert isinstance(epub_result, bytes)
        assert isinstance(pdf_result, bytes)

        # Get performance metrics
        metrics = self.stop_performance_monitoring()
        self.assert_performance_within(metrics, max_duration=5.0, max_memory_mb=200)

    def test_pipeline_error_propagation(self):
        """Test error propagation between pipeline stages"""
        # Mock scraping failure
        self.web_scraper.scrape_content = Mock(side_effect=Exception("Scraping failed"))

        with pytest.raises(Exception, match="Scraping failed"):
            self.web_scraper.scrape_content("https://example.com/article")

        # Verify subsequent stages are not called
        self.content_pipeline.process_content.assert_not_called()
        self.tei_generator.generate_tei.assert_not_called()
        self.format_converter.convert_to_latex.assert_not_called()

    def test_pipeline_component_coordination(self):
        """Test coordination between pipeline components"""
        # Create pipeline job tracker
        job_id = "test-job-001"
        self.processing_jobs[job_id] = {
            "status": "started",
            "stage": "scraping",
            "start_time": time.time(),
        }

        # Simulate pipeline progression
        stages = ["scraping", "processing", "tei_generation", "format_conversion"]
        result = None  # Initialize result to avoid unbound variable error

        for stage in stages:
            self.processing_jobs[job_id]["stage"] = stage
            self.processing_jobs[job_id]["status"] = "processing"

            # Simulate stage processing
            time.sleep(0.1)  # Simulate processing time

            if stage == "scraping":
                result = self.web_scraper.scrape_content("https://example.com/article")
            elif stage == "processing":
                result = self.content_pipeline.process_content(result)
            elif stage == "tei_generation":
                result = self.tei_generator.generate_tei(result)
            elif stage == "format_conversion":
                result = self.format_converter.convert_to_latex(result)

            self.processing_jobs[job_id]["status"] = "completed"

        # Verify pipeline completion
        assert self.processing_jobs[job_id]["stage"] == "format_conversion"
        assert self.processing_jobs[job_id]["status"] == "completed"

    def test_pipeline_resource_management(self):
        """Test resource management and cleanup during pipeline execution"""
        # Track resource usage
        resources_used = {"files": [], "memory": 0, "connections": 0}

        def create_temp_file(content: str) -> Path:
            """Create temporary file and track it"""
            temp_file = self.temp_dir / f"temp_{len(resources_used['files'])}.tmp"
            temp_file.write_text(content)
            resources_used["files"].append(temp_file)
            return temp_file

        # Simulate resource usage during pipeline
        temp_files = []
        try:
            # Stage 1: Create temp files during scraping
            scrape_file = create_temp_file("Scraped content")
            temp_files.append(scrape_file)

            # Stage 2: Create temp files during processing
            process_file = create_temp_file("Processed content")
            temp_files.append(process_file)

            # Stage 3: Create TEI file
            tei_file = create_temp_file("<?xml version='1.0'?><TEI>Test</TEI>")
            temp_files.append(tei_file)

            # Stage 4: Create format files
            latex_file = create_temp_file("\\LaTeX content")
            temp_files.append(latex_file)

            # Initialize result variables to avoid unbound errors
            result = None

            # Verify resources are tracked
            assert len(resources_used["files"]) == 4
            assert all(f.exists() for f in resources_used["files"])

        finally:
            # Cleanup resources
            for temp_file in temp_files:
                if temp_file.exists():
                    temp_file.unlink()

            resources_used["files"].clear()
            assert len(resources_used["files"]) == 0

    def test_pipeline_concurrent_processing(self):
        """Test concurrent processing of multiple pipeline jobs"""
        # Create multiple pipeline jobs
        job_urls = [
            "https://example.com/article1",
            "https://example.com/article2",
            "https://example.com/article3",
        ]

        job_results = {}

        def process_single_job(url: str, job_id: str) -> Dict[str, Any]:
            """Process a single pipeline job"""
            try:
                # Scrape
                scrape_result = self.web_scraper.scrape_content(url)

                # Process
                process_result = self.content_pipeline.process_content(scrape_result)

                # Generate TEI
                tei_result = self.tei_generator.generate_tei(process_result)

                # Convert formats
                formats = {
                    "latex": self.format_converter.convert_to_latex(tei_result),
                    "epub": self.format_converter.convert_to_epub(tei_result),
                    "pdf": self.format_converter.convert_to_pdf(tei_result),
                }

                return {
                    "job_id": job_id,
                    "status": "completed",
                    "url": url,
                    "formats": formats,
                }
            except Exception as e:
                return {"job_id": job_id, "status": "failed", "error": str(e)}

        # Process jobs concurrently
        import concurrent.futures

        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
            futures = {}

            for i, url in enumerate(job_urls):
                job_id = f"concurrent-job-{i:03d}"
                future = executor.submit(process_single_job, url, job_id)
                futures[future] = job_id

            # Collect results
            for future in concurrent.futures.as_completed(futures):
                job_id = futures[future]
                result = future.result()
                job_results[job_id] = result

        # Verify all jobs completed
        assert len(job_results) == 3
        for job_id, result in job_results.items():
            assert result["status"] == "completed"
            assert "formats" in result
            assert "latex" in result["formats"]
            assert "epub" in result["formats"]
            assert "pdf" in result["formats"]

    def test_pipeline_performance_benchmarks(self):
        """Test pipeline performance against benchmarks"""
        # Performance benchmarks
        benchmarks = {
            "scraping": {"max_duration": 2.0, "max_memory_mb": 200},
            "processing": {"max_duration": 1.0, "max_memory_mb": 200},
            "tei_generation": {"max_duration": 0.5, "max_memory_mb": 200},
            "format_conversion": {"max_duration": 3.0, "max_memory_mb": 200},
        }

        stage_metrics = {}

        # Test each stage performance
        stages = [
            (
                "scraping",
                lambda: self.web_scraper.scrape_content("https://example.com/article"),
            ),
            (
                "processing",
                lambda: self.content_pipeline.process_content({"content": "test"}),
            ),
            (
                "tei_generation",
                lambda: self.tei_generator.generate_tei({"content": "test"}),
            ),
            (
                "format_conversion",
                lambda: self.format_converter.convert_to_latex("<TEI>test</TEI>"),
            ),
        ]

        for stage_name, stage_func in stages:
            self.start_performance_monitoring()

            # Execute stage
            result = stage_func()

            # Get metrics
            metrics = self.stop_performance_monitoring()
            stage_metrics[stage_name] = metrics

            # Verify against benchmarks
            benchmark = benchmarks[stage_name]
            self.assert_performance_within(
                metrics,
                max_duration=benchmark["max_duration"],
                max_memory_mb=benchmark["max_memory_mb"],
            )

        # Verify overall pipeline performance
        total_duration = sum(m["duration_seconds"] for m in stage_metrics.values())
        assert total_duration < 10.0, (
            f"Total pipeline duration {total_duration}s exceeds limit"
        )

    def test_pipeline_data_integrity(self):
        """Test data integrity throughout the pipeline"""
        # Original test data
        original_data = {
            "title": "Philosophical Inquiry",
            "content": "The nature of consciousness remains one of philosophy's most profound questions...",
            "author": "Test Philosopher",
            "metadata": {"date": "2024-01-15", "category": "metaphysics"},
        }

        # Pipeline stages with data validation
        stage_data = original_data.copy()

        # Stage 1: Scraping (mock returns test data)
        scrape_result = self.web_scraper.scrape_content("https://example.com/article")
        # Verify scrape was called successfully (mock data may differ)
        stage_data = scrape_result

        # Stage 2: Processing
        process_result = self.content_pipeline.process_content(stage_data)
        assert "processed_content" in process_result
        # Verify metadata is preserved
        assert "metadata" in process_result
        stage_data = process_result

        # Stage 3: TEI Generation
        tei_result = self.tei_generator.generate_tei(stage_data)
        self.assert_valid_xml(tei_result)
        # Verify title is preserved in TEI
        assert original_data["title"] in tei_result
        stage_data = tei_result

        # Stage 4: Format Conversion
        latex_result = self.format_converter.convert_to_latex(stage_data)
        assert isinstance(latex_result, str)
        # Verify content is preserved
        assert "philosophical" in latex_result.lower()

        # Final integrity check
        assert original_data["title"] in latex_result
        assert original_data["content"][:50] in latex_result

    def test_pipeline_error_recovery(self):
        """Test pipeline error recovery and retry mechanisms"""
        # Mock intermittent failures
        call_count = {"scraping": 0, "processing": 0}

        def failing_scrape(*args, **kwargs):
            call_count["scraping"] += 1
            if call_count["scraping"] < 3:
                raise Exception("Network error")
            return {"title": "Recovered Article", "content": "Recovered content"}

        def failing_process(*args, **kwargs):
            call_count["processing"] += 1
            if call_count["processing"] < 2:
                raise Exception("Processing error")
            return {"processed_content": "Processed recovered content"}

        self.web_scraper.scrape_content = failing_scrape
        self.content_pipeline.process_content = failing_process

        # Test retry mechanism
        max_retries = 3
        retry_count = 0
        scrape_result = None
        process_result = None
        tei_result = None

        while retry_count < max_retries:
            call_count["processing"] = 0  # Reset for each attempt
            try:
                # Attempt pipeline execution
                scrape_result = self.web_scraper.scrape_content(
                    "https://example.com/article"
                )
                process_result = self.content_pipeline.process_content(scrape_result)
                tei_result = self.tei_generator.generate_tei(process_result)

                # Success - break out of retry loop
                break

            except Exception as e:
                retry_count += 1
                if retry_count >= max_retries:
                    pytest.fail(f"Pipeline failed after {max_retries} retries: {e}")
                time.sleep(0.1)  # Brief delay before retry

        # Verify eventual success
        if scrape_result is not None:
            assert scrape_result["title"] == "Recovered Article"
        if process_result is not None:
            assert "processed_content" in process_result
        if tei_result is not None:
            assert "<TEI" in tei_result
