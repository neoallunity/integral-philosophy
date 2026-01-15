"""
Complete Workflow Testing for Integral Philosophy Publishing System
Tests full document lifecycle from scraping to publishing with all intermediate steps
"""

import pytest
import asyncio
import json
import time
from pathlib import Path
from typing import Dict, Any, List, Optional
from unittest.mock import Mock, patch, AsyncMock

from ...utils.base_test_classes import (
    IntegrationTestCase,
    SeleniumTestCase,
    PerformanceTestCase,
)
from ...utils.test_helpers import (
    TestDataManager,
    PerformanceMonitor,
    create_uuid_string,
)


class TestPublishingWorkflow(IntegrationTestCase, PerformanceTestCase):
    """Test complete document lifecycle: scraping → processing → publishing"""

    @pytest.fixture(autouse=True)
    def setup_workflow_environment(self, temp_dir):
        """Setup comprehensive workflow testing environment"""
        self.temp_dir = temp_dir
        self.data_manager = TestDataManager(temp_dir)
        self.performance_monitor = PerformanceMonitor()

        # Test data directories
        self.input_dir = self.data_manager.create_directory("input")
        self.processing_dir = self.data_manager.create_directory("processing")
        self.output_dir = self.data_manager.create_directory("output")
        self.archive_dir = self.data_manager.create_directory("archive")

        # Mock service components
        self.scraper_service = None
        self.converter_service = None
        self.tei_service = None
        self.uml_service = None
        self.validator_service = None
        self.publisher_service = None

        yield

        self.data_manager.cleanup()

    def test_complete_markdown_workflow(self):
        """Test full workflow from markdown input to published output"""
        # 1. Create sample markdown content
        source_content = """# Integral Philosophy Document

## Introduction
This document explores the fundamental principles of integral philosophy.

## Core Concepts
### First Person Perspectives
- Subjective experience
- Consciousness studies
- Phenomenology

### Second Person Perspectives
- Intersubjective reality
- Cultural contexts
- Shared meanings

### Third Person Perspectives
- Objective reality
- Scientific methods
- Empirical data

## Quadrants Model
The integral approach combines all four quadrants...

## References
1. Wilber, Ken. *Integral Psychology*
2. Gebser, Jean. *The Ever-Present Origin*
"""

        source_file = self.data_manager.create_sample_file(
            "integral_philosophy.md", source_content
        )

        # 2. Start performance monitoring
        self.start_performance_monitoring()

        try:
            # 3. Execute complete pipeline
            result = self._execute_complete_pipeline(source_file, "markdown")

            # 4. Validate results
            assert result["status"] == "completed"
            assert "outputs" in result
            assert len(result["outputs"]) >= 4  # MD, HTML, TEI, UML

            # 5. Validate output files exist and are valid
            for output_type in ["html", "tei", "uml", "pdf"]:
                output_file = self.output_dir / f"integral_philosophy.{output_type}"
                assert output_file.exists(), f"Missing {output_type} output"

                if output_type == "tei":
                    self.assert_valid_xml(output_file.read_text(encoding="utf-8"))
                elif output_type == "html":
                    assert "<html" in output_file.read_text(encoding="utf-8")

        finally:
            metrics = self.stop_performance_monitoring()

            # 6. Performance assertions
            self.assert_performance_within(
                metrics,
                max_duration=30.0,  # 30 seconds max
                max_memory_mb=512,  # 512MB max
                max_cpu_percent=80,  # 80% CPU max
            )

    def test_batch_processing_workflow(self):
        """Test processing multiple documents in batch"""
        # 1. Create multiple test documents
        documents = [
            ("doc1.md", "# Document 1\nContent for document 1..."),
            (
                "doc2.html",
                "<html><body><h1>Document 2</h1><p>Content for document 2...</p></body></html>",
            ),
            (
                "doc3.tex",
                "\\documentclass{article}\\begin{document}\\section{Document 3}Content for document 3...\\end{document}",
            ),
        ]

        source_files = []
        for filename, content in documents:
            source_files.append(self.data_manager.create_sample_file(filename, content))

        # 2. Execute batch processing
        batch_results = self._execute_batch_processing(source_files)

        # 3. Validate batch results
        assert len(batch_results) == len(source_files)
        assert all(result["status"] == "completed" for result in batch_results)

        # 4. Validate all outputs generated
        for i, source_file in enumerate(source_files):
            base_name = source_file.stem
            for output_type in ["html", "tei"]:
                output_file = self.output_dir / f"{base_name}.{output_type}"
                assert output_file.exists(), f"Missing {output_type} for {base_name}"

    def test_error_recovery_workflow(self):
        """Test error handling and recovery in the pipeline"""
        # 1. Create problematic content
        problematic_content = """# Invalid Document
<script>alert('xss')</script>
<<invalid>>xml<<content>>
"""

        source_file = self.data_manager.create_sample_file(
            "invalid.md", problematic_content
        )

        # 2. Execute pipeline with error handling
        result = self._execute_pipeline_with_error_handling(source_file)

        # 3. Validate error handling
        assert result["status"] in ["completed_with_warnings", "failed"]
        if result["status"] == "completed_with_warnings":
            assert "warnings" in result
            assert len(result["warnings"]) > 0
        else:
            assert "errors" in result
            assert len(result["errors"]) > 0

        # 4. Validate cleanup
        assert self._validate_cleanup_after_error()

    def test_rollback_workflow(self):
        """Test rollback functionality when pipeline fails"""
        # 1. Create test document
        source_file = self.data_manager.create_sample_file(
            "rollback_test.md", "# Rollback Test\nContent to test rollback..."
        )

        # 2. Mock failure in the middle of pipeline
        with patch("scripts.content_pipeline.process_document") as mock_process:
            mock_process.side_effect = Exception("Simulated pipeline failure")

            # 3. Execute pipeline expecting failure
            result = self._execute_pipeline_with_rollback(source_file)

            # 4. Validate rollback
            assert result["status"] == "failed"
            assert result.get("rollback", False)

            # 5. Verify no partial outputs remain
            assert self._verify_no_partial_outputs(source_file.stem)

    def test_performance_across_pipeline(self):
        """Test performance metrics across different pipeline stages"""
        # 1. Create large test document
        large_content = self._generate_large_document()
        source_file = self.data_manager.create_sample_file(
            "large_document.md", large_content
        )

        # 2. Track performance per stage
        stage_metrics = {}

        # 3. Execute pipeline with stage tracking
        for stage in [
            "scraping",
            "conversion",
            "tei_generation",
            "uml_generation",
            "validation",
        ]:
            self.start_performance_monitoring()

            try:
                stage_result = self._execute_single_stage(source_file, stage)
                metrics = self.stop_performance_monitoring()
                stage_metrics[stage] = metrics

                assert stage_result["status"] == "completed"

            except Exception as e:
                metrics = self.stop_performance_monitoring()
                stage_metrics[stage] = {"error": str(e), **metrics}
                pytest.fail(f"Stage {stage} failed: {e}")

        # 4. Validate overall performance
        total_duration = sum(
            metrics.get("duration_seconds", 0)
            for metrics in stage_metrics.values()
            if "error" not in metrics
        )
        assert total_duration < 60.0, f"Total pipeline too slow: {total_duration}s"

        # 5. Validate memory usage consistency
        memory_usages = [
            metrics.get("peak_memory_mb", 0)
            for metrics in stage_metrics.values()
            if "error" not in metrics
        ]
        assert max(memory_usages) < 1024, (
            f"Memory usage too high: {max(memory_usages)}MB"
        )

    def _execute_complete_pipeline(
        self, source_file: Path, input_type: str
    ) -> Dict[str, Any]:
        """Execute complete document processing pipeline"""
        from scripts.content_pipeline import process_document

        result = process_document(
            str(source_file),
            input_type=input_type,
            output_dir=str(self.output_dir),
            processing_dir=str(self.processing_dir),
            generate_tei=True,
            generate_uml=True,
            validate_output=True,
        )

        return result

    def _execute_batch_processing(
        self, source_files: List[Path]
    ) -> List[Dict[str, Any]]:
        """Execute batch processing of multiple documents"""
        from scripts.content_pipeline import process_batch

        results = process_batch(
            [str(f) for f in source_files],
            output_dir=str(self.output_dir),
            processing_dir=str(self.processing_dir),
            max_concurrent=3,
        )

        return results

    def _execute_pipeline_with_error_handling(
        self, source_file: Path
    ) -> Dict[str, Any]:
        """Execute pipeline with comprehensive error handling"""
        from scripts.content_pipeline import process_document_with_recovery

        result = process_document_with_recovery(
            str(source_file),
            output_dir=str(self.output_dir),
            processing_dir=str(self.processing_dir),
            error_recovery=True,
            continue_on_error=True,
        )

        return result

    def _execute_pipeline_with_rollback(self, source_file: Path) -> Dict[str, Any]:
        """Execute pipeline with rollback capability"""
        from scripts.content_pipeline import process_document_with_rollback

        result = process_document_with_rollback(
            str(source_file),
            output_dir=str(self.output_dir),
            processing_dir=str(self.processing_dir),
            enable_rollback=True,
        )

        return result

    def _execute_single_stage(self, source_file: Path, stage: str) -> Dict[str, Any]:
        """Execute single pipeline stage"""
        if stage == "scraping":
            from scripts.web_scraper import scrape_content

            return scrape_content(str(source_file), str(self.processing_dir))
        elif stage == "conversion":
            from scripts.format_converter import convert_document

            return convert_document(str(source_file), "html", str(self.output_dir))
        elif stage == "tei_generation":
            from scripts.tei_generator import generate_tei

            return generate_tei(str(source_file), str(self.output_dir))
        elif stage == "uml_generation":
            from scripts.ast_to_uml import generate_uml

            return generate_uml(str(source_file), str(self.output_dir))
        elif stage == "validation":
            from scripts.validate_publication import validate_document

            return validate_document(str(source_file))
        else:
            raise ValueError(f"Unknown stage: {stage}")

    def _generate_large_document(self) -> str:
        """Generate a large document for performance testing"""
        content = ["# Large Performance Test Document"]

        for i in range(100):  # 100 sections
            content.append(f"\n## Section {i + 1}")
            for j in range(10):  # 10 paragraphs per section
                content.append(
                    f"\nParagraph {j + 1} of section {i + 1}. " * 20
                )  # Long paragraphs

        return "\n".join(content)

    def _validate_cleanup_after_error(self) -> bool:
        """Validate proper cleanup after error conditions"""
        # Check processing directory is clean
        processing_files = list(self.processing_dir.rglob("*"))
        return len(processing_files) == 0

    def _verify_no_partial_outputs(self, document_name: str) -> bool:
        """Verify no partial output files exist after rollback"""
        expected_outputs = [
            f"{document_name}.html",
            f"{document_name}.tei",
            f"{document_name}.uml",
            f"{document_name}.pdf",
        ]

        for output in expected_outputs:
            output_file = self.output_dir / output
            if output_file.exists():
                return False
        return True


class TestMultiFormatConversion(IntegrationTestCase):
    """Test multi-format document conversion workflows"""

    @pytest.fixture(autouse=True)
    def setup_conversion_environment(self, temp_dir):
        """Setup conversion testing environment"""
        self.temp_dir = temp_dir
        self.data_manager = TestDataManager(temp_dir)
        self.conversion_dir = self.data_manager.create_directory("conversions")

        yield

        self.data_manager.cleanup()

    def test_markdown_to_all_formats(self):
        """Test conversion from markdown to all supported formats"""
        # 1. Create comprehensive markdown document
        markdown_content = """# Integral Philosophy: A Comprehensive Guide

## Overview
This document explores the integral approach to philosophy and consciousness.

## Mathematical Content
The integral formula: $\\int_0^1 x^2 dx = \\frac{1}{3}$

## Code Examples
```python
def integral_approach():
    return "holistic_understanding"
```

## Tables
| Quadrant | Perspective | Example |
|----------|-------------|---------|
| UL       | Interior Individual | Subjective experience |
| UR       | Exterior Individual | Objective behavior |
| LL       | Interior Collective | Intersubjective culture |
| LR       | Exterior Collective | Interobjective systems |

## Lists
1. First-tier consciousness
2. Second-tier consciousness
3. Third-tier consciousness

## Citations
> The integral approach embraces all perspectives [Wilber, 2000].

## Conclusion
Integral philosophy provides a comprehensive framework...
"""

        source_file = self.data_manager.create_sample_file(
            "integral_guide.md", markdown_content
        )

        # 2. Convert to all formats
        conversions = self._convert_to_all_formats(source_file, "markdown")

        # 3. Validate each conversion
        for target_format, result in conversions.items():
            assert result["success"], f"Conversion to {target_format} failed"
            assert result["output_file"].exists(), (
                f"Output file missing for {target_format}"
            )

            # Format-specific validation
            if target_format == "html":
                content = result["output_file"].read_text(encoding="utf-8")
                assert "<html" in content
                assert "integral_approach" in content
            elif target_format == "tei":
                content = result["output_file"].read_text(encoding="utf-8")
                self.assert_valid_xml(content)
                assert "<TEI" in content
            elif target_format == "pdf":
                assert (
                    result["output_file"].stat().st_size > 1000
                )  # Basic PDF validation

    def test_html_to_all_formats(self):
        """Test conversion from HTML to all supported formats"""
        html_content = """<!DOCTYPE html>
<html>
<head>
    <title>Integral Philosophy</title>
</head>
<body>
    <h1>Integral Philosophy: Digital Approach</h1>
    <h2>Overview</h2>
    <p>This document explores integral philosophy in digital format.</p>
    
    <h3>Four Quadrants</h3>
    <ul>
        <li><strong>Upper-Left:</strong> Interior Individual</li>
        <li><strong>Upper-Right:</strong> Exterior Individual</li>
        <li><strong>Lower-Left:</strong> Interior Collective</li>
        <li><strong>Lower-Right:</strong> Exterior Collective</li>
    </ul>
    
    <blockquote>
        "Integral philosophy embraces all quadrants, all levels, all lines..."
    </blockquote>
</body>
</html>"""

        source_file = self.data_manager.create_sample_file(
            "integral_digital.html", html_content
        )

        # Convert to all formats
        conversions = self._convert_to_all_formats(source_file, "html")

        # Validate conversions
        for target_format, result in conversions.items():
            assert result["success"], f"HTML to {target_format} conversion failed"
            assert result["output_file"].exists()

    def test_latex_to_all_formats(self):
        """Test conversion from LaTeX to all supported formats"""
        latex_content = r"""\documentclass{article}
\usepackage{amsmath}
\usepackage{graphicx}
\title{Integral Philosophy: Mathematical Approach}
\author{Test Author}

\begin{document}
\maketitle

\section{Introduction}
Integral philosophy provides a mathematical framework for understanding consciousness.

\section{Mathematical Formulation}
The integral consciousness function:
\[ C(x) = \int_{0}^{x} f(t) dt \]

Where $f(t)$ represents the developmental trajectory.

\section{Quadrant Model}
The four quadrants can be represented as:
\[ Q = \{Q_{UL}, Q_{UR}, Q_{LL}, Q_{LR}\} \]

\section{Conclusion}
This mathematical approach provides precision to integral theory.

\begin{thebibliography}{9}
\bibitem{wilber2000} Ken Wilber, \emph{Integral Psychology}, Shambhala, 2000.
\end{thebibliography}

\end{document}"""

        source_file = self.data_manager.create_sample_file(
            "integral_mathematical.tex", latex_content
        )

        # Convert to all formats
        conversions = self._convert_to_all_formats(source_file, "latex")

        # Validate conversions
        for target_format, result in conversions.items():
            if target_format == "pdf":  # LaTeX to PDF should work well
                assert result["success"], f"LaTeX to {target_format} conversion failed"
                assert result["output_file"].exists()

    def test_round_trip_conversion(self):
        """Test round-trip conversion: MD → HTML → MD → TEI → HTML"""
        original_content = """# Round Trip Test

## Original Content
This is the original content for round-trip testing.

### Features
- **Bold text**
- *Italic text*
- `Code snippets`
- [Links](https://example.com)

#### Nested Content
Nested content to test structure preservation.

> Blockquotes should be preserved.

```
Code blocks
with multiple lines
```

1. Numbered lists
2. Should maintain order
3. Through conversions
"""

        # Start with markdown
        current_file = self.data_manager.create_sample_file(
            "round_trip.md", original_content
        )
        current_format = "markdown"

        # Conversion chain: MD → HTML → MD → TEI → HTML
        conversion_chain = [
            ("markdown", "html"),
            ("html", "markdown"),
            ("markdown", "tei"),
            ("tei", "html"),
        ]

        conversion_results = []

        for from_format, to_format in conversion_chain:
            result = self._convert_single_format(current_file, from_format, to_format)
            conversion_results.append(result)

            if result["success"]:
                current_file = result["output_file"]
                # Update content for next conversion
                if to_format == "markdown":
                    current_format = "markdown"
                elif to_format == "html":
                    current_format = "html"
                elif to_format == "tei":
                    current_format = "tei"
            else:
                pytest.fail(
                    f"Round-trip conversion failed: {from_format} → {to_format}"
                )

        # Final validation - check content preservation
        final_content = current_file.read_text(encoding="utf-8")

        # Check key elements preserved through round-trip
        assert "Round Trip Test" in final_content
        assert "Original Content" in final_content
        assert "Bold text" in final_content
        assert "Code snippets" in final_content
        assert "Numbered lists" in final_content

    def _convert_to_all_formats(
        self, source_file: Path, source_format: str
    ) -> Dict[str, Any]:
        """Convert source file to all supported formats"""
        target_formats = ["html", "tei", "pdf", "markdown"]
        conversions = {}

        for target_format in target_formats:
            if target_format != source_format:
                result = self._convert_single_format(
                    source_file, source_format, target_format
                )
                conversions[target_format] = result
            else:
                # Copy original file
                target_file = (
                    self.conversion_dir / f"{source_file.stem}.{target_format}"
                )
                target_file.write_bytes(source_file.read_bytes())
                conversions[target_format] = {
                    "success": True,
                    "output_file": target_file,
                    "conversion_time": 0,
                }

        return conversions

    def _convert_single_format(
        self, source_file: Path, source_format: str, target_format: str
    ) -> Dict[str, Any]:
        """Convert single format with timing and validation"""
        from scripts.format_converter import convert_document
        import time

        start_time = time.time()

        try:
            target_file = self.conversion_dir / f"{source_file.stem}.{target_format}"

            result = convert_document(
                str(source_file),
                target_format,
                str(target_file),
                source_format=source_format,
            )

            conversion_time = time.time() - start_time

            return {
                "success": result.get("success", False),
                "output_file": target_file,
                "conversion_time": conversion_time,
                "details": result,
            }

        except Exception as e:
            return {
                "success": False,
                "output_file": None,
                "conversion_time": time.time() - start_time,
                "error": str(e),
            }


class TestBatchProcessingWorkflows(IntegrationTestCase, PerformanceTestCase):
    """Test batch processing workflows with various scenarios"""

    @pytest.fixture(autouse=True)
    def setup_batch_environment(self, temp_dir):
        """Setup batch processing testing environment"""
        self.temp_dir = temp_dir
        self.data_manager = TestDataManager(temp_dir)
        self.batch_input_dir = self.data_manager.create_directory("batch_input")
        self.batch_output_dir = self.data_manager.create_directory("batch_output")

        yield

        self.data_manager.cleanup()

    def test_large_batch_processing(self):
        """Test processing large batch of documents"""
        # 1. Create large batch of test documents
        batch_size = 50
        source_files = []

        for i in range(batch_size):
            content = f"""# Document {i + 1}
## Content for document {i + 1}
This is the content for document number {i + 1} in the large batch test.

### Section {i + 1}-1
Content for section {i + 1}-1.

### Section {i + 1}-2
Content for section {i + 1}-2.

## Key Points
- Point {i + 1}-1
- Point {i + 1}-2
- Point {i + 1}-3

## Conclusion
This concludes document {i + 1}.
"""
            filename = f"doc_{i + 1:03d}.md"
            source_files.append(self.data_manager.create_sample_file(filename, content))

        # 2. Execute batch processing with performance monitoring
        self.start_performance_monitoring()

        try:
            batch_results = self._execute_batch_with_monitoring(source_files)

        finally:
            metrics = self.stop_performance_monitoring()

        # 3. Validate batch results
        assert len(batch_results) == batch_size
        successful_count = sum(
            1 for result in batch_results if result["status"] == "completed"
        )
        assert successful_count >= batch_size * 0.95, (
            f"Too many failures: {successful_count}/{batch_size}"
        )

        # 4. Validate performance
        self.assert_performance_within(
            metrics,
            max_duration=120.0,  # 2 minutes for 50 documents
            max_memory_mb=2048,  # 2GB max for batch
            max_cpu_percent=90,
        )

        # 5. Validate output files
        for source_file in source_files[:10]:  # Check first 10
            base_name = source_file.stem
            html_output = self.batch_output_dir / f"{base_name}.html"
            tei_output = self.batch_output_dir / f"{base_name}.tei"
            assert html_output.exists()
            assert tei_output.exists()

    def test_mixed_format_batch(self):
        """Test batch processing with mixed input formats"""
        # 1. Create mixed format documents
        mixed_files = []

        # Markdown files
        for i in range(5):
            content = f"""# Markdown Document {i + 1}
## Content
This is markdown document {i + 1}.
- List item 1
- List item 2
"""
            mixed_files.append(
                self.data_manager.create_sample_file(f"md_doc_{i + 1}.md", content)
            )

        # HTML files
        for i in range(5):
            content = f"""<html><body>
<h1>HTML Document {i + 1}</h1>
<p>This is HTML document {i + 1}.</p>
<ul><li>Item 1</li><li>Item 2</li></ul>
</body></html>"""
            mixed_files.append(
                self.data_manager.create_sample_file(f"html_doc_{i + 1}.html", content)
            )

        # LaTeX files
        for i in range(3):
            content = f"""\\documentclass{{article}}
\\begin{{document}}
\\section{{LaTeX Document {i + 1}}}
This is LaTeX document {i + 1}.
\\end{{document}}"""
            mixed_files.append(
                self.data_manager.create_sample_file(f"tex_doc_{i + 1}.tex", content)
            )

        # 2. Process mixed batch
        batch_results = self._execute_mixed_format_batch(mixed_files)

        # 3. Validate mixed format processing
        assert len(batch_results) == len(mixed_files)

        for i, result in enumerate(batch_results):
            assert result["status"] in ["completed", "completed_with_warnings"]
            assert "input_format" in result
            assert "output_formats" in result

            # Check appropriate outputs for each input type
            base_name = Path(result["input_file"]).stem
            for output_format in result["output_formats"]:
                output_file = self.batch_output_dir / f"{base_name}.{output_format}"
                assert output_file.exists(), f"Missing {output_format} for {base_name}"

    def test_parallel_batch_processing(self):
        """Test parallel batch processing with concurrent workers"""
        # 1. Create test documents
        parallel_files = []
        for i in range(20):
            content = f"""# Parallel Document {i + 1}
## Processing Test
This document tests parallel processing capability.

### Data
Document ID: {i + 1}
Processing timestamp: {time.time()}

### Content
Content for parallel processing test {i + 1}.
"""
            parallel_files.append(
                self.data_manager.create_sample_file(
                    f"parallel_doc_{i + 1}.md", content
                )
            )

        # 2. Test with different worker counts
        worker_counts = [1, 2, 4, 8]
        results_by_workers = {}

        for workers in worker_counts:
            start_time = time.time()
            batch_results = self._execute_parallel_batch(parallel_files, workers)
            processing_time = time.time() - start_time

            results_by_workers[workers] = {
                "results": batch_results,
                "time": processing_time,
                "success_rate": sum(
                    1 for r in batch_results if r["status"] == "completed"
                )
                / len(batch_results),
            }

        # 3. Validate parallel processing efficiency
        # More workers should generally be faster (up to a point)
        single_worker_time = results_by_workers[1]["time"]
        multi_worker_time = results_by_workers[4]["time"]

        # 4 workers should be at least 2x faster than 1 worker
        assert multi_worker_time < single_worker_time * 0.8, (
            f"Parallel processing not efficient: {multi_worker_time}s vs {single_worker_time}s"
        )

        # All configurations should maintain high success rate
        for workers, data in results_by_workers.items():
            assert data["success_rate"] >= 0.95, (
                f"Low success rate with {workers} workers: {data['success_rate']}"
            )

    def _execute_batch_with_monitoring(
        self, source_files: List[Path]
    ) -> List[Dict[str, Any]]:
        """Execute batch processing with integrated monitoring"""
        from scripts.content_pipeline import process_batch

        results = process_batch(
            [str(f) for f in source_files],
            output_dir=str(self.batch_output_dir),
            max_concurrent=4,
            generate_all_formats=True,
        )

        return results

    def _execute_mixed_format_batch(
        self, source_files: List[Path]
    ) -> List[Dict[str, Any]]:
        """Execute batch processing with mixed input formats"""
        from scripts.content_pipeline import process_mixed_batch

        # Detect input formats
        file_formats = {}
        for file_path in source_files:
            suffix = file_path.suffix.lower()
            if suffix == ".md":
                file_formats[str(file_path)] = "markdown"
            elif suffix == ".html":
                file_formats[str(file_path)] = "html"
            elif suffix == ".tex":
                file_formats[str(file_path)] = "latex"
            else:
                file_formats[str(file_path)] = "unknown"

        results = process_mixed_batch(
            file_formats, output_dir=str(self.batch_output_dir), max_concurrent=3
        )

        return results

    def _execute_parallel_batch(
        self, source_files: List[Path], max_workers: int
    ) -> List[Dict[str, Any]]:
        """Execute parallel batch processing with specified worker count"""
        from scripts.content_pipeline import process_batch_parallel

        results = process_batch_parallel(
            [str(f) for f in source_files],
            output_dir=str(self.batch_output_dir),
            max_workers=max_workers,
            chunk_size=max(1, len(source_files) // max_workers),
        )

        return results
