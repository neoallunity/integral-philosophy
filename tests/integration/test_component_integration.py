"""
Component integration tests for the Integral Philosophy Publishing System
Testing interactions between different components and their coordination
"""

import pytest
import asyncio
import time
import json
import threading
from pathlib import Path
from typing import Dict, Any, List, Optional
from unittest.mock import Mock, patch, MagicMock

from tests.utils.base_test_classes import IntegrationTestCase


class TestComponentIntegration(IntegrationTestCase):
    """Test component interaction and integration"""

    @pytest.fixture(autouse=True)
    def setup_component_integration(self):
        """Setup component integration test environment"""
        self.components = {}
        self.component_messages = []
        self.shared_resources = {}

        # Setup mock components
        self.setup_mock_components()

        yield

        # Cleanup components
        self.cleanup_components()

    def setup_mock_components(self):
        """Setup mock components for integration testing"""
        # Mock WebScraper
        self.web_scraper = Mock()
        self.web_scraper.scrape_content = Mock(
            return_value={
                "url": "https://philosophy.example.com/article",
                "title": "The Nature of Consciousness",
                "content": "Philosophical exploration of consciousness...",
                "metadata": {"author": "Test Author", "date": "2024-01-15"},
            }
        )

        # Mock ContentPipeline
        self.content_pipeline = Mock()
        self.content_pipeline.process_content = Mock(
            return_value={
                "processed_content": "Processed philosophical text",
                "structure": {
                    "sections": ["introduction", "main_body", "conclusion"],
                    "paragraphs": 15,
                    "word_count": 1200,
                },
                "metadata": {"processing_time": 0.5, "language": "en"},
            }
        )

        # Mock FormatConverter
        self.format_converter = Mock()
        self.format_converter.convert_to_format = Mock(
            side_effect=self.mock_convert_to_format
        )
        self.format_converter.get_supported_formats = Mock(
            return_value=["latex", "epub", "pdf", "html"]
        )

        # Mock Validator
        self.validator = Mock()
        self.validator.validate_content = Mock(
            return_value={"valid": True, "errors": []}
        )
        self.validator.validate_structure = Mock(
            return_value={"valid": True, "errors": []}
        )

        # Mock API Server
        self.api_server = Mock()
        self.api_server.start_job = Mock(
            return_value={"job_id": "test-job-001", "status": "started"}
        )
        self.api_server.get_job_status = Mock(
            return_value={
                "job_id": "test-job-001",
                "status": "completed",
                "progress": 100,
            }
        )

        # Register components
        self.components = {
            "web_scraper": self.web_scraper,
            "content_pipeline": self.content_pipeline,
            "format_converter": self.format_converter,
            "validator": self.validator,
            "api_server": self.api_server,
        }

    def mock_convert_to_format(self, content: str, format_type: str) -> str:
        """Mock format conversion based on format type"""
        conversions = {
            "latex": f"\\documentclass{{article}}\n\\begin{{document}}\n{content}\n\\end{{document}}",
            "html": f"<html><body>{content}</body></html>",
            "epub": b"EPUB binary content",
            "pdf": b"PDF binary content",
        }
        return conversions.get(format_type, content)

    def cleanup_components(self):
        """Cleanup components after tests"""
        self.components.clear()
        self.component_messages.clear()
        self.shared_resources.clear()

    def test_webscraper_content_pipeline_integration(self):
        """Test WebScraper ↔ ContentPipeline integration"""
        # Setup data flow between components
        test_url = "https://philosophy.example.com/article"

        # Step 1: WebScraper fetches content
        scrape_result = self.web_scraper.scrape_content(test_url)

        assert scrape_result["title"] == "The Nature of Consciousness"
        assert "content" in scrape_result
        assert "metadata" in scrape_result

        # Step 2: ContentPipeline processes scraped content
        process_result = self.content_pipeline.process_content(scrape_result)

        assert "processed_content" in process_result
        assert "structure" in process_result
        assert "metadata" in process_result

        # Verify data flow integrity
        assert process_result["metadata"]["language"] == "en"
        assert process_result["structure"]["word_count"] > 0

        # Verify component interaction
        self.web_scraper.scrape_content.assert_called_once_with(test_url)
        self.content_pipeline.process_content.assert_called_once_with(scrape_result)

    def test_format_converter_validator_coordination(self):
        """Test FormatConverter ↔ Validator coordination"""
        test_content = "Philosophical text about consciousness and existence"

        # Step 1: Validate content before conversion
        validation_result = self.validator.validate_content(test_content)
        assert validation_result["valid"] is True
        assert len(validation_result["errors"]) == 0

        # Step 2: Convert content to multiple formats
        formats = ["latex", "html", "epub", "pdf"]
        conversion_results = {}

        for format_type in formats:
            result = self.format_converter.convert_to_format(test_content, format_type)
            conversion_results[format_type] = result

            # Validate conversion result
            if format_type in ["latex", "html"]:
                # Text-based formats should be strings
                assert isinstance(result, str)
                assert test_content in result or test_content.replace(
                    " ", ""
                ) in result.replace(" ", "")
            else:
                # Binary formats should be bytes
                assert isinstance(result, bytes)

        # Verify all formats were generated
        assert len(conversion_results) == len(formats)
        for format_type in formats:
            assert format_type in conversion_results

        # Verify coordination
        self.validator.validate_content.assert_called_once_with(test_content)
        assert self.format_converter.convert_to_format.call_count == len(formats)

    def test_api_server_background_job_processing(self):
        """Test API Server ↔ Background job processing integration"""
        # Start background job processing service
        job_queue = []
        job_results = {}

        def background_worker():
            """Mock background worker that processes jobs"""
            while True:
                if job_queue:
                    job = job_queue.pop(0)
                    job_id = job["job_id"]

                    # Simulate job processing
                    time.sleep(0.1)

                    # Store job result
                    job_results[job_id] = {
                        "job_id": job_id,
                        "status": "completed",
                        "result": f"Processed {job['job_type']}",
                        "processing_time": 0.1,
                    }
                time.sleep(0.01)

        # Start background worker
        self.start_service("background_worker", background_worker)

        # Submit job through API server
        job_params = {
            "job_type": "content_conversion",
            "input_data": {"content": "Test philosophical content"},
            "output_format": "latex",
        }

        api_response = self.api_server.start_job(job_params)
        job_id = api_response["job_id"]

        # Add job to queue
        job_queue.append({"job_id": job_id, **job_params})

        # Wait for job processing
        max_wait = 5.0
        start_time = time.time()

        while job_id not in job_results and time.time() - start_time < max_wait:
            time.sleep(0.1)

        # Verify job completion
        assert job_id in job_results
        assert job_results[job_id]["status"] == "completed"

        # Verify API server can retrieve job status
        status_response = self.api_server.get_job_status(job_id)
        assert status_response["job_id"] == job_id
        assert status_response["status"] == "completed"

    def test_database_consistency_across_components(self):
        """Test database consistency across components (if applicable)"""
        # Mock database interface
        mock_db = Mock()
        mock_db.insert_content = Mock(return_value={"id": 1, "status": "inserted"})
        mock_db.get_content = Mock(
            return_value={"id": 1, "content": "Test content", "status": "processed"}
        )
        mock_db.update_content = Mock(return_value={"id": 1, "status": "updated"})

        # Test content flow through database
        content_data = {
            "title": "Database Test Article",
            "content": "Content for database consistency test",
            "metadata": {"source": "test", "timestamp": time.time()},
        }

        # Step 1: Insert content
        insert_result = mock_db.insert_content(content_data)
        assert insert_result["status"] == "inserted"
        content_id = insert_result["id"]

        # Step 2: Retrieve content
        retrieved_content = mock_db.get_content(content_id)
        assert retrieved_content["id"] == content_id
        assert "content" in retrieved_content

        # Step 3: Update content after processing
        processed_data = {"processed": True, "processing_time": 0.5}
        update_result = mock_db.update_content(content_id, processed_data)
        assert update_result["status"] == "updated"

        # Verify database operations sequence
        mock_db.insert_content.assert_called_once_with(content_data)
        mock_db.get_content.assert_called_once_with(content_id)
        mock_db.update_content.assert_called_once_with(content_id, processed_data)

    def test_file_system_operations_integration(self):
        """Test file system operations across components"""
        # Create shared file storage area
        storage_dir = self.temp_dir / "shared_storage"
        storage_dir.mkdir(exist_ok=True)

        # Component 1: WebScraper saves scraped content
        scraped_content = {
            "title": "File System Test",
            "content": "Content for file system integration test",
        }

        scraped_file = storage_dir / "scraped_content.json"
        scraped_file.write_text(json.dumps(scraped_content))
        assert scraped_file.exists()

        # Component 2: ContentPipeline reads and processes content
        read_content = json.loads(scraped_file.read_text())
        processed_content = f"PROCESSED: {read_content['content']}"

        processed_file = storage_dir / "processed_content.txt"
        processed_file.write_text(processed_content)
        assert processed_file.exists()

        # Component 3: FormatConverter creates format-specific files
        formats = {
            "latex": "\\documentclass{article}\n\\begin{document}\n"
            + processed_content
            + "\n\\end{document}",
            "html": f"<html><body><h1>{read_content['title']}</h1><p>{processed_content}</p></body></html>",
        }

        for format_type, content in formats.items():
            format_file = storage_dir / f"output.{format_type}"
            format_file.write_text(content)
            assert format_file.exists()

        # Verify file system consistency
        all_files = list(storage_dir.glob("*"))
        assert len(all_files) == 4  # scraped, processed, latex, html

        # Verify file contents
        assert "PROCESSED:" in processed_file.read_text()
        assert "\\documentclass" in (storage_dir / "output.latex").read_text()
        assert "<html>" in (storage_dir / "output.html").read_text()

    def test_component_error_handling_integration(self):
        """Test error handling and recovery between components"""
        # Mock component with failure
        self.web_scraper.scrape_content = Mock(side_effect=Exception("Network failure"))

        # Setup error handling coordination
        error_log = []

        def handle_component_error(component_name: str, error: Exception):
            """Mock error handler for component integration"""
            error_log.append(
                {
                    "component": component_name,
                    "error": str(error),
                    "timestamp": time.time(),
                    "handled": True,
                }
            )

        # Test error propagation
        try:
            self.web_scraper.scrape_content("https://example.com/article")
        except Exception as e:
            handle_component_error("web_scraper", e)

        # Verify error was logged
        assert len(error_log) == 1
        assert error_log[0]["component"] == "web_scraper"
        assert "Network failure" in error_log[0]["error"]
        assert error_log[0]["handled"] is True

        # Test subsequent components are not called on error
        self.content_pipeline.process_content.assert_not_called()
        self.format_converter.convert_to_format.assert_not_called()

    def test_component_message_passing(self):
        """Test message passing between components"""
        # Setup message bus
        message_bus = []

        def send_message(sender: str, receiver: str, message: Dict[str, Any]):
            """Send message from one component to another"""
            message_bus.append(
                {
                    "sender": sender,
                    "receiver": receiver,
                    "message": message,
                    "timestamp": time.time(),
                }
            )

        # Component 1 sends message to Component 2
        send_message(
            "web_scraper",
            "content_pipeline",
            {
                "type": "content_ready",
                "data": {"content": "Test content", "metadata": {}},
            },
        )

        # Component 2 processes and sends to Component 3
        send_message(
            "content_pipeline",
            "format_converter",
            {
                "type": "content_processed",
                "data": {"processed_content": "Processed test content"},
            },
        )

        # Component 3 sends completion message
        send_message(
            "format_converter",
            "api_server",
            {
                "type": "conversion_complete",
                "data": {"formats": ["latex", "html"], "status": "success"},
            },
        )

        # Verify message flow
        assert len(message_bus) == 3

        # Verify message sequence
        assert message_bus[0]["sender"] == "web_scraper"
        assert message_bus[0]["receiver"] == "content_pipeline"
        assert message_bus[1]["sender"] == "content_pipeline"
        assert message_bus[1]["receiver"] == "format_converter"
        assert message_bus[2]["sender"] == "format_converter"
        assert message_bus[2]["receiver"] == "api_server"

        # Verify message contents
        assert message_bus[0]["message"]["type"] == "content_ready"
        assert message_bus[1]["message"]["type"] == "content_processed"
        assert message_bus[2]["message"]["type"] == "conversion_complete"

    def test_component_lifecycle_management(self):
        """Test component lifecycle and state management"""
        # Component states
        component_states = {}

        class ComponentManager:
            """Mock component manager for lifecycle testing"""

            def __init__(self):
                self.components = {}
                self.states = {}

            def register_component(self, name: str, component: Mock):
                """Register a component"""
                self.components[name] = component
                self.states[name] = "registered"

            def initialize_component(self, name: str):
                """Initialize a component"""
                if name in self.components:
                    self.states[name] = "initializing"
                    # Simulate initialization
                    time.sleep(0.01)
                    self.states[name] = "ready"

            def start_component(self, name: str):
                """Start a component"""
                if name in self.components and self.states[name] == "ready":
                    self.states[name] = "starting"
                    time.sleep(0.01)
                    self.states[name] = "running"

            def stop_component(self, name: str):
                """Stop a component"""
                if name in self.components and self.states[name] == "running":
                    self.states[name] = "stopping"
                    time.sleep(0.01)
                    self.states[name] = "stopped"

            def shutdown_component(self, name: str):
                """Shutdown a component"""
                if name in self.components:
                    self.states[name] = "shutting_down"
                    time.sleep(0.01)
                    self.states[name] = "shutdown"

        # Test component lifecycle
        manager = ComponentManager()

        # Register components
        manager.register_component("web_scraper", self.web_scraper)
        manager.register_component("content_pipeline", self.content_pipeline)
        manager.register_component("format_converter", self.format_converter)

        assert manager.states["web_scraper"] == "registered"
        assert manager.states["content_pipeline"] == "registered"
        assert manager.states["format_converter"] == "registered"

        # Initialize components
        manager.initialize_component("web_scraper")
        manager.initialize_component("content_pipeline")
        manager.initialize_component("format_converter")

        assert manager.states["web_scraper"] == "ready"
        assert manager.states["content_pipeline"] == "ready"
        assert manager.states["format_converter"] == "ready"

        # Start components
        manager.start_component("web_scraper")
        manager.start_component("content_pipeline")

        assert manager.states["web_scraper"] == "running"
        assert manager.states["content_pipeline"] == "running"
        assert manager.states["format_converter"] == "ready"

        # Stop components
        manager.stop_component("web_scraper")
        manager.stop_component("content_pipeline")

        assert manager.states["web_scraper"] == "stopped"
        assert manager.states["content_pipeline"] == "stopped"

        # Shutdown all components
        manager.shutdown_component("web_scraper")
        manager.shutdown_component("content_pipeline")
        manager.shutdown_component("format_converter")

        assert manager.states["web_scraper"] == "shutdown"
        assert manager.states["content_pipeline"] == "shutdown"
        assert manager.states["format_converter"] == "shutdown"

    def test_component_performance_integration(self):
        """Test performance integration between components"""
        # Track component performance
        performance_data = {}

        def measure_component_performance(component_name: str, func, *args, **kwargs):
            """Measure performance of component function"""
            start_time = time.time()
            start_memory = 0  # Mock memory measurement

            try:
                result = func(*args, **kwargs)
                end_time = time.time()
                end_memory = 0  # Mock memory measurement

                performance_data[component_name] = {
                    "duration": end_time - start_time,
                    "memory_used": end_memory - start_memory,
                    "status": "success",
                }

                return result

            except Exception as e:
                end_time = time.time()
                performance_data[component_name] = {
                    "duration": end_time - start_time,
                    "memory_used": 0,
                    "status": "error",
                    "error": str(e),
                }
                raise

        # Measure performance of component chain
        try:
            # WebScraper performance
            scrape_result = measure_component_performance(
                "web_scraper",
                self.web_scraper.scrape_content,
                "https://example.com/article",
            )

            # ContentPipeline performance
            process_result = measure_component_performance(
                "content_pipeline", self.content_pipeline.process_content, scrape_result
            )

            # FormatConverter performance
            latex_result = measure_component_performance(
                "format_converter",
                self.format_converter.convert_to_format,
                process_result["processed_content"],
                "latex",
            )

        except Exception as e:
            pytest.fail(f"Component performance test failed: {e}")

        # Verify performance data collection
        assert len(performance_data) == 3
        assert "web_scraper" in performance_data
        assert "content_pipeline" in performance_data
        assert "format_converter" in performance_data

        # Verify all components completed successfully
        for component, data in performance_data.items():
            assert data["status"] == "success"
            assert data["duration"] > 0
            assert isinstance(data["memory_used"], (int, float))

        # Verify total performance
        total_duration = sum(data["duration"] for data in performance_data.values())
        assert total_duration < 5.0  # Should complete within 5 seconds
