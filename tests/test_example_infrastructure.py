"""
Example unit test demonstrating the test infrastructure
"""

import pytest
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from utils.base_test_classes import ComponentTestCase, BaseTestCase
from utils.test_helpers import TestDataManager, PerformanceMonitor


class TestExampleComponent(ComponentTestCase):
    """Example component test using base test class"""

    def test_sample_data_manager(self):
        """Test TestDataManager functionality"""
        # Create sample file
        file_path = self.data_manager.create_sample_file(
            "test.txt", "Test content for data manager"
        )

        # Verify file was created
        assert file_path.exists()
        assert file_path.read_text() == "Test content for data manager"

        # Verify it's tracked for cleanup
        assert file_path in self.data_manager.created_files

    def test_file_comparison(self):
        """Test file comparison utility"""
        # Create two identical files
        file1 = self.data_manager.create_sample_file("file1.txt", "Same content")
        file2 = self.data_manager.create_sample_file("file2.txt", "Same content")

        # Test comparison
        self.assert_files_equal(file1, file2)

        # Create different file
        file3 = self.data_manager.create_sample_file("file3.txt", "Different content")

        # Should not be equal (this will raise AssertionError)
        with pytest.raises(AssertionError):
            self.assert_files_equal(file1, file3)

    def test_json_validation(self):
        """Test JSON validation utility"""
        valid_json = '{"key": "value", "number": 42}'
        invalid_json = '{"key": "value", "number":}'

        # Should pass
        self.assert_valid_json(valid_json)

        # Should fail
        with pytest.raises(AssertionError):
            self.assert_valid_json(invalid_json)

    def test_xml_validation(self):
        """Test XML validation utility"""
        valid_xml = '<?xml version="1.0"?><root><item>test</item></root>'
        invalid_xml = '<?xml version="1.0"?><root><item>test</root>'

        # Should pass
        self.assert_valid_xml(valid_xml)

        # Should fail
        with pytest.raises(AssertionError):
            self.assert_valid_xml(invalid_xml)

    def test_component_response_validation(self):
        """Test component response validation"""
        valid_response = {"status": "success", "data": {"result": "ok"}}

        # Should pass
        self.assert_component_response(valid_response, ["status", "data"])

        # Should fail - missing key
        invalid_response = {"status": "success"}
        with pytest.raises(AssertionError):
            self.assert_component_response(invalid_response, ["status", "data"])

    def test_mock_component_creation(self):
        """Test mock component factory"""
        mock_component = self.create_mock_component(["process", "validate"])

        # Verify methods exist
        assert hasattr(mock_component, "process")
        assert hasattr(mock_component, "validate")

        # Test method calls
        mock_component.process.return_value = {"success": True}
        result = mock_component.process("input")

        assert result == {"success": True}
        mock_component.process.assert_called_once_with("input")


class TestPerformanceMonitoring(BaseTestCase):
    """Example performance testing using base test class"""

    def test_performance_monitoring(self):
        """Test performance monitoring functionality"""
        monitor = PerformanceMonitor()

        # Start monitoring
        monitor.start_monitoring()

        # Simulate some work
        import time

        time.sleep(0.1)  # 100ms of work

        # Stop monitoring
        metrics = monitor.stop_monitoring()

        # Verify metrics
        assert "duration_seconds" in metrics
        assert metrics["duration_seconds"] >= 0.1
        assert "peak_memory_mb" in metrics
        assert "cpu_samples_count" in metrics


class TestInfrastructureIntegration(BaseTestCase):
    """Example infrastructure integration test"""

    def test_full_test_workflow(self):
        """Test complete workflow using infrastructure"""
        # 1. Create test data
        test_files = self.create_sample_files()
        assert "sample.md" in test_files

        # 2. Verify content
        markdown_content = test_files["sample.md"].read_text()
        assert "# Simple Document" in markdown_content

        # 3. Test file hash calculation
        file_hash = self.data_manager.get_file_hash(test_files["sample.md"])
        assert len(file_hash) == 64  # SHA256 length
        assert isinstance(file_hash, str)

        # 4. Test directory structure creation
        test_dir = self.data_manager.create_directory("test_structure")
        assert test_dir.exists()
        assert test_dir.is_dir()

        # 5. Test complex file structure creation
        structure = {
            "project": {
                "src": {"main.py": "# Main module", "utils.py": "# Utility functions"},
                "tests": {"test_main.py": "# Main test"},
                "config.json": '{"name": "test_project"}',
            }
        }

        created = self.data_manager.create_file_structure(structure)
        assert "project" in created
        assert created["project"].is_dir()
        assert (created["project"] / "config.json").exists()

    @pytest.mark.slow
    def test_slow_operation_simulation(self):
        """Example of a slow test marker usage"""
        import time

        time.sleep(0.5)  # Simulate slow operation
        assert True  # Test passes after delay


@pytest.mark.functional
class TestFunctionalExample(BaseTestCase):
    """Example functional test with marker"""

    def test_format_conversion_simulation(self):
        """Simulate format conversion functionality"""
        # Create input file
        input_content = "# Title\n\nSome content"
        input_file = self.data_manager.create_sample_file("input.md", input_content)

        # Simulate conversion (normally would use actual converter)
        output_content = f"<h1>Title</h1><p>Some content</p>"
        output_file = self.data_manager.create_sample_file(
            "output.html", output_content
        )

        # Verify conversion worked
        assert input_file.exists()
        assert output_file.exists()
        assert "<h1>Title</h1>" in output_file.read_text()

        # Verify files are different formats but related
        input_hash = self.data_manager.get_file_hash(input_file)
        output_hash = self.data_manager.get_file_hash(output_file)
        assert input_hash != output_hash  # Different content


if __name__ == "__main__":
    # Allow running this test file directly
    pytest.main([__file__, "-v"])
