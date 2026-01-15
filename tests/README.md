# Testing Infrastructure for Integral Philosophy Publishing System

This directory contains comprehensive testing infrastructure for the Integral Philosophy Publishing System. The infrastructure is designed to support all phases of testing from unit tests to end-to-end integration tests.

## Directory Structure

```
tests/
├── conftest.py                    # Pytest configuration and shared fixtures
├── pytest.ini                    # Test execution settings
├── requirements.txt               # Test dependencies
├── run_tests.py                   # Test runner script
├── utils/
│   ├── __init__.py
│   ├── test_helpers.py           # Common test utilities
│   └── base_test_classes.py     # Abstract base test classes
├── fixtures/
│   ├── __init__.py
│   ├── sample_data/              # Sample test data files
│   ├── mock_data/                # Mock responses and data
│   └── test_files/              # Test input/output files
├── reports/                      # Test reports and outputs
├── unit/                         # Unit tests
│   ├── __init__.py
│   ├── validators/
│   ├── scripts/
│   ├── api/
│   └── web_interface/
├── integration/                  # Integration tests
│   ├── __init__.py
│   ├── pipeline/
│   ├── api_web/
│   └── validators/
├── functional/                   # Functional tests
│   ├── __init__.py
│   ├── scraping/
│   ├── conversion/
│   ├── tei_uml/
│   └── validation/
├── e2e/                         # End-to-end tests
│   ├── __init__.py
│   ├── workflows/
│   └── user_scenarios/
├── performance/                  # Performance tests
│   ├── __init__.py
│   ├── load/
│   ├── stress/
│   └── benchmark/
└── security/                     # Security tests
    ├── __init__.py
    ├── auth/
    ├── injection/
    └── xss/
```

## Features

### 🔧 Test Configuration
- **Pytest Configuration**: Comprehensive pytest settings with markers, coverage, and reporting
- **Shared Fixtures**: Reusable fixtures for common test scenarios
- **Custom Markers**: Organize tests by type and requirements

### 🛠️ Test Utilities
- **Test Data Management**: Create and manage test files and directories
- **WebDriver Management**: Selenium WebDriver setup and cleanup
- **Mock Server**: HTTP server for testing API endpoints
- **Performance Monitoring**: CPU, memory, and timing metrics
- **File Comparison**: Tools for comparing files and directories
- **Async Testing**: Utilities for asynchronous test scenarios

### 🏗️ Base Test Classes
- **BaseTestCase**: Common functionality for all tests
- **APITestCase**: HTTP API testing with request/response helpers
- **WebTestCase**: Flask web interface testing
- **SeleniumTestCase**: Browser automation testing
- **PerformanceTestCase**: Performance testing with metrics
- **IntegrationTestCase**: System integration testing
- **SecurityTestCase**: Security vulnerability testing

### 📊 Test Fixtures
- **Sample Documents**: Markdown, HTML, LaTeX, JSON test data
- **Mock Responses**: Pre-configured API responses
- **Test Configurations**: Various component configurations
- **Sample AST Data**: Abstract Syntax Tree test data

## Quick Start

### 1. Setup Test Environment

```bash
# Install test dependencies
pip install -r tests/requirements.txt

# Setup test environment
python tests/run_tests.py setup
```

### 2. Run Tests

```bash
# Run all tests
python tests/run_tests.py all

# Run unit tests only
python tests/run_tests.py unit

# Run integration tests
python tests/run_tests.py integration

# Run functional tests
python tests/run_tests.py functional

# Run end-to-end tests
python tests/run_tests.py e2e

# Run performance tests
python tests/run_tests.py performance

# Run security tests
python tests/run_tests.py security

# Run quick tests (excluding slow and selenium tests)
python tests/run_tests.py quick

# Run CI-optimized tests
python tests/run_tests.py ci
```

### 3. Run with Coverage

```bash
# Run tests with coverage analysis
python tests/run_tests.py all --coverage

# Coverage report will be generated in tests/reports/htmlcov/
```

### 4. Run Tests in Parallel

```bash
# Run tests in parallel for faster execution
python tests/run_tests.py all --parallel
```

## Test Categories

### 🔍 Unit Tests (`unit/`)
Test individual components in isolation:
- Format converters
- TEI generators
- UML generators
- Validation functions
- Utility functions

**Example:**
```python
class TestFormatConverter(ComponentTestCase):
    def test_markdown_to_html(self):
        result = self.format_converter.convert(
            self.markdown_file, 
            "html"
        )
        assert result.success
        assert result.output_file.exists()
```

### 🔗 Integration Tests (`integration/`)
Test component interactions:
- API to Web Interface integration
- Pipeline component interactions
- Validator integration
- Data flow between components

**Example:**
```python
class TestPipelineIntegration(IntegrationTestCase):
    def test_full_conversion_pipeline(self):
        # Test markdown → AST → HTML → TEI pipeline
        pass
```

### ⚡ Functional Tests (`functional/`)
Test specific features and functionality:
- Web scraping capabilities
- Format conversion accuracy
- TEI generation correctness
- UML diagram generation

**Example:**
```python
class TestWebScraping(FunctionalTestCase):
    def test_scrape_website(self):
        result = self.web_scraper.scrape_url("https://example.com")
        assert len(result["pages"]) > 0
```

### 🌐 End-to-End Tests (`e2e/`)
Test complete user workflows:
- Document processing pipeline
- API job submission to completion
- Web interface user journeys
- System-wide workflows

**Example:**
```python
class TestDocumentProcessing(E2ETestCase):
    def test_markdown_to_publication_pipeline(self):
        # Test complete workflow from upload to publication
        pass
```

### 📈 Performance Tests (`performance/`)
Test system performance and scalability:
- Load testing
- Stress testing
- Benchmark comparisons
- Resource usage monitoring

**Example:**
```python
class TestPerformance(PerformanceTestCase):
    def test_conversion_performance(self):
        with self.performance_monitor:
            result = self.format_converter.convert(large_file, "html")
        
        metrics = self.performance_monitor.get_metrics()
        assert metrics["duration_seconds"] < 10.0
```

### 🔒 Security Tests (`security/`)
Test for security vulnerabilities:
- Authentication bypass
- Injection attacks
- XSS vulnerabilities
- Access control

**Example:**
```python
class TestSecurity(SecurityTestCase):
    def test_xss_prevention(self):
        malicious_input = "<script>alert('xss')</script>"
        response = self.web_client.post("/process", data=malicious_input)
        self.assert_no_xss_vulnerability(response.get_data(as_text=True))
```

## Test Markers

The following pytest markers are available for organizing tests:

- `@pytest.mark.unit` - Unit tests
- `@pytest.mark.integration` - Integration tests
- `@pytest.mark.functional` - Functional tests
- `@pytest.mark.e2e` - End-to-end tests
- `@pytest.mark.performance` - Performance tests
- `@pytest.mark.security` - Security tests
- `@pytest.mark.slow` - Slow-running tests
- `@pytest.mark.selenium` - Tests requiring Selenium WebDriver
- `@pytest.mark.requires_components` - Tests requiring all system components

## Configuration Files

### `conftest.py`
Contains shared fixtures for:
- Test data and directories
- Mock WebDriver instances
- API and web clients
- Component instances
- Performance monitoring

### `pytest.ini`
Pytest configuration with:
- Test discovery patterns
- Coverage settings
- Output formatting
- Marker definitions
- Timeout and async settings

### `requirements.txt`
Test-specific dependencies:
- Testing frameworks
- Selenium WebDriver
- Performance tools
- Security scanners
- Reporting tools

## Test Utilities

### TestDataManager
Create and manage test files and directories:
```python
data_manager = TestDataManager()
file_path = data_manager.create_sample_file("test.md", "# Test Content")
# Automatically cleaned up after test
```

### WebDriverManager
Manage Selenium WebDriver instances:
```python
driver_manager = WebDriverManager(headless=True)
driver = driver_manager.get_chrome_driver()
# Driver automatically cleaned up
```

### PerformanceMonitor
Monitor performance during tests:
```python
monitor = PerformanceMonitor()
monitor.start_monitoring()
# Run test code here
metrics = monitor.stop_monitoring()
assert metrics["peak_memory_mb"] < 100
```

### MockServer
Mock HTTP server for API testing:
```python
server = MockServer()
server.add_response("/test", {"data": "value"})
server.start()
# Make requests to localhost:8080/test
server.stop()
```

## Best Practices

### 1. Test Organization
- Use appropriate test categories
- Follow naming conventions
- Group related tests
- Use descriptive test names

### 2. Test Data
- Use fixtures for reusable data
- Create minimal test data
- Clean up test artifacts
- Avoid hardcoded paths

### 3. Assertions
- Use specific assertion methods
- Include helpful error messages
- Test edge cases
- Verify state changes

### 4. Performance
- Keep tests fast
- Use parallel execution when possible
- Mark slow tests appropriately
- Monitor test performance

### 5. Security
- Test for common vulnerabilities
- Validate input sanitization
- Test authentication/authorization
- Check for information disclosure

## Continuous Integration

The test infrastructure is designed for CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run Tests
  run: python tests/run_tests.py ci --coverage

- name: Upload Coverage
  uses: codecov/codecov-action@v3
  with:
    file: tests/reports/coverage.xml
```

## Troubleshooting

### Common Issues

1. **Selenium WebDriver Issues**
   ```bash
   # Install drivers
   pip install webdriver-manager
   export CHROME_DRIVER_VERSION=latest
   ```

2. **Import Errors**
   ```bash
   # Check PYTHONPATH
   export PYTHONPATH="${PYTHONPATH}:$(pwd)"
   ```

3. **Permission Errors**
   ```bash
   # Create necessary directories
   mkdir -p tests/reports api_jobs web_jobs
   ```

4. **Timeout Issues**
   ```bash
   # Increase timeout
   python tests/run_tests.py all --args="--timeout=600"
   ```

### Debug Mode

Run tests with verbose output and debugging:
```bash
python tests/run_tests.py unit --verbose --args="-s --pdb"
```

## Contributing

When adding new tests:

1. Choose appropriate test category
2. Use existing fixtures and utilities
3. Follow naming conventions
4. Add proper documentation
5. Update this README if needed

## License

This test infrastructure is part of the Integral Philosophy Publishing System and follows the same licensing terms.