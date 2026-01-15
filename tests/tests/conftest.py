"""
Pytest configuration and shared fixtures for Integral Philosophy Publishing System tests
"""

import pytest
import asyncio
import tempfile
import shutil
from pathlib import Path
import json
import sys
import os
from unittest.mock import Mock, MagicMock
import selenium.webdriver
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.webdriver.firefox.options import Options as FirefoxOptions
import flask
import threading
import time
import requests
from datetime import datetime

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / "scripts"))

# Import project modules
try:
    # Try to import API components
    from api_server import app as api_app
    from api_server import job_status

    API_AVAILABLE = True
except (ImportError, AssertionError) as e:
    print(f"Warning: API components not available: {e}")
    api_app = None
    job_status = {}
    API_AVAILABLE = False

try:
    # Try to import web interface
    from web_interface import app as web_app
    from web_interface import job_status as web_job_status

    WEB_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Web components not available: {e}")
    web_app = None
    web_job_status = {}
    WEB_AVAILABLE = False

try:
    # Try to import validators
    from validators.validators import ValidationSystem

    VALIDATORS_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Validators not available: {e}")
    ValidationSystem = None
    VALIDATORS_AVAILABLE = False

try:
    # Try to import format converter
    from format_converter import FormatConverter

    CONVERTER_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Format converter not available: {e}")
    FormatConverter = None
    CONVERTER_AVAILABLE = False

try:
    # Try to import TEI generator
    from tei_generator import TEIGenerator

    TEI_AVAILABLE = True
except ImportError as e:
    print(f"Warning: TEI generator not available: {e}")
    TEIGenerator = None
    TEI_AVAILABLE = False

try:
    # Try to import UML generator
    from ast_to_uml import UMLGenerator

    UML_AVAILABLE = True
except ImportError as e:
    print(f"Warning: UML generator not available: {e}")
    UMLGenerator = None
    UML_AVAILABLE = False

try:
    # Try to import web scraper
    from web_scraper import WebScraper

    SCRAPER_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Web scraper not available: {e}")
    WebScraper = None
    SCRAPER_AVAILABLE = False

try:
    # Try to import content pipeline
    from content_pipeline import ContentPipeline

    PIPELINE_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Content pipeline not available: {e}")
    ContentPipeline = None
    PIPELINE_AVAILABLE = False

# Overall availability flag
COMPONENTS_AVAILABLE = (
    API_AVAILABLE
    and WEB_AVAILABLE
    and VALIDATORS_AVAILABLE
    and CONVERTER_AVAILABLE
    and TEI_AVAILABLE
    and UML_AVAILABLE
    and SCRAPER_AVAILABLE
    and PIPELINE_AVAILABLE
)


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


@pytest.fixture(scope="session")
def temp_dir():
    """Create a temporary directory for tests"""
    temp_path = Path(tempfile.mkdtemp(prefix="integral_test_"))
    yield temp_path
    shutil.rmtree(temp_path, ignore_errors=True)


@pytest.fixture
def sample_markdown_content():
    """Sample markdown content for testing"""
    return """# Sample Document

This is a **sample** document with *markdown* formatting.

## Section 1

Here's some content with a [link](https://example.com) and an image:

![Sample Image](image.jpg)

### Subsection

Some code:

```python
def hello_world():
    print("Hello, World!")
```

## Section 2

- List item 1
- List item 2
- List item 3

> This is a blockquote
> with multiple lines

---

End of document.
"""


@pytest.fixture
def sample_html_content():
    """Sample HTML content for testing"""
    return """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Sample Document</title>
</head>
<body>
    <h1>Sample Document</h1>
    <p>This is a <strong>sample</strong> document with <em>HTML</em> formatting.</p>
    
    <h2>Section 1</h2>
    <p>Here's some content with a <a href="https://example.com">link</a> and an image:</p>
    <img src="image.jpg" alt="Sample Image">
    
    <h3>Subsection</h3>
    <p>Some code:</p>
    <pre><code>def hello_world():
    print("Hello, World!")</code></pre>
    
    <h2>Section 2</h2>
    <ul>
        <li>List item 1</li>
        <li>List item 2</li>
        <li>List item 3</li>
    </ul>
    
    <blockquote>
        <p>This is a blockquote<br>with multiple lines</p>
    </blockquote>
    
    <hr>
    
    <p>End of document.</p>
</body>
</html>"""


@pytest.fixture
def sample_latex_content():
    """Sample LaTeX content for testing"""
    return r"""\documentclass{article}
\usepackage{graphicx}
\usepackage{hyperref}

\title{Sample Document}
\author{Test Author}

\begin{document}

\maketitle

\section{Section 1}

This is a \textbf{sample} document with \textit{LaTeX} formatting.

Here's some content with a \href{https://example.com}{link} and an image:

\includegraphics{image.jpg}

\subsection{Subsection}

Some code:

\begin{verbatim}
def hello_world():
    print("Hello, World!")
\end{verbatim}

\section{Section 2}

\begin{itemize}
    \item List item 1
    \item List item 2
    \item List item 3
\end{itemize}

\begin{quote}
This is a blockquote\\
with multiple lines
\end{quote}

\hrulefill

End of document.

\end{document}"""


@pytest.fixture
def sample_ast_data():
    """Sample Abstract Syntax Tree data for testing"""
    return {
        "type": "document",
        "title": "Sample Document",
        "metadata": {"author": "Test Author", "date": "2024-01-01", "language": "en"},
        "children": [
            {"type": "heading", "level": 1, "content": "Sample Document"},
            {"type": "paragraph", "content": "This is a sample document."},
            {"type": "heading", "level": 2, "content": "Section 1"},
            {"type": "paragraph", "content": "Content for section 1."},
            {
                "type": "code_block",
                "language": "python",
                "content": "def hello_world():\n    print('Hello, World!')",
            },
        ],
    }


@pytest.fixture
def mock_webdriver():
    """Mock Selenium WebDriver for testing"""
    driver = Mock()
    driver.get = Mock()
    driver.quit = Mock()
    driver.current_url = "https://example.com"
    driver.title = "Example Page"
    driver.page_source = "<html><body>Test content</body></html>"
    driver.find_element = Mock(return_value=Mock())
    driver.find_elements = Mock(return_value=[Mock()])
    driver.execute_script = Mock(return_value=None)
    driver.save_screenshot = Mock(return_value=True)
    driver.add_cookie = Mock()
    driver.get_cookies = Mock(return_value=[{"name": "test", "value": "value"}])
    driver.delete_all_cookies = Mock()
    return driver


@pytest.fixture
def chrome_options():
    """Chrome options for Selenium WebDriver"""
    options = ChromeOptions()
    options.add_argument("--headless")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--disable-gpu")
    options.add_argument("--window-size=1920,1080")
    options.add_argument("--disable-extensions")
    options.add_argument("--disable-plugins")
    options.add_argument("--disable-images")
    return options


@pytest.fixture
def firefox_options():
    """Firefox options for Selenium WebDriver"""
    options = FirefoxOptions()
    options.add_argument("--headless")
    options.add_argument("--width=1920")
    options.add_argument("--height=1080")
    return options


@pytest.fixture
def api_client():
    """Flask API test client"""
    if not API_AVAILABLE or not api_app:
        pytest.skip("API components not available")

    api_app.config["TESTING"] = True
    with api_app.test_client() as client:
        # Set API key for testing
        client.environ_base["HTTP_X_API_KEY"] = "test-api-key-2025"
        yield client


@pytest.fixture
def web_client():
    """Flask web interface test client"""
    if not WEB_AVAILABLE or not web_app:
        pytest.skip("Web components not available")

    web_app.config["TESTING"] = True
    web_app.config["WTF_CSRF_ENABLED"] = False
    with web_app.test_client() as client:
        yield client


@pytest.fixture
def validation_system():
    """Validation system instance"""
    if not VALIDATORS_AVAILABLE or not ValidationSystem:
        pytest.skip("Validation components not available")

    return ValidationSystem()


@pytest.fixture
def format_converter(temp_dir):
    """Format converter instance"""
    if not CONVERTER_AVAILABLE or not FormatConverter:
        pytest.skip("Converter components not available")

    return FormatConverter(temp_dir)


@pytest.fixture
def tei_generator():
    """TEI generator instance"""
    if not TEI_AVAILABLE or not TEIGenerator:
        pytest.skip("TEI components not available")

    return TEIGenerator()


@pytest.fixture
def uml_generator():
    """UML generator instance"""
    if not UML_AVAILABLE or not UMLGenerator:
        pytest.skip("UML components not available")

    return UMLGenerator()


@pytest.fixture
def web_scraper():
    """Web scraper instance"""
    if not SCRAPER_AVAILABLE or not WebScraper:
        pytest.skip("Scraper components not available")

    config = {
        "max_pages": 5,
        "timeout": 30,
        "rate_limit": 1.0,
        "respect_robots": True,
        "user_agent": "Integral-Philosophy-Test/1.0",
    }
    return WebScraper(config)


@pytest.fixture
def content_pipeline(temp_dir):
    """Content pipeline instance"""
    if not PIPELINE_AVAILABLE or not ContentPipeline:
        pytest.skip("Pipeline components not available")

    config = {
        "scraping": {"depth": 2, "max_pages": 5, "timeout": 30},
        "processing": {"generate_tei": True, "generate_uml": True},
        "output": {"formats": ["html", "tei"], "directory": str(temp_dir)},
    }
    return ContentPipeline(config)


@pytest.fixture
def mock_responses():
    """Mock HTTP responses for testing"""

    class MockResponses:
        def __init__(self):
            self.responses = {}
            self.call_count = {}

        def add(self, url, response_data, status_code=200):
            self.responses[url] = {"data": response_data, "status_code": status_code}
            self.call_count[url] = 0

        def get(self, url, **kwargs):
            if url in self.responses:
                self.call_count[url] += 1
                response = Mock()
                response.status_code = self.responses[url]["status_code"]
                response.json.return_value = self.responses[url]["data"]
                response.text = str(self.responses[url]["data"])
                response.headers = {"content-type": "application/json"}
                return response
            else:
                response = Mock()
                response.status_code = 404
                response.raise_for_status.side_effect = requests.exceptions.HTTPError(
                    "404 Not Found"
                )
                return response

    return MockResponses()


@pytest.fixture
def mock_file_structure(temp_dir):
    """Create mock file structure for testing"""
    # Create sample files
    sample_files = {
        "sample.md": "# Sample Markdown\n\nContent here.",
        "sample.html": "<html><body><h1>Sample HTML</h1><p>Content here.</p></body></html>",
        "sample.tex": r"\documentclass{article}\begin{document}\title{Sample}\end{document}",
        "sample.txt": "Plain text content.",
        "config.yaml": "test: value\nversion: 1.0",
        "metadata.json": json.dumps({"title": "Test", "author": "Test Author"}),
    }

    created_files = {}
    for filename, content in sample_files.items():
        file_path = temp_dir / filename
        file_path.write_text(content, encoding="utf-8")
        created_files[filename] = file_path

    return created_files


@pytest.fixture(scope="function")
def cleanup_jobs():
    """Cleanup job status after tests"""
    yield

    # Clean up job status dictionaries
    if "job_status" in globals():
        job_status.clear()
    if "web_job_status" in globals():
        web_job_status.clear()


@pytest.fixture
def performance_metrics():
    """Collect performance metrics during tests"""

    class MetricsCollector:
        def __init__(self):
            self.start_time = None
            self.end_time = None
            self.memory_usage = []
            self.response_times = []

        def start_timer(self):
            self.start_time = time.time()

        def stop_timer(self):
            self.end_time = time.time()
            return self.end_time - self.start_time

        def add_response_time(self, response_time):
            self.response_times.append(response_time)

        def add_memory_usage(self, memory_mb):
            self.memory_usage.append(memory_mb)

        def get_summary(self):
            return {
                "total_time": self.end_time - self.start_time
                if self.end_time and self.start_time
                else 0,
                "avg_response_time": sum(self.response_times) / len(self.response_times)
                if self.response_times
                else 0,
                "max_response_time": max(self.response_times)
                if self.response_times
                else 0,
                "avg_memory_mb": sum(self.memory_usage) / len(self.memory_usage)
                if self.memory_usage
                else 0,
                "max_memory_mb": max(self.memory_usage) if self.memory_usage else 0,
            }

    return MetricsCollector()


# Pytest configuration
def pytest_configure(config):
    """Configure pytest with custom markers"""
    config.addinivalue_line("markers", "unit: mark test as a unit test")
    config.addinivalue_line("markers", "integration: mark test as an integration test")
    config.addinivalue_line("markers", "functional: mark test as a functional test")
    config.addinivalue_line("markers", "e2e: mark test as an end-to-end test")
    config.addinivalue_line("markers", "performance: mark test as a performance test")
    config.addinivalue_line("markers", "security: mark test as a security test")
    config.addinivalue_line("markers", "slow: mark test as slow running")
    config.addinivalue_line(
        "markers", "selenium: mark test as requiring Selenium WebDriver"
    )
    config.addinivalue_line(
        "markers", "requires_components: mark test as requiring all components"
    )


def pytest_collection_modifyitems(config, items):
    """Modify test collection based on component availability"""
    if not COMPONENTS_AVAILABLE:
        # Skip tests that require components if they're not available
        skip_missing_components = pytest.mark.skip(reason="Components not available")
        for item in items:
            if "requires_components" in item.keywords:
                item.add_marker(skip_missing_components)

    # Skip API tests if API not available
    if not API_AVAILABLE:
        skip_api = pytest.mark.skip(reason="API components not available")
        for item in items:
            if "api" in item.keywords or item.nodeid.startswith("tests/api/"):
                item.add_marker(skip_api)

    # Skip Selenium tests if selenium not available
    try:
        import selenium
    except ImportError:
        skip_selenium = pytest.mark.skip(reason="Selenium not available")
        for item in items:
            if "selenium" in item.keywords:
                item.add_marker(skip_selenium)


# Test data generators
@pytest.fixture
def generate_test_document():
    """Generate test documents with different parameters"""

    def generator(title="Test Document", content_type="markdown", complexity="simple"):
        if complexity == "simple":
            if content_type == "markdown":
                return f"# {title}\n\nSimple content for testing."
            elif content_type == "html":
                return f"<html><body><h1>{title}</h1><p>Simple content for testing.</p></body></html>"
            elif content_type == "latex":
                return f"""\\documentclass{{article}}
\\title{{{title}}}
\\begin{{document}}
\\maketitle
Simple content for testing.
\\end{{document}}"""

        elif complexity == "complex":
            if content_type == "markdown":
                return f"""# {title}

## Introduction

This is a **complex** document with *multiple* elements.

### Features

- List item 1
- List item 2
- List item 3

> "This is a quote"
> 
> - Author

```python
def example():
    return "complex code"
```

![Image](image.jpg)

[Link](https://example.com)"""

            elif content_type == "html":
                return f"""<!DOCTYPE html>
<html>
<head><title>{title}</title></head>
<body>
<h1>{title}</h1>
<h2>Introduction</h2>
<p>This is a <strong>complex</strong> document with <em>multiple</em> elements.</p>
<h3>Features</h3>
<ul>
<li>List item 1</li>
<li>List item 2</li>
<li>List item 3</li>
</ul>
<blockquote><p>&quot;This is a quote&quot;</p><p>- Author</p></blockquote>
<pre><code>def example():
    return "complex code"</code></pre>
<img src="image.jpg" alt="Image">
<a href="https://example.com">Link</a>
</body>
</html>"""

        return f"Generated {content_type} document: {title}"

    return generator
