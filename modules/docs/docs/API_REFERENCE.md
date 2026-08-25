# API Reference

## 📚 Complete API Documentation

This document provides comprehensive API documentation for all scripts and components of the Integral Philosophy Publishing System.

---

## 🎮 Bash Script APIs

### `pipeline.sh` - Master Pipeline Controller

**Purpose**: Orchestrate the complete content processing pipeline

**Usage**:
```bash
./pipeline.sh [OPTIONS]
```

**Options**:
- `--url <URL>` - Website URL to process
- `--input <PATH>` - Local input directory or file
- `--output <PATH>` - Output directory (default: `./output`)
- `--formats <FORMATS>` - Comma-separated output formats
- `--depth <N>` - Scraping depth (default: 3)
- `--include-uml` - Generate UML diagrams
- `--tei-canonical` - Create TEI canonical storage
- `--js-render` - Enable JavaScript rendering
- `--parallel <N>` - Parallel processing threads
- `--cache-dir <PATH>` - Cache directory for scraping
- `--config <PATH>` - Configuration file path

**Examples**:
```bash
# Process website to all formats
./pipeline.sh --url https://philosophy-site.com --output ./processed --formats html,pdf,epub

# Local processing with UML
./pipeline.sh --input ./articles --tei-canonical --include-uml

# High-performance scraping
./pipeline.sh --url https://large-site.com --depth 5 --parallel 8 --cache-dir ./cache
```

---

### `scrape.sh` - Web Scraping Interface

**Purpose**: Extract content from websites with JavaScript support

**Usage**:
```bash
./scrape.sh [OPTIONS]
```

**Options**:
- `--url <URL>` - Target website URL
- `--output <PATH>` - Output directory (default: `./scraped`)
- `--depth <N>` - Maximum recursion depth (default: 3)
- `--recursive` - Enable recursive link following
- `--js-render` - Enable JavaScript rendering
- `--js-timeout <N>` - JavaScript timeout in seconds (default: 30)
- `--user-agent <STRING>` - Custom user agent
- `--extract-metadata` - Extract structured metadata
- `--respect-robots` - Respect robots.txt (default: true)
- `--rate-limit <N>` - Delay between requests in seconds (default: 1)
- `--cache-dir <PATH>` - Cache directory
- `--exclude <PATTERN>` - URL patterns to exclude
- `--include <PATTERN>` - URL patterns to include

**Return Values**:
- `0` - Success
- `1` - General error
- `2` - Network error
- `3` - JavaScript rendering failed

**Examples**:
```bash
# Basic scraping
./scrape.sh --url https://philosophy-website.com

# JavaScript-heavy site
./scrape.sh --url https://dynamic-site.com --js-render --js-timeout 60

# Advanced scraping with filtering
./scrape.sh --url https://example.com --recursive --depth 5 --exclude "\.pdf$" --rate-limit 2
```

---

### `convert.sh` - Format Conversion Interface

**Purpose**: Convert between 10+ markup formats

**Usage**:
```bash
./convert.sh [OPTIONS]
```

**Options**:
- `--input <PATH>` - Input file or directory
- `--input-dir <PATH>` - Input directory for batch processing
- `--output <PATH>` - Output file or directory
- `--to <FORMAT>` - Target format
- `--from <FORMAT>` - Source format (auto-detected if not specified)
- `--chain <FORMATS>` - Chain conversion through multiple formats
- `--preserve-math` - Preserve mathematical formulas
- `--extract-links` - Extract and preserve link structure
- `--batch` - Enable batch processing
- `--parallel <N>` - Parallel processing threads
- `--test` - Run self-test
- `--test-all` - Run comprehensive tests

**Supported Formats**:
- `markdown` / `md`
- `html` / `htm`
- `latex` / `tex`
- `org`
- `asciidoc` / `adoc`
- `rst` / `rest`
- `typst` / `typ`
- `tei` / `xml`
- `json` / `jsonld`
- `docx`
- `epub`
- `pdf`

**Examples**:
```bash
# Single conversion
./convert.sh --input article.md --to html

# Chain conversion
./convert.sh --input article.md --chain org,asciidoc --to rst

# Batch processing
./convert.sh --input-dir ./articles --to pdf --output-dir ./pdfs

# Test conversions
./convert.sh --test-all
```

---

### `uml.sh` - UML Diagram Generation

**Purpose**: Generate UML diagrams from content structure

**Usage**:
```bash
./uml.sh [OPTIONS]
```

**Options**:
- `--input <PATH>` - Input file or directory
- `--output <PATH>` - Output file (default: auto-generated)
- `--format <FORMAT>` - UML format (plantuml, mermaid, dot, all)
- `--type <TYPE>` - Diagram type (structure, flowchart, class, sequence)
- `--theme <THEME>` - Visual theme (default, dark, academic)
- `--include-content` - Include content snippets in diagrams
- `--max-depth <N>` - Maximum nesting depth
- `--filter <PATTERN>` - Filter nodes by pattern
- `--style <STYLE>` - Styling options (compact, detailed, minimal)

**Formats**:
- `plantuml` - PlantUML format
- `mermaid` - Mermaid.js format  
- `dot` - Graphviz DOT format
- `all` - Generate all formats

**Examples**:
```bash
# Generate PlantUML diagram
./uml.sh --input site.md --format plantuml

# Generate all formats with dark theme
./uml.sh --input site.md --format all --theme dark

# Detailed structure diagram
./uml.sh --input site.md --type structure --style detailed --include-content
```

---

### `tei.sh` - TEI XML Generation

**Purpose**: Generate academic-standard TEI XML

**Usage**:
```bash
./tei.sh [OPTIONS]
```

**Options**:
- `--input <PATH>` - Input file or directory
- `--output <PATH>` - Output TEI XML file
- `--bibliography <PATH>` - Bibliography file (BibTeX)
- `--citations <STYLE>` - Citation style (ieee, apa, mla, chicago)
- `--metadata <JSON>` - Additional metadata as JSON
- `--validate` - Validate TEI XML output
- `--schema <PATH>` - Custom TEI schema
- `--encoding <ENCODING>` - XML encoding (default: utf-8)
- `--preserve-entities` - Preserve HTML entities
- `--header-only` - Generate only TEI header
- `--test-validate` - Test TEI validation

**Examples**:
```bash
# Basic TEI generation
./tei.sh --input article.md --output article.tei

# With bibliography
./tei.sh --input paper.md --bibliography refs.bib --citations apa --output paper.tei

# Validate TEI output
./tei.sh --input article.md --validate
```

---

### `transform.sh` - XSLT Transformations

**Purpose**: Transform TEI XML to multiple output formats

**Usage**:
```bash
./transform.sh [OPTIONS]
```

**Options**:
- `--tei <PATH>` - Input TEI XML file
- `--to <FORMAT>` - Output format
- `--output <PATH>` - Output file
- `--xslt <PATH>` - Custom XSLT stylesheet
- `--params <KEY=VALUE>` - XSLT parameters
- `--theme <THEME>` - Visual theme (for HTML/PDF)
- `--style <PATH>` - Custom CSS/stylesheet
- `--images-dir <PATH>` - Images directory
- `--metadata <JSON>` - Additional metadata
- `--optimize` - Optimize output size
- `--validate-xslt` - Validate XSLT before transformation

**Output Formats**:
- `html` - Responsive HTML with SCSS
- `pdf` - PDF via LaTeX
- `epub` - E-book format
- `docx` - Microsoft Word
- `latex` - LaTeX source
- `rst` - reStructuredText
- `markdown` - Markdown

**Examples**:
```bash
# Transform to HTML
./transform.sh --tei document.tei --to html --output document.html

# Transform to PDF with custom theme
./transform.sh --tei document.tei --to pdf --theme academic --output document.pdf

# Custom XSLT transformation
./transform.sh --tei document.tei --xslt custom.xsl --params "param1=value1,param2=value2"
```

---

## 🐍 Python Script APIs

### `scripts/web_scraper.py` - Web Scraping Engine

**Classes**:

#### `WebScraper`
```python
class WebScraper:
    def __init__(self, config: Dict[str, Any] = None)
    def scrape_url(self, url: str, depth: int = 3) -> Dict[str, Any]
    def scrape_recursive(self, urls: List[str], max_depth: int) -> Dict[str, Any]
    def extract_content(self, html: str) -> Dict[str, Any]
    def extract_metadata(self, url: str, html: str) -> Dict[str, Any]
```

**Configuration**:
```python
config = {
    'user_agent': 'Mozilla/5.0...',
    'timeout': 30,
    'js_render': True,
    'js_timeout': 30,
    'respect_robots': True,
    'rate_limit': 1.0,
    'cache_dir': './cache',
    'exclude_patterns': [r'\.pdf$', r'\.jpg$'],
    'include_patterns': [r'.*'],
    'max_depth': 3,
    'max_pages': 1000
}
```

**Examples**:
```python
from scripts.web_scraper import WebScraper

scraper = WebScraper(config)
result = scraper.scrape_url('https://example.com', depth=3)
print(f"Scraped {len(result['pages'])} pages")
```

---

### `scripts/markdowntex_parser.py` - Markdown+TeX Parser

**Classes**:

#### `MarkdownTeXParser`
```python
class MarkdownTeXParser:
    def __init__(self, config: Dict[str, Any] = None)
    def parse(self, content: str) -> 'ASTNode'
    def parse_file(self, filepath: str) -> 'ASTNode'
    def extract_links(self, ast: 'ASTNode') -> List[Dict[str, str]]
    def extract_math(self, ast: 'ASTNode') -> List[str]
```

#### `ASTNode`
```python
@dataclass
class ASTNode:
    type: str
    content: Union[str, List['ASTNode'], None]
    metadata: Dict[str, Any] = field(default_factory=dict)
    children: List['ASTNode'] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]
    def find_by_type(self, node_type: str) -> List['ASTNode']
    def get_text(self) -> str
```

**Examples**:
```python
from scripts.markdowntex_parser import MarkdownTeXParser

parser = MarkdownTeXParser()
ast = parser.parse("# Title\n\nContent with $E=mc^2$ math")
links = parser.extract_links(ast)
math_formulas = parser.extract_math(ast)
```

---

### `scripts/ast_to_uml.py` - UML Generator

**Classes**:

#### `UMLGenerator`
```python
class UMLGenerator:
    def __init__(self, config: Dict[str, Any] = None)
    def generate_plantuml(self, ast: ASTNode) -> str
    def generate_mermaid(self, ast: ASTNode) -> str
    def generate_dot(self, ast: ASTNode) -> str
    def generate_all(self, ast: ASTNode) -> Dict[str, str]
    def analyze_structure(self, ast: ASTNode) -> Dict[str, Any]
```

**Configuration**:
```python
config = {
    'theme': 'default',
    'include_content': False,
    'max_depth': 10,
    'node_style': 'rectangle',
    'edge_style': 'solid',
    'filter_patterns': [],
    'compact_mode': False
}
```

**Examples**:
```python
from scripts.ast_to_uml import UMLGenerator

generator = UMLGenerator({'theme': 'dark'})
plantuml_code = generator.generate_plantuml(ast)
with open('diagram.puml', 'w') as f:
    f.write(plantuml_code)
```

---

### `scripts/tei_generator.py` - TEI XML Generator

**Classes**:

#### `TEIGenerator`
```python
class TEIGenerator:
    def __init__(self, config: Dict[str, Any] = None)
    def generate_tei(self, ast: ASTNode, metadata: Dict = None) -> str
    def generate_header(self, metadata: Dict) -> str
    def generate_body(self, ast: ASTNode) -> str
    def process_bibliography(self, bibtex_file: str) -> str
    def validate_tei(self, tei_xml: str) -> bool
```

**Metadata Schema**:
```python
metadata = {
    'title': 'Document Title',
    'author': 'Author Name',
    'date': '2025-01-15',
    'language': 'en',
    'keywords': ['philosophy', 'mathematics'],
    'abstract': 'Document abstract...',
    'publication': {
        'publisher': 'Publisher Name',
        'venue': 'Journal Name',
        'year': 2025
    }
}
```

**Examples**:
```python
from scripts.tei_generator import TEIGenerator

generator = TEIGenerator()
tei_xml = generator.generate_tei(ast, metadata)
is_valid = generator.validate_tei(tei_xml)
```

---

### `scripts/xslt_transformer.py` - XSLT Transformer

**Classes**:

#### `XSLTTransformer`
```python
class XSLTTransformer:
    def __init__(self, config: Dict[str, Any] = None)
    def transform_to_html(self, tei_xml: str, params: Dict = None) -> str
    def transform_to_pdf(self, tei_xml: str, params: Dict = None) -> bytes
    def transform_to_epub(self, tei_xml: str, params: Dict = None) -> bytes
    def transform_with_custom_xslt(self, tei_xml: str, xslt_path: str, params: Dict = None) -> str
    def optimize_output(self, content: str, format_type: str) -> str
```

**Transformation Parameters**:
```python
params = {
    'theme': 'academic',
    'style': 'compact',
    'include_toc': True,
    'include_references': True,
    'math_format': 'mathjax',
    'link_format': 'relative',
    'image_format': 'webp',
    'optimize_size': True
}
```

**Examples**:
```python
from scripts.xslt_transformer import XSLTTransformer

transformer = XSLTTransformer()
html = transformer.transform_to_html(tei_xml, {'theme': 'dark'})
pdf_bytes = transformer.transform_to_pdf(tei_xml)
```

---

### `scripts/html_tei_converter.py` - HTML↔TEI Converter

**Classes**:

#### `HTMLTEIConverter`
```python
class HTMLTEIConverter:
    def __init__(self, config: Dict[str, Any] = None)
    def html_to_tei(self, html: str) -> str
    def tei_to_html(self, tei: str) -> str
    def test_isomorphism(self, original: str, converted: str) -> Dict[str, Any]
    def extract_structure(self, content: str, content_type: str) -> Dict[str, Any]
    def preserve_math(self, content: str) -> str
```

**Examples**:
```python
from scripts.html_tei_converter import HTMLTEIConverter

converter = HTMLTEIConverter()
tei = converter.html_to_tei(html_content)
back_to_html = converter.tei_to_html(tei)
isomorphism_test = converter.test_isomorphism(html_content, back_to_html)
```

---

### `scripts/format_converter.py` - Universal Format Converter

**Classes**:

#### `FormatConverter`
```python
class FormatConverter:
    def __init__(self, config: Dict[str, Any] = None)
    def convert(self, content: str, from_format: str, to_format: str) -> str
    def convert_chain(self, content: str, format_chain: List[str]) -> str
    def detect_format(self, content: str) -> str
    def get_supported_formats(self) -> Dict[str, List[str]]
    def batch_convert(self, files: List[str], to_format: str) -> Dict[str, str]
```

**Supported Conversions**:
```python
# Direct conversions
converter.convert(markdown, 'markdown', 'html')
converter.convert(html, 'html', 'latex')

# Chain conversions
converter.convert_chain(markdown, ['markdown', 'org', 'asciidoc', 'html'])

# Batch processing
files = ['doc1.md', 'doc2.md', 'doc3.md']
results = converter.batch_convert(files, 'pdf')
```

---

### `scripts/content_pipeline.py` - Master Pipeline

**Classes**:

#### `ContentPipeline`
```python
class ContentPipeline:
    def __init__(self, config: Dict[str, Any] = None)
    def process_website(self, url: str, options: Dict = None) -> Dict[str, Any]
    def process_files(self, input_path: str, options: Dict = None) -> Dict[str, Any]
    def run_full_pipeline(self, input_source: str, options: Dict = None) -> Dict[str, Any]
    def get_pipeline_status(self) -> Dict[str, Any]
    def optimize_performance(self, config: Dict) -> Dict[str, Any]
```

**Pipeline Configuration**:
```python
config = {
    'scraping': {
        'depth': 3,
        'js_render': True,
        'rate_limit': 1.0
    },
    'processing': {
        'generate_tei': True,
        'generate_uml': True,
        'preserve_math': True
    },
    'output': {
        'formats': ['html', 'pdf', 'epub'],
        'theme': 'academic',
        'optimize_size': True
    },
    'performance': {
        'parallel_threads': 4,
        'cache_enabled': True,
        'memory_limit': '4GB'
    }
}
```

**Examples**:
```python
from scripts.content_pipeline import ContentPipeline

pipeline = ContentPipeline(config)
result = pipeline.process_website('https://philosophy-site.com', {
    'output_dir': './processed',
    'formats': ['html', 'pdf', 'tei'],
    'include_uml': True
})

print(f"Processed {result['pages_processed']} pages")
print(f"Generated {len(result['output_files'])} output files")
```

---

## 🔧 Configuration System

### Global Configuration (`config.json`)

```json
{
    "system": {
        "version": "1.0.0",
        "log_level": "INFO",
        "temp_dir": "./tmp",
        "cache_dir": "./cache"
    },
    "scraping": {
        "default_depth": 3,
        "js_render": true,
        "timeout": 30,
        "rate_limit": 1.0,
        "respect_robots": true
    },
    "processing": {
        "preserve_math": true,
        "extract_links": true,
        "generate_metadata": true,
        "validate_output": true
    },
    "output": {
        "default_formats": ["html", "tei"],
        "theme": "default",
        "optimize_size": false,
        "include_toc": true
    },
    "performance": {
        "parallel_threads": 4,
        "memory_limit": "4GB",
        "disk_limit": "10GB"
    }
}
```

### Environment Variables

```bash
# Python environment
export PYTHONPATH="${PYTHONPATH}:$(pwd)/scripts"
export INTEGRAL_CONFIG_PATH="./config.json"

# Performance tuning
export INTEGRAL_THREADS=8
export INTEGRAL_MEMORY="8GB"
export INTEGRAL_CACHE="./cache"

# External tools
export PANDOC_PATH="/usr/bin/pandoc"
export LATEX_ENGINE="lualatex"
export CHROME_PATH="/usr/bin/google-chrome"
```

---

## 🧪 Testing APIs

### Self-Test Functions

All scripts include built-in testing capabilities:

```bash
# Test individual components
./convert.sh --test
./scrape.sh --test
./tei.sh --test-validate
./uml.sh --test-generate

# Comprehensive testing
./convert.sh --test-all
./pipeline.sh --test-full

# Performance testing
./pipeline.sh --benchmark --url https://example.com --iterations 10
```

### Python Unit Tests

```python
# Test individual modules
python -m pytest tests/test_web_scraper.py
python -m pytest tests/test_tei_generator.py
python -m pytest tests/test_format_converter.py

# Test with coverage
python -m pytest --cov=scripts tests/

# Integration tests
python -m pytest tests/test_pipeline_integration.py
```

---

## 📊 Performance Monitoring

### Resource Usage APIs

```python
from scripts.content_pipeline import ContentPipeline

pipeline = ContentPipeline()
status = pipeline.get_pipeline_status()

print(f"Memory usage: {status['memory_mb']} MB")
print(f"CPU usage: {status['cpu_percent']}%")
print(f"Cache hits: {status['cache_hits']}")
print(f"Pages processed: {status['pages_processed']}")
```

### Benchmarking

```bash
# Benchmark scraping performance
./scrape.sh --benchmark --url https://example.com --concurrent 5

# Benchmark conversion performance
./convert.sh --benchmark --input large_document.md --formats html,pdf,epub

# Full pipeline benchmark
./pipeline.sh --benchmark --url https://philosophy-site.com --measure-memory
```

---

## 🚨 Error Handling

### Error Codes

| Code | Component | Description |
|------|-----------|-------------|
| 1-10 | General | System-level errors |
| 11-20 | Scraping | Network, parsing, JavaScript errors |
| 21-30 | Conversion | Format conversion errors |
| 31-40 | TEI | XML validation, schema errors |
| 41-50 | XSLT | Transformation errors |
| 51-60 | Pipeline | Orchestration errors |

### Exception Classes

```python
# Python exceptions
from scripts.web_scraper import ScrapingError
from scripts.format_converter import ConversionError
from scripts.tei_generator import TEIError
from scripts.content_pipeline import PipelineError

try:
    result = pipeline.process_website(url)
except ScrapingError as e:
    print(f"Scraping failed: {e}")
except ConversionError as e:
    print(f"Conversion failed: {e}")
except PipelineError as e:
    print(f"Pipeline error: {e}")
```

---

## 🔗 Integration Examples

### Python Integration

```python
from scripts.content_pipeline import ContentPipeline

# Simple integration
pipeline = ContentPipeline()
result = pipeline.process_website('https://philosophy-site.com')

# Advanced integration with custom config
config = {
    'scraping': {'depth': 5, 'js_render': True},
    'processing': {'generate_uml': True},
    'output': {'formats': ['html', 'pdf', 'tei']}
}
pipeline = ContentPipeline(config)
result = pipeline.run_full_pipeline('https://example.com')
```

### Bash Integration

```bash
#!/bin/bash

# Wrapper script for automated processing
process_philosophy_site() {
    local url=$1
    local output_dir=$2
    
    echo "Processing $url..."
    
    ./pipeline.sh \
        --url "$url" \
        --output "$output_dir" \
        --formats html,pdf,epub,tei \
        --include-uml \
        --tei-canonical
        
    echo "Processing complete. Results in $output_dir"
}

# Usage
process_philosophy_site "https://plato.stanford.edu" "./processed-stanford"
```

### REST API Integration (Future)

```python
# Future REST API client example
import requests

client = PipelineAPI('http://localhost:8000')
job = client.submit_job({
    'type': 'website',
    'source': 'https://philosophy-site.com',
    'options': {
        'formats': ['html', 'pdf'],
        'generate_uml': True
    }
})

result = client.wait_for_completion(job.id)
print(f"Job completed: {result.status}")
```

---

This API reference provides complete documentation for integrating and extending the Integral Philosophy Publishing System. For more detailed implementation examples, see the source code and test files.