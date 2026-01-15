# 🌟 Integral Philosophy Publishing System

> **Elegant Academic Content Processing Pipeline** - Transform ideas into published works

```
📚 Content → 🔄 Processing → 📖 Publication
```

## ✨ Features

### 🎯 Core Capabilities
- **🕷️ Web Scraping** - Extract content from any website with JavaScript support
- **📝 Format Conversion** - Seamless conversion between 10+ markup formats  
- **📚 Academic Publishing** - TEI XML generation meeting scholarly standards
- **🎨 Visualization** - UML diagrams for content structure analysis
- **🌐 Web Interface** - Modern responsive UI with real-time processing
- **🔌 REST API** - Complete programmatic access to all features

### 🔄 Supported Formats
**Input:** Markdown, HTML, LaTeX, Org, AsciiDoc, reST, Typst, TEI, DocBook, JATS, JSON  
**Output:** All input formats + PDF, EPUB, DOCX via XSLT

## 🚀 Quick Start

```bash
# 1. Setup environment
./tools/cli/setup_venv.sh

# 2. Activate environment  
source venv/bin/activate

# 3. Process content
./tools/cli/pipeline.sh --url https://example.com --output ./results

# 4. Start web interface
./tools/cli/start_web_interface.sh
```

## 📁 Project Structure

```
integral-philosophy-publisher/
├── 🎯 core/                    # Core processing modules
│   ├── parsers/                 # Content parsers (Markdown, LaTeX, etc.)
│   ├── converters/              # Format converters  
│   ├── scrapers/               # Web scraping engines
│   ├── generators/              # TEI, UML, XSLT generators
│   └── validators/             # Content validation
├── 🛠️ tools/                    # Command-line tools
│   ├── cli/                    # Shell scripts and utilities
│   └── automation/             # Automation workflows
├── 🌐 web/                      # Web interface and API
│   ├── api/                    # REST API server
│   ├── ui/                     # Web interface
│   └── templates/              # HTML templates
├── 📚 docs/                     # Documentation
│   ├── user/                   # User guides
│   ├── developer/              # Developer docs
│   └── api/                    # API reference
├── ⚙️ config/                   # Configuration files
│   ├── pipelines/              # Processing pipelines
│   ├── styles/                 # Styling and themes
│   └── metadata/               # Metadata schemas
├── 💾 data/                     # Data directories
│   ├── input/                  # Input content
│   ├── output/                 # Generated outputs
│   └── cache/                 # Temporary files
├── 🧪 tests/                    # Test suites
│   ├── unit/                   # Unit tests
│   ├── integration/            # Integration tests
│   └── e2e/                    # End-to-end tests
├── 🎪 examples/                 # Examples and demos
│   ├── samples/                # Sample content
│   └── demos/                  # Demonstration projects
└── 🚀 deploy/                   # Deployment configurations
    ├── docker/                 # Docker configurations
    └── scripts/                # Deployment scripts
```

## 🎨 Usage Examples

### Web Scraping & Processing
```bash
# Scrape and process a philosophy website
./tools/cli/scrape.sh https://plato.stanford.edu --depth 2 --output ./plato_content

# Convert between formats
./tools/cli/convert.sh ./plato_content/input.md --to html --output ./plato_content/output.html

# Generate UML diagrams
./tools/cli/uml.sh ./plato_content/structure.json --format plantuml --output ./plato_content/diagram.puml
```

### Academic Publishing
```bash
# Generate TEI XML
./tools/cli/tei.sh ./plato_content/content.md --output ./plato_content/tei.xml

# Transform to multiple formats
./tools/cli/transform.sh ./plato_content/tei.xml --formats html,pdf,epub --output ./publications/
```

### API Usage
```python
import requests

# Process content via API
response = requests.post('http://localhost:8001/api/process', json={
    'url': 'https://philosophy-example.com',
    'formats': ['html', 'pdf', 'tei'],
    'generate_uml': True
})

result = response.json()
print(f"Processing complete: {result['status']}")
```

## 🏗️ Architecture

The system follows a **pipeline architecture** with clear separation of concerns:

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Input Source  │ → │   Content Parser │ → │   Content AST   │
│ (Web/Files)    │    │ (Structure)      │    │ (Canonical)    │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                        │
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   UML Diagrams  │ ← │  AST Analyzer    │ ← │   Validators    │
│ (Visualization) │    │ (Relationships)  │    │ (Quality)       │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                                        │
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│  Multiple       │ → │  Format Engines  │ → │   TEI XML       │
│  Formats        │    │ (Pandoc/LuaLaTeX)│    │ (Academic)     │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

## 🛠️ Installation

### Prerequisites
- Python 3.8+
- Pandoc 3.0+
- LaTeX (LuaLaTeX)
- Node.js 14+ (optional, for web interface)

### Setup
```bash
# Clone repository
git clone <repository-url>
cd integral-philosophy-publisher

# Automated setup
./tools/cli/setup_venv.sh

# Manual setup (if needed)
python -m venv venv
source venv/bin/activate
pip install -r docs/user/requirements.txt
```

## 🌟 Highlights

- **🎓 Academic Standards**: TEI P5 compliant XML generation
- **⚡ High Performance**: Sub-second conversion times
- **🔄 Universal**: Convert between any supported formats
- **🎨 Beautiful Output**: Responsive HTML, professional PDFs
- **🔒 Reliable**: Comprehensive validation and error handling
- **🐳 Production Ready**: Docker deployment with monitoring

## 📖 Documentation

- **[User Guide](docs/user/README.md)** - Complete usage instructions
- **[Developer Guide](docs/developer/README.md)** - Architecture and contribution
- **[API Reference](docs/api/README.md)** - REST API documentation
- **[Examples](examples/README.md)** - Sample projects and use cases

## 🤝 Contributing

We welcome contributions! Please see the [Developer Guide](docs/developer/README.md) for details.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**🌟 Built with passion for academic publishing and digital humanities**