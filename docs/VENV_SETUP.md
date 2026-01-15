# Virtual Environment Setup Guide

## 🎯 Overview

The Integral Philosophy Publishing System now includes a comprehensive virtual environment setup with all necessary dependencies. This ensures isolated, reproducible installations and system compatibility.

## 🚀 Quick Start

### Option 1: Automatic Setup
```bash
# Clone and setup everything automatically
./setup_venv.sh
```

### Option 2: Manual Setup
```bash
# Create virtual environment
python3 -m venv venv

# Activate environment
source venv/bin/activate

# Install core dependencies
pip install selenium lxml pyyaml beautifulsoup4 html2text requests markdown
```

## 📦 Dependencies

### Core Dependencies
- **selenium** >= 4.39.0 - Web automation and JavaScript execution
- **lxml** >= 6.0.2 - XML processing and XSLT transformations
- **pyyaml** >= 6.0.1 - YAML configuration and parsing
- **beautifulsoup4** >= 4.14.3 - HTML parsing and cleanup
- **html2text** >= 2024.4.15 - HTML to markdown conversion
- **requests** >= 2.32.5 - HTTP client for web scraping
- **markdown** >= 3.10 - Markdown processing

### Extended Dependencies
- **pypandoc** - Enhanced Pandoc interface
- **python-docx** - DOCX file generation
- **ebooklib** - EPUB ebook creation
- **matplotlib** - Data visualization
- **plotly** - Interactive plotting
- **networkx** - Graph analysis
- **jupyter** - Interactive notebooks
- **Pillow** - Image processing

### Development Dependencies
- **pytest** - Testing framework
- **black** - Code formatting
- **flake8** - Linting
- **mypy** - Type checking

## 🗂️ System Dependencies

### Required for Full Functionality
```bash
# Ubuntu/Debian
sudo apt-get install -y pandoc lualatex tidy saxon-xslt libxml2-utils graphviz

# macOS (with Homebrew)
brew install pandoc lualatex tidy saxon libxml2 graphviz

# CentOS/RHEL/Fedora
sudo dnf install -y pandoc lualatex tidy saxon-xslt libxml2-utils graphviz
```

### Optional
- **chromedriver** - Chrome WebDriver (alternative to system Chromium)
- **Jupyter** - Interactive notebooks (for development)

## 📁 Project Structure After Setup

```
Integral Philosophy Publishing System/
├── venv/                          # Python virtual environment
│   ├── bin/                         # Python executable
│   ├── lib/                         # Installed packages
│   └── include/                    # C headers (if compiled)
├── scripts/                         # Python components
│   ├── web_scraper.py              # Web scraping with Selenium
│   ├── markdowntex_parser.py       # Markdown+TeX parsing
│   ├── ast_to_uml.py              # UML generation
│   ├── tei_generator.py             # TEI XML generation
│   ├── xslt_transformer.py          # XSLT transformations
│   ├── html_tei_converter.py       # HTML↔TEI conversion
│   ├── format_converter.py         # Multi-format converter
│   └── content_pipeline.py        # Master pipeline
├── *.sh                            # Bash interface scripts
│   ├── setup_venv.sh               # Venv setup script
│   ├── pipeline.sh                  # Master pipeline
│   ├── convert.sh                   # Format conversion
│   ├── scrape.sh                    # Web scraping
│   ├── uml.sh                      # UML generation
│   ├── tei.sh                       # TEI generation
│   └── transform.sh                 # XSLT transformations
├── requirements.txt                  # Python dependencies
├── config.json                      # Configuration file
├── activate.sh                       # Activation script
├── activate.bat                     # Windows activation
├── activate.fish                     # Fish shell activation
├── quick_start.sh                    # Quick start script
├── dev_env.sh                       # Development environment
├── .gitignore                        # Git ignore rules
├── venv/                           # Virtual environment
├── content_pipeline/                # Pipeline output
├── format_conversion/               # Conversion output
├── examples/                        # Example files
└── docs/                           # Documentation
```

## 🔧 Usage Examples

### Basic Conversion
```bash
# Activate environment
source venv/bin/activate

# Convert markdown to HTML
./convert.sh document.md -f html

# Convert with chain
./convert.sh document.md --chain org asciidoc rst

# Convert to LaTeX and PDF
./convert.sh document.md -f latex
lualatex document.tex
```

### Web Scraping
```bash
# Activate environment
source venv/bin/activate

# Scrape entire website
./scrape.sh https://philosophy-site.com -p 50

# Generate UML diagrams
./uml.sh scraped_content/site_ast.json -o diagrams -f all

# Generate TEI XML
./tei.sh scraped_content/site_ast.json

# Transform to multiple formats
./transform.sh tei_document.xml -o output -f all
```

### Master Pipeline
```bash
# Full website processing
source venv/bin/activate
./pipeline.sh https://example.com -o project_output -p 100
```

## 🐛 Shell Integration

### Bash/Zsh
```bash
# Activate environment
source venv/bin/activate

# Quick activation
./activate.sh

# Development mode
./dev_env.sh
```

### Fish Shell
```fish
source venv/bin/activate.fish
```

### Windows (Command Prompt)
```cmd
# Activate environment
venv\Scripts\activate.bat

# Quick activation
activate.bat
```

## 🧪 Development Workflow

### Testing
```bash
# Run tests
source venv/bin/activate
pytest tests/

# Type checking
mypy scripts/

# Code formatting
black scripts/

# Linting
flake8 scripts/
```

### Documentation
```bash
# Generate documentation
source venv/bin/activate
python -c "
from scripts.content_pipeline import ContentPipeline
pipeline = ContentPipeline()
report = pipeline.generate_report()
print(f'Documentation generated: {pipeline.results}')
"
```

## 🔍 Configuration

### Environment Variables
```bash
# Configuration file: config.json
cat config.json

# Environment variables
export PYTHONPATH="$PWD:$PYTHONPATH"
export CHROME_BIN="/usr/bin/chromium-browser"
export PANDOC_BIN="/usr/bin/pandoc"
```

### Custom Setup
```bash
# Minimal setup
./setup_venv.sh --minimal

# Development setup
./setup_venv.sh --dev

# Clean setup (remove existing venv)
./setup_env.sh --clean
```

## 🐛️ Troubleshooting

### Common Issues

#### 1. Chrome/Selenium Issues
```bash
# Check Chrome installation
which chromium-browser
which chromedriver

# Use system Chrome
export CHROME_BIN=$(which chromium-browser)

# Install ChromeDriver
sudo apt-get install chromium-chromedriver
# or
brew install chromedriver
```

#### 2. LaTeX Compilation Issues
```bash
# Check LaTeX installation
which lualatex
which pdflatex

# Install TeX Live
sudo apt-get install texlive-full

# Alternative: Use pandoc for PDF
pandoc document.md -f pdf -o document.pdf
```

#### 3. System Dependencies
```bash
# Check system dependencies
which pandoc lualatex tidy xmllint graphviz

# Install missing packages
sudo apt-get install pandoc lualatex tidy saxon-xslt libxml2-utils graphviz

# Check installation
pandoc --version
lualatex --version
```

#### 4. Python Issues
```bash
# Check Python version
python3 --version

# Rebuild venv
rm -rf venv
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

#### 5. Permission Issues
```bash
# Fix permissions
chmod +x *.sh

# Virtual environment permissions
chmod +x venv/bin/activate
chmod +x venv/bin/pip
```

### Environment Conflicts
```bash
# Check active Python
which python3
python3 --version

# Deactivate virtual environment
deactivate

# Reactivate environment
source venv/bin/activate
```

## 📊 Performance and Optimization

### Memory Usage
```bash
# Monitor memory usage during scraping
source venv/bin/activate
./scrape.sh https://large-site.com -p 1000 --low-memory

# Monitor process
top -p python
```

### Parallel Processing
```bash
# Convert multiple files in parallel
source venv/bin/activate
for file in *.md; do
    ./convert.sh "$file" -f html &
done
wait  # Wait for all conversions
```

### Caching
```bash
# Clear cache
rm -rf __pycache__/
find . -name "*.pyc" -delete

# Disable pip cache
pip install --no-cache package
```

## 🔒 Security Considerations

### Isolated Environment
- ✅ All dependencies in virtual environment
- ✅ No system package conflicts
- ✅ Reproducible builds
- ✅ Clean uninstall

### Safe Web Scraping
```bash
# Respect robots.txt
./scrape.sh https://site.com --respect-robots

# Rate limiting
./scrape.sh https://site.com -d 2.0 --max-pages 100

# User agent customization
export SCRAPER_USER="MyBot/1.0"
```

## 🚀 Production Deployment

### Docker Integration
```dockerfile
FROM python:3.9-slim

WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .
CMD ["./pipeline.sh", "$URL", "-o", "output"]
```

### CI/CD Integration
```yaml
name: Build and Test
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.9'
      - name: Install dependencies
        run: |
          python -m venv venv
          source venv/bin/activate
          pip install -r requirements.txt
      - name: Run tests
        run: |
          source venv/bin/activate
          python -m pytest tests/ -v
      - name: Test functionality
        run: |
          source venv/bin/activate
          python scripts/content_pipeline.py --report-only
```

## 📚 Maintenance

### Updating Dependencies
```bash
# Update all packages
source venv/bin/activate
pip install --upgrade -r requirements.txt

# Update specific package
pip install selenium --upgrade

# Rebuild environment
rm -rf venv
./setup_venv.sh
```

### Cleanup
```bash
# Clean cache files
find . -name "__pycache__" -exec rm -rf {}
find . -name "*.pyc" -delete

# Clean output directories
rm -rf content_pipeline/ format_conversion/ scraped_content/

# Clean temporary files
rm -f *.tmp *.temp *.log
```

---

## 🎉 Next Steps

1. **Run setup**: `./setup_venv.sh`
2. **Test functionality**: `./quick_start.sh`
3. **Process your first document**: `./convert.sh your_document.md -f html`
4. **Read documentation**: `docs/CONTENT_PIPELINE.md`
5. **Explore examples**: Check `examples/` directory

The Integral Philosophy Publishing System is now ready with a professional development environment! 🚀