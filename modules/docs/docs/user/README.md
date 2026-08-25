# 🌟 Integral Philosophy Publishing System

> **Transform philosophical ideas into beautiful academic publications**

## 🚀 Quick Start

```bash
# 1. Beautiful setup
./setup.sh

# 2. Process a philosophy website  
python main.py process https://plato.stanford.edu/entries/descartes-epistemology/ --output ./descartes-publication --formats html pdf tei --uml

# 3. Start elegant web interface
python main.py web --port 8000

# 4. Access your beautiful publication
open http://localhost:8000
```

## ✨ What Makes This Beautiful?

### 🎯 **Elegant Simplicity**
- **Single Command** - Go from URL to publication in one step
- **Intuitive Interface** - Beautiful CLI with helpful prompts
- **Smart Defaults** - Works out of the box for academic content

### 🎨 **Beautiful Output**
- **Responsive HTML** - Modern, mobile-friendly design
- **Professional PDF** - Publication-ready LaTeX output  
- **Academic TEI** - Standards-compliant XML
- **Interactive UML** - Visual content structure diagrams

### 🧠 **Intelligent Processing**
- **Content Understanding** - Preserves philosophical concepts
- **Mathematical Formulas** - Perfect equation rendering
- **Citation Handling** - Automatic bibliography management
- **Cross-References** - Maintains academic structure

## 📚 Usage Examples

### Web Scraping & Processing
```bash
# Process Stanford Encyclopedia entry
python main.py process \
  https://plato.stanford.edu/entries/kant-ethics/ \
  --output ./kant-ethics \
  --formats html pdf epub tei \
  --uml
```

### Format Conversion
```bash
# Convert philosophy manuscript to multiple formats
python main.py convert philosophy-manuscript.md --to html pdf epub --output ./publications/
```

### Academic Publishing
```bash
# Generate TEI XML for journal submission
python main.py convert research-paper.md --to tei --output ./journal-submission.xml
```

## 🏗️ Beautiful Architecture

```
📚 Content Sources → 🧠 Processing → 📖 Beautiful Publications
     │                  │                    │
  • Websites         • Parsing          • HTML (Responsive)
  • Documents        • Analysis         • PDF (Publication) 
  • Manuscripts      • Validation       • EPUB (E-book)
                     • Enrichment       • TEI (Academic)
                                      • UML (Visualization)
```

## 🎨 Project Structure

```
integral-philosophy-publisher/
├── 🌟 main.py              # Beautiful main entry point
├── 🛠️ setup.sh             # Elegant setup script
├── 📚 README.md            # This beautiful guide
├── 🎯 core/                # Core processing modules
│   ├── parsers/            # Content intelligence
│   ├── converters/         # Format transformation
│   ├── scrapers/           # Web extraction
│   ├── generators/         # Output creation
│   └── validators/         # Quality assurance
├── 🌐 web/                 # Web interface & API
├── 📖 docs/                # Beautiful documentation
├── ⚙️ config/              # Configuration files
├── 💾 data/                # Content directories
├── 🧪 tests/               # Test suites
├── 🎪 examples/            # Sample projects
└── 🚀 deploy/              # Deployment files
```

## 🌟 Features

### 🕷️ **Intelligent Web Scraping**
- JavaScript rendering for modern websites
- Respectful crawling with rate limiting
- Content extraction and cleaning
- Metadata and bibliography extraction

### 📝 **Universal Format Support**
**10+ Input Formats:** Markdown, HTML, LaTeX, Org, AsciiDoc, reST, Typst, TEI, DocBook, JATS, JSON

**Multiple Output Formats:** HTML, PDF, EPUB, DOCX, LaTeX, TEI XML, UML Diagrams

### 📚 **Academic Excellence**
- TEI P5 compliant XML generation
- LaTeX mathematical formula preservation
- Citation and bibliography management
- Cross-reference maintenance
- Academic metadata handling

### 🎨 **Beautiful Visualization**
- PlantUML structure diagrams
- Mermaid.js web-based visualization  
- Graphviz advanced diagrams
- Content relationship mapping
- Hierarchical structure analysis

### 🌐 **Modern Web Interface**
- Responsive design with beautiful gradients
- Real-time processing progress
- Drag-and-drop file uploads
- Live status updates
- Download management

### 🔌 **Complete REST API**
- All pipeline functionality programmatically accessible
- Background job processing
- Status tracking and monitoring
- Rate limiting and security
- JSON request/response format

## 🛠️ Installation

### Automated Setup (Recommended)
```bash
git clone <repository-url>
cd integral-philosophy-publisher
./setup.sh
```

### Manual Setup
```bash
python3 -m venv venv
source venv/bin/activate
pip install -r docs/user/requirements.txt
```

## 🎯 Beautiful Examples

### Philosophy Article Processing
```bash
# Process complete philosophy article
python main.py process \
  https://philosophy-example.com/articles/integral-consciousness \
  --output ./integral-consciousness-publication \
  --formats html pdf epub tei \
  --uml \
  --depth 2
```

### Academic Journal Submission
```bash
# Prepare journal submission package
python main.py convert \
  manuscript.md \
  --to tei \
  --output ./journal-submission.xml

# Generate multiple publication formats
python main.py process \
  ./journal-submission.xml \
  --output ./submission-package \
  --formats html pdf docx
```

### Course Material Preparation
```bash
# Convert course materials for online delivery
python main.py convert \
  course-syllabus.md \
  --to html \
  --output ./online-course/

# Generate supplementary materials
python main.py convert \
  course-readings/ \
  --to epub \
  --output ./e-book-version/
```

## 📖 Documentation

- **[User Guide](docs/user/README.md)** - Complete usage instructions
- **[Developer Guide](docs/developer/README.md)** - Architecture and contribution
- **[API Reference](docs/api/README.md)** - REST API documentation
- **[Examples](examples/README.md)** - Sample projects and workflows

## 🌟 Why This System?

### **For Philosophers**
- Focus on content, not formatting
- Preserve complex philosophical concepts
- Generate publication-ready outputs
- Maintain academic standards

### **For Academic Publishers**  
- Streamline submission processing
- Automated format conversion
- Quality validation
- Multiple output formats

### **For Digital Humanities**
- TEI XML compliance
- Metadata preservation
- Visualization tools
- API integration

## 🚀 Production Deployment

```bash
# Docker deployment
docker-compose up -d

# Access services
# Web Interface: http://localhost:8000
# API Server: http://localhost:8001
# Monitoring: http://localhost:3000
```

## 🤝 Contributing

We welcome contributions! See the [Developer Guide](docs/developer/README.md) for details.

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

**🌟 Built with passion for philosophy and academic excellence** 🌟

> *"Philosophy begins in wonder."* - Socrates