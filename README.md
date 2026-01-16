# 🌟 Integral Philosophy Publishing System

**Modular Academic Content Processing Platform** - Split into focused subprojects

## 📦 Project Structure

This is the **main project** that orchestrates all subprojects:

```
integral-philosophy/
├── 🧠 subprojects/integral-philosophy-core/     # Core processing engine
├── 🌐 subprojects/integral-philosophy-web/      # Web interface & API
├── 🛠️ subprojects/integral-philosophy-cli/     # Command-line tools
├── 📚 subprojects/integral-philosophy-docs/    # Documentation
├── 📚 subprojects/integral-philosophy-content/ # Sample content
├── ⚙️ subprojects/integral-philosophy-config/ # Configuration
├── 🚀 subprojects/integral-philosophy-deploy/  # Deployment
└── 🧪 subprojects/integral-philosophy-tests/  # Testing suite
```

## 🚀 Quick Start

### 🔄 Setup with Submodules
\`\`\`bash
# Clone with submodules
git clone --recursive https://github.com/dominicusin/integral-philosophy.git
cd integral-philosophy

# Or initialize submodules manually
git submodule update --init --recursive
\`\`\`

### 🌟 Install Complete System
\`\`\`bash
# Setup all subprojects
./setup-all.sh

# Start complete system
./start-full.sh
\`\`\`

## 📦 Individual Subproject Usage

### 🧠 Core Engine
\`\`\`bash
cd subprojects/integral-philosophy-core
pip install -e ".[dev]"
integral-core parse content.md --format md
\`\`\`

### 🌐 Web Interface  
\`\`\`bash
cd subprojects/integral-philosophy-web
pip install -e ".[dev]"
integral-web --port 8000
\`\`\`

### 🛠️ CLI Tools
\`\`\`bash
cd subprojects/integral-philosophy-cli
pip install -e ".[dev]"
integral-publisher process https://example.com --output ./results
\`\`\`

## 🏗️ Architecture

### 🔄 Communication Between Subprojects

```
🌐 Web Interface → 🔌 API Gateway → 🧠 Core Engine → 📚 Output
     │                    │                    │               │
  • React UI         • FastAPI          • Content Parsers   • HTML
  • WebSocket         • Background Jobs    • Format Converters  • PDF  
  • Live Updates      • Rate Limiting     • Content Validators • EPUB
  • File Management    • JWT Auth         • Content Generators • TEI XML
```

## 🔧 Development Workflow

### 🛠️ Individual Subproject Development
\`\`\`bash
# Work on specific subproject
cd subprojects/integral-philosophy-core
git checkout -b feature/new-parser
npm run dev

# Test integration
cd ../integral-philosophy-tests
pytest tests/integration/
\`\`\`

### 🔄 Cross-Subproject Testing
\`\`\`bash
# Full integration tests
./run-integration-tests.sh

# Docker development
docker-compose -f docker-compose.dev.yml up
\`\`\`

## 📦 Installation Options

### 🎯 Complete Installation
\`\`\`bash
# Install all subprojects
./install-all.sh

# Or install specific components
./install-core.sh       # Core engine only
./install-web.sh        # Web interface only
./install-cli.sh         # CLI tools only
\`\`\`

### 🐳 Docker Deployment
\`\`\`bash
# Complete system
docker-compose up -d

# Individual services
docker-compose up -d web api core
\`\`\`

## 🚀 Production Deployment

### ☸️ Kubernetes
\`\`\`bash
# Deploy all services
kubectl apply -f deploy/kubernetes/

# Individual services
kubectl apply -f deploy/kubernetes/web/
kubectl apply -f deploy/kubernetes/api/
\`\`\`

### 🐳 Docker Swarm
\`\`\`bash
docker stack deploy -c docker-compose.yml integral-philosophy
\`\`\`

## 📚 Documentation

- **[User Guide](docs/user-guide.md)** - Complete user documentation
- **[Developer Guide](docs/developer-guide.md)** - Architecture and development
- **[API Reference](docs/api-reference.md)** - Full API documentation
- **[Deployment Guide](docs/deployment-guide.md)** - Production deployment

## 🧪 Testing

- **[Test Suite](tests/README.md)** - Testing documentation
- **[CI/CD](.github/workflows/)** - GitHub Actions workflows
- **[Quality Gates](tests/quality/)** - Code quality checks

## 📦 Subproject Details

### 🧠 integral-philosophy-core
- **Purpose**: Core content processing engine
- **Language**: Python 3.8+
- **Dependencies**: BeautifulSoup, Pandoc, Pandas
- **Tests**: pytest, coverage
- **CI**: GitHub Actions

### 🌐 integral-philosophy-web  
- **Purpose**: Web interface and REST API
- **Technologies**: FastAPI, React, WebSocket
- **Features**: Real-time updates, file management
- **Tests**: pytest, Playwright
- **CI**: GitHub Actions

### 🛠️ integral-philosophy-cli
- **Purpose**: Command-line interface tools
- **Features**: Unified CLI, batch processing
- **Compatibility**: Works with core engine
- **Tests**: pytest, integration tests

## 🔧 Contributing

### 🎯 Development Workflow
1. Choose subproject to work on
2. Create feature branch
3. Make changes with tests
4. Submit pull request

### 🔄 Integration Guidelines
- Use semantic versioning
- Follow project coding standards
- Update cross-project tests
- Document breaking changes

## 📄 License

MIT License - see [LICENSE](LICENSE) file

---

## 🌟 What's New in v2.0.0

- ✨ **Modular Architecture** - Split into 8 focused subprojects
- 🔧 **Better Separation** - Clear boundaries and dependencies
- 🚀 **Easier Development** - Independent development cycles
- 📦 **Flexible Deployment** - Deploy individual components
- 🔄 **Improved Testing** - Focused testing for each subproject

**🚀 Integral Philosophy Publishing System - Now Modular and Production-Ready!**
