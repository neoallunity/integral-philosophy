#!/bin/bash
# Virtual Environment Setup Script
# Usage: ./setup_venv.sh

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_status() {
	local color=$1
	local message=$2
	echo -e "${color}${message}${NC}"
}

show_help() {
	echo "Virtual Environment Setup Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 [options]"
	echo ""
	echo "Options:"
	echo "  -h, --help      Show this help message"
	echo "  --clean         Remove existing venv before setup"
	echo "  --dev           Install development dependencies"
	echo "  --minimal       Install only core dependencies"
	echo ""
}

# Parse arguments
CLEAN=false
DEV=false
MINIMAL=false

while [[ $# -gt 0 ]]; do
	case $1 in
	-h | --help)
		show_help
		exit 0
		;;
	--clean)
		CLEAN=true
		shift
		;;
	--dev)
		DEV=true
		shift
		;;
	--minimal)
		MINIMAL=true
		shift
		;;
	-*)
		echo "Unknown option: $1"
		show_help
		exit 1
		;;
	*)
		echo "Unexpected argument: $1"
		exit 1
		;;
	esac
done

VENV_DIR="venv"

print_status $BLUE "🔧 Setting up virtual environment..."

# Clean existing venv if requested
if [ "$CLEAN" = true ] && [ -d "$VENV_DIR" ]; then
	print_status $YELLOW "🗑️  Removing existing virtual environment..."
	rm -rf "$VENV_DIR"
fi

# Create virtual environment
if [ ! -d "$VENV_DIR" ]; then
	print_status $BLUE "📦 Creating virtual environment..."
	python3 -m venv "$VENV_DIR"
else
	print_status $GREEN "✅ Virtual environment already exists"
fi

# Activate virtual environment
print_status $BLUE "🐍 Activating virtual environment..."
source "$VENV_DIR/bin/activate"

# Upgrade pip
print_status $BLUE "📦 Upgrading pip..."
pip install --upgrade pip setuptools wheel

# Install dependencies based on mode
if [ "$MINIMAL" = true ]; then
	print_status $BLUE "📦 Installing minimal dependencies..."
	pip install selenium lxml pyyaml beautifulsoup4 html2text
elif [ "$DEV" = true ]; then
	print_status $BLUE "📦 Installing development dependencies..."
	pip install -r requirements.txt
else
	print_status $BLUE "📦 Installing production dependencies..."
	# Install core dependencies
	pip install selenium lxml pyyaml beautifulsoup4 html2text requests
	pip install markdown pypandoc python-docx ebooklib
	pip install matplotlib plotly networkx jupyter
	pip install Pillow pytest pytest-asyncio black flake8 mypy
	pip install click rich tqdm
fi

# Install system-level dependencies check
print_status $BLUE "🔍 Checking system dependencies..."

check_system_dep() {
	local dep=$1
	local pkg=$2
	local install_cmd=$3

	if command -v "$dep" &>/dev/null; then
		print_status $GREEN "✅ $dep is available"
		return 0
	else
		print_status $YELLOW "⚠️  $dep is not available"
		print_status $BLUE "   Install with: $install_cmd $pkg"
		return 1
	fi
}

missing_deps=0

# Check system dependencies
if command -v apt-get &>/dev/null; then
	check_system_dep "pandoc" "pandoc" "sudo apt-get install"
	check_system_dep "lualatex" "texlive-luatex" "sudo apt-get install"
	check_system_dep "tidy" "tidy" "sudo apt-get install"
	check_system_dep "xmllint" "libxml2-utils" "sudo apt-get install"
	check_system_dep "graphviz" "graphviz" "sudo apt-get install"
elif command -v brew &>/dev/null; then
	check_system_dep "pandoc" "pandoc" "brew install"
	check_system_dep "lualatex" "texlive" "brew install"
	check_system_dep "tidy" "tidy-html5" "brew install"
	check_system_dep "xmllint" "libxml2" "brew install"
	check_system_dep "graphviz" "graphviz" "brew install"
else
	print_status $YELLOW "⚠️  Could not detect package manager"
	print_status $BLUE "   Please ensure the following are installed:"
	echo "     • pandoc"
	echo "     • lualatex (or pdflatex)"
	echo "     • tidy"
	echo "     • xmllint"
	echo "     • graphviz"
fi

# Create activation scripts
print_status $BLUE "📜 Creating activation scripts..."

# Bash activation script
cat >activate.sh <<'EOF'
#!/bin/bash
# Activate virtual environment
source venv/bin/activate
echo "✅ Virtual environment activated"
echo "🚀 Run scripts: ./pipeline.sh, ./convert.sh, etc."
EOF
chmod +x activate.sh

# Fish activation script
cat >activate.fish <<'EOF'
# Activate virtual environment for fish shell
source venv/bin/activate.fish
echo "✅ Virtual environment activated"
EOF

# Windows activation script (bat)
cat >activate.bat <<'EOF'
@echo off
REM Activate virtual environment for Windows
call venv\Scripts\activate.bat
echo "✅ Virtual environment activated"
EOF

# Create convenience scripts
print_status $BLUE "📝 Creating convenience scripts..."

# Quick start script
cat >quick_start.sh <<'EOF'
#!/bin/bash
# Quick start script for Integral Philosophy Publishing System

echo "🚀 Quick Start - Integral Philosophy Publishing System"
echo ""

# Activate virtual environment
source venv/bin/activate

# Example: Convert a document
echo "📄 Example: Converting document to HTML"
./convert.sh examples/sample.md -f html

echo ""
echo "✅ Ready to use!"
echo "📚 Documentation: ./docs/CONTENT_PIPELINE.md"
EOF
chmod +x quick_start.sh

# Development environment script
cat >dev_env.sh <<'EOF'
#!/bin/bash
# Development environment setup

source venv/bin/activate

echo "🔧 Development Environment"
echo "📦 Installing development dependencies..."
pip install -r requirements.txt

echo "🧪 Running tests..."
python -m pytest tests/ -v

echo "📊 Type checking..."
mypy scripts/

echo "🎨 Code formatting..."
black scripts/

echo "✅ Development environment ready!"
EOF
chmod +x dev_env.sh

# Create .gitignore if not exists
if [ ! -f .gitignore ]; then
	cat >.gitignore <<'EOF'
# Virtual environments
venv/
env/
.venv/

# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
wheels/
*.egg-info/
.installed.cfg
*.egg
MANIFEST

# Testing
.coverage
.pytest_cache/
htmlcov/
.tox/
.nox/

# IDEs
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db

# Project logs and output
*.log
content_pipeline/
scraped_content/
format_conversion/
transformed_output/
test_output/
test_uml_*/
test_transform_*/
test_tei_*.xml

# Temporary files
*.tmp
*.temp
temp/
tmp/

# Generated files
*.pdf
*.html
*.tex
*.adoc
*.org
*.docx
*.epub
site_ast.json
test.md
final_test.*
EOF
	print_status $GREEN "✅ .gitignore created"
fi

# Test installation
print_status $BLUE "🧪 Testing installation..."

if python -c "import selenium, lxml, yaml" 2>/dev/null; then
	print_status $GREEN "✅ Core dependencies working"
else
	print_status $RED "❌ Core dependencies failed"
fi

if python -c "import markdown, requests, beautifulsoup4" 2>/dev/null; then
	print_status $GREEN "✅ Extended dependencies working"
else
	print_status $YELLOW "⚠️  Some extended dependencies failed"
fi

# Create configuration file
print_status $BLUE "⚙️  Creating configuration..."

cat >config.json <<'EOF'
{
    "version": "1.0.0",
    "environment": "development",
    "dependencies": {
        "core": ["selenium", "lxml", "pyyaml", "beautifulsoup4", "html2text"],
        "optional": ["requests", "markdown", "pypandoc", "python-docx", "ebooklib"],
        "development": ["matplotlib", "plotly", "networkx", "jupyter", "pytest", "black", "flake8", "mypy"]
    },
    "system_dependencies": {
        "required": ["pandoc", "lualatex", "tidy", "xmllint", "graphviz"],
        "optional": ["chromedriver", "saxon"]
    },
    "paths": {
        "venv": "venv",
        "scripts": "scripts",
        "docs": "docs",
        "examples": "examples"
    }
}
EOF

# Final message
echo ""
print_status $GREEN "🎉 Virtual environment setup complete!"
echo ""

print_status $BLUE "📋 Next steps:"
echo ""
echo "1. Activate the environment:"
echo "   source venv/bin/activate"
echo "   # Or use: ./activate.sh"
echo ""
echo "2. Quick start:"
echo "   ./quick_start.sh"
echo ""
echo "3. Development mode:"
echo "   ./dev_env.sh"
echo ""
echo "4. Test the system:"
echo "   python -c 'import selenium, lxml; print(\"✅ Dependencies working\")'"
echo ""
echo "📚 Documentation: ./docs/CONTENT_PIPELINE.md"
echo "🔧 Configuration: ./config.json"
echo ""

if [ ${#missing_deps[@]} -gt 0 ]; then
	print_status $YELLOW "⚠️  Some system dependencies are missing"
	print_status $BLUE "   Install them manually or use your system package manager"
	echo ""
fi

print_status $GREEN "✨ Integral Philosophy Publishing System is ready!"
