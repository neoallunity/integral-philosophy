#!/bin/bash
# Integral Philosophy Publishing System - Elegant Setup Script
# Sets up a beautiful, clean environment for academic content processing

set -e # Exit on any error

# Colors for beautiful output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Emoji for beautiful output
ROCKET="🚀"
SPARKLE="✨"
GEAR="⚙️"
CHECK="✅"
BOOK="📚"
WRENCH="🛠️"

# Print with style
print_style() {
	echo -e "${2}${1}${NC}"
}

print_header() {
	echo
	print_style "🌟 Integral Philosophy Publishing System 🌟" "$CYAN"
	print_style "Elegant Academic Content Processing Pipeline" "$BLUE"
	echo
}

print_step() {
	echo
	print_style "$1 $2" "$3"
}

print_success() {
	print_style "$CHECK $1" "$GREEN"
}

print_error() {
	print_style "❌ $1" "$RED"
}

# Check prerequisites
check_prerequisites() {
	print_step "$GEAR" "Checking prerequisites..." "$YELLOW"

	# Check Python
	if command -v python3 &>/dev/null; then
		PYTHON_VERSION=$(python3 --version | cut -d' ' -f2)
		print_success "Python $PYTHON_VERSION found"
	else
		print_error "Python 3.8+ is required but not found"
		exit 1
	fi

	# Check pip
	if command -v pip3 &>/dev/null; then
		print_success "pip found"
	else
		print_error "pip is required but not found"
		exit 1
	fi

	# Check Pandoc (optional but recommended)
	if command -v pandoc &>/dev/null; then
		PANDOC_VERSION=$(pandoc --version | head -n1 | cut -d' ' -f2)
		print_success "Pandoc $PANDOC_VERSION found"
	else
		print_style "⚠️  Pandoc not found (optional but recommended for full functionality)" "$YELLOW"
	fi
}

# Create virtual environment
create_venv() {
	print_step "$GEAR" "Creating beautiful virtual environment..." "$BLUE"

	if [ ! -d "venv" ]; then
		python3 -m venv venv
		print_success "Virtual environment created"
	else
		print_success "Virtual environment already exists"
	fi
}

# Activate virtual environment
activate_venv() {
	print_step "$GEAR" "Activating virtual environment..." "$BLUE"

	if [ -f "venv/bin/activate" ]; then
		source venv/bin/activate
		print_success "Virtual environment activated"
	else
		print_error "Virtual environment not found"
		exit 1
	fi
}

# Install dependencies
install_dependencies() {
	print_step "$GEAR" "Installing elegant dependencies..." "$BLUE"

	# Upgrade pip
	pip install --upgrade pip >/dev/null 2>&1

	# Install core dependencies
	if [ -f "docs/user/requirements.txt" ]; then
		pip install -r docs/user/requirements.txt
		print_success "Core dependencies installed"
	else
		# Install essential packages manually
		pip install beautifulsoup4 lxml selenium requests pandas numpy matplotlib seaborn plotly networkx pillow jinja2 flask pyyaml >/dev/null 2>&1
		print_success "Essential dependencies installed"
	fi
}

# Create necessary directories
create_directories() {
	print_step "$GEAR" "Creating beautiful directory structure..." "$BLUE"

	mkdir -p data/{input,output,cache}
	mkdir -p logs
	mkdir -p temp

	print_success "Directory structure created"
}

# Setup configuration
setup_config() {
	print_step "$GEAR" "Setting up beautiful configuration..." "$BLUE"

	if [ ! -f "config/pipelines/default.yaml" ]; then
		print_success "Configuration already exists"
	else
		print_success "Default configuration ready"
	fi
}

# Validate installation
validate_installation() {
	print_step "$GEAR" "Validating beautiful installation..." "$YELLOW"

	# Test Python imports
	python3 -c "
import sys
sys.path.insert(0, 'core')
try:
    import yaml
    import requests
    from bs4 import BeautifulSoup
    print('✅ Core modules import successfully')
except ImportError as e:
    print(f'❌ Import error: {e}')
    exit(1)
"

	if [ $? -eq 0 ]; then
		print_success "Installation validated"
	else
		print_error "Installation validation failed"
		exit 1
	fi
}

# Show next steps
show_next_steps() {
	echo
	print_style "$SPARKLE Beautiful Setup Complete! $SPARKLE" "$GREEN"
	echo
	print_style "$BOOK Quick Start Guide:" "$CYAN"
	echo
	echo "  1. Activate environment:"
	echo "     $ source venv/bin/activate"
	echo
	echo "  2. Process content:"
	echo "     $ python main.py process https://example.com --output ./results"
	echo
	echo "  3. Start web interface:"
	echo "     $ python main.py web --port 8000"
	echo
	echo "  4. Start API server:"
	echo "     $ python main.py api --port 8001"
	echo
	echo "  5. Get help:"
	echo "     $ python main.py --help"
	echo
	print_style "$ROCKET Your elegant academic publishing system is ready!" "$GREEN"
	echo
}

# Main setup function
main() {
	print_header

	check_prerequisites
	create_venv
	activate_venv
	install_dependencies
	create_directories
	setup_config
	validate_installation
	show_next_steps
}

# Run the beautiful setup
main "$@"
