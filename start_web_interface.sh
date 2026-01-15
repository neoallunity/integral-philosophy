#!/bin/bash
# Web Interface Startup Script for Integral Philosophy Publishing System

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
	local color=$1
	local message=$2
	echo -e "${color}${message}${NC}"
}

print_status $BLUE "🌐 Starting Integral Philosophy Publishing System Web Interface"
echo "=================================================="

# Check if virtual environment exists
if [ ! -d "venv" ]; then
	print_status $YELLOW "📦 Virtual environment not found. Setting up..."
	./setup_venv.sh
fi

# Activate virtual environment
print_status $BLUE "🔧 Activating virtual environment..."
source venv/bin/activate

# Install web dependencies
print_status $BLUE "📚 Installing web dependencies..."
pip install -r web_requirements.txt

# Check if Flask is installed
if ! python3 -c "import flask" 2>/dev/null; then
	print_status $RED "❌ Flask installation failed"
	exit 1
fi

print_status $GREEN "✅ Dependencies installed"

# Create necessary directories
mkdir -p web_jobs
mkdir -p web_templates

# Start the web interface
print_status $BLUE "🚀 Starting web interface..."
print_status $GREEN "📱 Web interface will be available at: http://localhost:5000"
print_status $YELLOW "💡 Press Ctrl+C to stop the server"
echo

# Start Flask application
python3 web_interface.py
