#!/bin/bash
# API Server Startup Script for Integral Philosophy Publishing System

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

print_status $BLUE "🔌 Starting Integral Philosophy Publishing System API"
echo "======================================================"

# Check if virtual environment exists
if [ ! -d "venv" ]; then
	print_status $YELLOW "📦 Virtual environment not found. Setting up..."
	./setup_venv.sh
fi

# Activate virtual environment
print_status $BLUE "🔧 Activating virtual environment..."
source venv/bin/activate

# Install API dependencies
print_status $BLUE "📚 Installing API dependencies..."
pip install -r api_requirements.txt

# Check if Flask-CORS is installed
if ! python3 -c "import flask_cors" 2>/dev/null; then
	print_status $RED "❌ API dependencies installation failed"
	exit 1
fi

# Set API key if not provided
if [ -z "$INTEGRAL_API_KEY" ]; then
	export INTEGRAL_API_KEY="dev-key-$(date +%s)"
	print_status $YELLOW "⚠️  Using development API key: $INTEGRAL_API_KEY"
	print_status $YELLOW "💡 Set INTEGRAL_API_KEY environment variable for production use"
fi

# Create necessary directories
mkdir -p api_jobs

print_status $GREEN "✅ Dependencies installed"
print_status $GREEN "🔐 API Key: $INTEGRAL_API_KEY"

# Start the API server
print_status $BLUE "🚀 Starting API server..."
print_status $GREEN "📡 API will be available at: http://localhost:5001"
print_status $GREEN "📚 API Documentation: http://localhost:5001/api/v1/info"
print_status $YELLOW "💡 Press Ctrl+C to stop the server"
echo

# Start Flask application
python3 api_server.py
