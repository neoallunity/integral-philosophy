#!/bin/bash
# 🌟 Integral Philosophy Publishing System - Full Stack Startup
# Starts all components in development mode

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

print_style() {
    echo -e "${2}${1}${NC}"
}

print_header() {
    echo
    print_style "🌟 Integral Philosophy Publishing System" "$CYAN"
    print_style "Starting Full Development Stack..." "$BLUE"
    echo
}

print_service() {
    local service=$1
    local port=$2
    local url=$3
    
    print_style "🌐" "$service starting on port $port..."
    echo "   URL: $url"
}

# Start services
print_header

# Start API Server (port 8001)
print_service "API Server" "8001" "http://localhost:8001"
cd subprojects/integral-philosophy-web && python -m integral_philosophy_web.api &

# Start Web Interface (port 8000)  
print_service "Web Interface" "8000" "http://localhost:8000"
cd subprojects/integral-philosophy-web && python -m integral_philosophy_web.ui &

# Core Processing Engine (as background service)
print_style "🧠" "Core Engine initialized"
cd subprojects/integral-philosophy-core && python -c "
import sys
sys.path.insert(0, 'src')
from integral_philosophy_core import ContentPipeline
pipeline = ContentPipeline()
print('✅ Core Engine ready')
" &

# Wait a moment for services to start
sleep 3

print_style "🌟" "Full system started!"
echo
print_style "🌐" "Web Interface: http://localhost:8000"
print_style "🔌" "API Server: http://localhost:8001"  
print_style "🧠" "Core Engine: Background service"
echo
print_style "⚠️" "Press Ctrl+C to stop all services"
echo

# Wait for interrupt
trap 'print_style "\n🌟 Stopping all services..."; kill 0' INT

# Keep script running
wait
