#!/bin/bash
# 🌟 Integral Philosophy Publishing System - Complete Setup
# Sets up all subprojects with beautiful output

set -e

# Colors
RED='\033[0;31m'
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
    print_style "🌟 Integral Philosophy Publishing System - Complete Setup" "$CYAN"
    print_style "Modular Academic Content Processing Platform" "$BLUE"
    echo
}

print_step() {
    echo
    print_style "$1 $2" "$YELLOW"
}

print_success() {
    print_style "$CHECK $1" "$GREEN"
}

setup_subproject() {
    local project=$1
    local path="subprojects/$project"
    
    if [ -d "$path" ]; then
        print_step "⚙️" "Setting up $project..."
        cd "$path"
        
        if [ -f "pyproject.toml" ]; then
            python -m pip install -e ".[dev]" --quiet
            print_success "$project installed"
        else
            print_step "📦" "Installing $project..."
            # Create basic structure if needed
            mkdir -p src tests docs examples
            print_success "$project structure created"
        fi
        
        cd ../..
    else
        print_step "⚠️" "$project not found, skipping..."
    fi
}

print_header

# Setup all subprojects
setup_subproject "integral-philosophy-core"
setup_subproject "integral-philosophy-web"
setup_subproject "integral-philosophy-cli"
setup_subproject "integral-philosophy-docs"
setup_subproject "integral-philosophy-content"
setup_subproject "integral-philosophy-config"
setup_subproject "integral-philosophy-deploy"
setup_subproject "integral-philosophy-tests"

print_success "🌟 All subprojects setup complete!"
echo
print_style "🚀 Next steps:" "$BLUE"
echo "  1. Activate environment:"
echo "     source venv/bin/activate"
echo "  2. Start full system:"
echo "     ./start-full.sh"
echo "  3. Or work on individual subprojects:"
echo "     cd subprojects/integral-philosophy-core"
echo
