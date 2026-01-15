#!/bin/bash
# Master Content Pipeline Script
# Usage: ./pipeline.sh <url> [options]

set -e

# Default values
URL=""
OUTPUT_DIR="content_pipeline"
MAX_PAGES=100
HELP=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Help function
show_help() {
	echo "Content Pipeline Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 <url> [options]"
	echo ""
	echo "Arguments:"
	echo "  url              Website URL to process"
	echo ""
	echo "Options:"
	echo "  -o, --output    Output directory (default: content_pipeline)"
	echo "  -p, --pages     Maximum pages to scrape (default: 100)"
	echo "  -r, --report    Generate report only"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Examples:"
	echo "  $0 https://example.com"
	echo "  $0 https://philosophy-site.com -o my_pipeline -p 50"
	echo "  $0 https://example.com -r"
	echo ""
}

# Print colored output
print_status() {
	local color=$1
	local message=$2
	echo -e "${color}${message}${NC}"
}

# Check dependencies
check_dependencies() {
	print_status $BLUE "Checking dependencies..."

	local missing=()

	# Check Python
	if ! command -v python3 &>/dev/null; then
		missing+=("python3")
	fi

	# Check system tools
	local tools=("pandoc" "lualatex" "tidy" "saxon" "xmllint")
	for tool in "${tools[@]}"; do
		if ! command -v "$tool" &>/dev/null; then
			missing+=("$tool")
		fi
	done

	if [ ${#missing[@]} -eq 0 ]; then
		print_status $GREEN "✅ All dependencies found"
		return 0
	else
		print_status $YELLOW "⚠️  Missing dependencies: ${missing[*]}"
		print_status $BLUE "Install with:"
		echo "  Ubuntu/Debian: sudo apt-get install ${missing[*]}"
		echo "  macOS: brew install ${missing[*]}"
		return 1
	fi
}

# Install dependencies
install_dependencies() {
	print_status $BLUE "Installing dependencies..."

	if [[ "$OSTYPE" == "linux-gnu"* ]]; then
		# Linux
		if command -v apt-get &>/dev/null; then
			sudo apt-get update
			sudo apt-get install -y python3-pip selenium chromium-browser tidy saxon-xslt libxml2-utils
			sudo apt-get install -y pandoc lualatex texlive-full
			sudo apt-get install -y graphviz
		elif command -v yum &>/dev/null; then
			sudo yum install -y python3-pip selenium chromium tidy
			sudo yum install -y pandoc texlive-luatex texlive-full
			sudo yum install -y graphviz
		fi
	elif [[ "$OSTYPE" == "darwin"* ]]; then
		# macOS
		if command -v brew &>/dev/null; then
			brew install selenium-server-standalone chromium tidy-html5 saxon libxml2
			brew install pandoc lualatex graphviz
		fi
	fi

	# Install Python packages
	pip3 install selenium lxml pyyaml beautifulsoup4 html2text pillow
}

# Run pipeline
run_pipeline() {
	local url=$1
	local output_dir=$2
	local max_pages=$3
	local report_only=$4

	print_status $BLUE "🚀 Starting Content Pipeline..."
	print_status $BLUE "📍 URL: $url"
	print_status $BLUE "📁 Output: $output_dir"
	print_status $BLUE "📄 Max pages: $max_pages"

	if [ "$report_only" = true ]; then
		python3 scripts/content_pipeline.py "$url" -o "$output_dir" --report-only
	else
		python3 scripts/content_pipeline.py "$url" -o "$output_dir" -p "$max_pages"
	fi

	# Show results
	if [ -f "$output_dir/reports/pipeline_report_"*.json ]; then
		print_status $GREEN "✅ Pipeline completed successfully!"
		print_status $BLUE "📊 Report generated: $output_dir/reports/"

		# Show summary
		python3 -c "
import json
import glob
import sys
import os

latest_report = max(glob.glob('$output_dir/reports/pipeline_report_*.json'), key=os.path.getctime)
with open(latest_report, 'r') as f:
    report = json.load(f)

results = report.get('results', {})
print(f'Success: {results.get(\"success\", False)}')
if results.get('duration'):
    print(f'Duration: {results[\"duration\"]:.2f} seconds')

for stage, result in results.get('stages', {}).items():
    status = '✅' if result.get('success', False) else '❌'
    print(f'{status} {stage.title()}: {result.get(\"success\", False)}')
"
	else
		print -e "${RED}❌ Pipeline failed. Check logs for details.${NC}"
		return 1
	fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
	case $1 in
	-h | --help)
		HELP=true
		shift
		;;
	-o | --output)
		OUTPUT_DIR="$2"
		shift 2
		;;
	-p | --pages)
		MAX_PAGES="$2"
		shift 2
		;;
	-r | --report)
		REPORT_ONLY=true
		shift
		;;
	-i | --install)
		INSTALL_DEPS=true
		shift
		;;
	-*)
		echo "Unknown option $1"
		show_help
		exit 1
		;;
	*)
		if [ -z "$URL" ]; then
			URL="$1"
		else
			echo "Unexpected argument: $1"
			show_help
			exit 1
		fi
		shift
		;;
	esac
done

# Handle help
if [ "$HELP" = true ] || [ -z "$URL" ]; then
	show_help
	exit 0
fi

# Handle dependency installation
if [ "$INSTALL_DEPS" = true ]; then
	install_dependencies
	exit 0
fi

# Check dependencies first
if ! check_dependencies; then
	echo ""
	read -p "Do you want to install missing dependencies? (y/N): " -n 1 -r
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		install_dependencies
	else
		echo "Please install dependencies manually and run again."
		exit 1
	fi
fi

# Run the pipeline
run_pipeline "$URL" "$OUTPUT_DIR" "$MAX_PAGES" "${REPORT_ONLY:-false}"
