#!/bin/bash
# Web Scraper Script
# Usage: ./scrape.sh <url> [options]

set -e

# Default values
URL=""
OUTPUT_DIR="scraped_content"
MAX_PAGES=100
DELAY=1.0

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
	echo "Web Scraper Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 <url> [options]"
	echo ""
	echo "Arguments:"
	echo "  url              Website URL to scrape"
	echo ""
	echo "Options:"
	echo "  -o, --output    Output directory (default: scraped_content)"
	echo "  -p, --pages     Maximum pages to scrape (default: 100)"
	echo "  -d, --delay     Delay between requests in seconds (default: 1.0)"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Example:"
	echo "  $0 https://philosophy-site.com -o my_scrape -p 50"
	echo ""
}

# Parse arguments
while [[ $# -gt 0 ]]; do
	case $1 in
	-h | --help)
		show_help
		exit 0
		;;
	-o | --output)
		OUTPUT_DIR="$2"
		shift 2
		;;
	-p | --pages)
		MAX_PAGES="$2"
		shift 2
		;;
	-d | --delay)
		DELAY="$2"
		shift 2
		;;
	-*)
		echo "Unknown option: $1"
		show_help
		exit 1
		;;
	*)
		if [ -z "$URL" ]; then
			URL="$1"
		else
			echo "Unexpected argument: $1"
			exit 1
		fi
		shift
		;;
	esac
done

# Check URL
if [ -z "$URL" ]; then
	echo -e "${RED}Error: URL is required${NC}"
	show_help
	exit 1
fi

# Check if Python and required packages are available
if ! command -v python3 &>/dev/null; then
	echo -e "${RED}Error: Python 3 is required${NC}"
	exit 1
fi

if ! python3 -c "import selenium" 2>/dev/null; then
	echo -e "${YELLOW}Warning: selenium not found. Install with: pip3 install selenium${NC}"
fi

# Run scraper
print_status $BLUE "🕷️  Starting web scraper..."
print_status $BLUE "📍 URL: $URL"
print_status $BLUE "📁 Output: $OUTPUT_DIR"
print_status $BLUE "📄 Max pages: $MAX_PAGES"
print_status $BLUE "⏱️  Delay: ${DELAY}s"

python3 scripts/web_scraper.py "$URL" -o "$OUTPUT_DIR" -m "$MAX_PAGES" -d "$DELAY"

# Show results
if [ -f "$OUTPUT_DIR/site_ast.json" ]; then
	print_status $GREEN "✅ Scraping completed successfully!"

	# Show statistics
	python3 -c "
import json
with open('$OUTPUT_DIR/site_ast.json', 'r') as f:
    data = json.load(f)
metadata = data.get('metadata', {})
pages = data.get('pages', {})
print(f'Total pages scraped: {metadata.get(\"total_pages\", 0)}')
print(f'Failed pages: {metadata.get(\"failed_pages\", 0)}')
print(f'Pages downloaded: {len(pages)}')
"
else
	print -e "${RED}❌ Scraping failed. Check logs for details.${NC}"
	exit 1
fi
