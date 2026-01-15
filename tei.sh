#!/bin/bash
# TEI Generator Script
# Usage: ./tei.sh <input_json> [options]

set -e

# Default values
INPUT_FILE=""
OUTPUT_FILE="site_document.xml"

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
	echo "TEI Generator Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 <input_json> [options]"
	echo ""
	echo "Arguments:"
	echo "  input_json      Site AST JSON file"
	echo ""
	echo "Options:"
	echo "  -o, --output    Output XML file (default: site_document.xml)"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Examples:"
	echo "  $0 scraped_content/site_ast.json"
	echo "  $0 site_ast.json -o my_tei_document.xml"
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
		OUTPUT_FILE="$2"
		shift 2
		;;
	-*)
		echo "Unknown option: $1"
		show_help
		exit 1
		;;
	*)
		if [ -z "$INPUT_FILE" ]; then
			INPUT_FILE="$1"
		else
			echo "Unexpected argument: $1"
			exit 1
		fi
		shift
		;;
	esac
done

# Check input file
if [ -z "$INPUT_FILE" ]; then
	echo -e "${RED}Error: Input JSON file is required${NC}"
	show_help
	exit 1
fi

if [ ! -f "$INPUT_FILE" ]; then
	echo -e "${RED}Error: Input file not found: $INPUT_FILE${NC}"
	exit 1
fi

# Check if it's a JSON file
if [[ ! "$INPUT_FILE" =~ \.json$ ]]; then
	echo -e "${RED}Error: Input file must be a JSON file${NC}"
	exit 1
fi

# Check dependencies
if ! command -v python3 &>/dev/null; then
	echo -e "${RED}Error: Python 3 is required${NC}"
	exit 1
fi

# Run TEI generator
print_status $BLUE "📚 Generating TEI XML..."
print_status $BLUE "📄 Input: $INPUT_FILE"
print_status $BLUE "📄 Output: $OUTPUT_FILE"

python3 scripts/tei_generator.py "$INPUT_FILE" -o "$OUTPUT_FILE"

# Show results
if [ -f "$OUTPUT_FILE" ]; then
	print_status $GREEN "✅ TEI XML generated successfully!"

	# Show file info
	file_size=$(du -h "$OUTPUT_FILE" | cut -f1)
	echo ""
	print_status $BLUE "TEI Document Info:"
	echo "  📁 File: $OUTPUT_FILE"
	echo "  📏 Size: $file_size"
	echo "  📜 Type: TEI XML (Text Encoding Initiative)"

	# Show validation if xmllint is available
	if command -v xmllint &>/dev/null; then
		echo "  🔍 Validating XML structure..."
		if xmllint --noout "$OUTPUT_FILE" 2>/dev/null; then
			echo "  ✅ XML is well-formed"
		else
			echo "  ❌ XML validation failed"
		fi
	fi

	echo ""
	print_status $BLUE "Next steps:"
	echo "  • Transform to other formats: ./transform.sh $OUTPUT_FILE"
	echo "  • Validate: xmllint --valid $OUTPUT_FILE"
	echo "  • View in browser: open $OUTPUT_FILE"

else
	print_status $RED "❌ TEI generation failed. Check logs for details."
	exit 1
fi
