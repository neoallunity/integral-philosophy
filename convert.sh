#!/bin/bash
# Format Converter Script
# Usage: ./convert.sh <input_file> [options]

set -e

# Default values
INPUT_FILE=""
OUTPUT_FORMAT=""
OUTPUT_FILE=""
WORK_DIR="format_conversion"

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
	echo "Format Converter Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 <input_file> [options]"
	echo ""
	echo "Arguments:"
	echo "  input_file      Input file to convert"
	echo ""
	echo "Options:"
	echo "  -f, --format    Output format (html, latex, pdf, epub, docx, org, asciidoc, rst, typst, tei)"
	echo "  -o, --output    Output file path"
	echo "  -w, --work-dir  Working directory (default: format_conversion)"
	echo "  -c, --chain     Conversion chain (space-separated list)"
	echo "  -a, --ast       Convert to AST"
	echo "  -m, --matrix    Create conversion matrix"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Examples:"
	echo "  $0 document.md -f html"
	echo "  $0 document.md --chain org asciidoc rst"
	echo "  $0 document.md -f pdf -o output.pdf"
	echo "  $0 document.md --matrix"
	echo ""
	echo "Supported formats: markdown, org, asciidoc, rst, typst, html, latex, tei, docbook, jats, json"
}

# Parse arguments
AST_ONLY=false
MATRIX_ONLY=false
CONVERSION_CHAIN=()

while [[ $# -gt 0 ]]; do
	case $1 in
	-h | --help)
		show_help
		exit 0
		;;
	-f | --format)
		OUTPUT_FORMAT="$2"
		shift 2
		;;
	-o | --output)
		OUTPUT_FILE="$2"
		shift 2
		;;
	-w | --work-dir)
		WORK_DIR="$2"
		shift 2
		;;
	-c | --chain)
		shift
		while [[ $# -gt 0 && ! "$1" =~ ^- ]]; do
			CONVERSION_CHAIN+=("$1")
			shift
		done
		;;
	-a | --ast)
		AST_ONLY=true
		shift
		;;
	-m | --matrix)
		MATRIX_ONLY=true
		shift
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
	echo -e "${RED}Error: Input file is required${NC}"
	show_help
	exit 1
fi

if [ ! -f "$INPUT_FILE" ]; then
	echo -e "${RED}Error: Input file not found: $INPUT_FILE${NC}"
	exit 1
fi

# Check dependencies
if ! command -v python3 &>/dev/null; then
	echo -e "${RED}Error: Python 3 is required${NC}"
	exit 1
fi

if ! command -v pandoc &>/dev/null; then
	echo -e "${YELLOW}Warning: Pandoc not found. Install with: brew install pandoc or apt-get install pandoc${NC}"
fi

# Create working directory
mkdir -p "$WORK_DIR"

# Run conversion
if [ "$AST_ONLY" = true ]; then
	print_status $BLUE "🔄 Converting to AST..."
	python3 scripts/format_converter.py "$INPUT_FILE" --ast --work-dir "$WORK_DIR"

elif [ "$MATRIX_ONLY" = true ]; then
	print_status $BLUE "📊 Creating conversion matrix..."
	python3 scripts/format_converter.py "$INPUT_FILE" --matrix --work-dir "$WORK_DIR"

elif [ ${#CONVERSION_CHAIN[@]} -gt 0 ]; then
	print_status $BLUE "🔄 Running conversion chain..."
	echo "Chain: ${CONVERSION_CHAIN[*]}"
	python3 scripts/format_converter.py "$INPUT_FILE" --chain "${CONVERSION_CHAIN[@]}" --work-dir "$WORK_DIR"

elif [ -n "$OUTPUT_FORMAT" ]; then
	print_status $BLUE "🔄 Converting to $OUTPUT_FORMAT..."

	if [ -n "$OUTPUT_FILE" ]; then
		python3 scripts/format_converter.py "$INPUT_FILE" -f "$OUTPUT_FORMAT" -o "$OUTPUT_FILE" --work-dir "$WORK_DIR"
	else
		python3 scripts/format_converter.py "$INPUT_FILE" -f "$OUTPUT_FORMAT" --work-dir "$WORK_DIR"
	fi

	# Show results
	if [ -f "$OUTPUT_FILE" ]; then
		print_status $GREEN "✅ Conversion completed: $OUTPUT_FILE"
	else
		print_status $GREEN "✅ Conversion completed"
	fi

else
	echo -e "${RED}Error: Either --format, --chain, --ast, or --matrix is required${NC}"
	show_help
	exit 1
fi
