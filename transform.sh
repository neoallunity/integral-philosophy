#!/bin/bash
# Transform Script - TEI to Multiple Formats
# Usage: ./transform.sh <input_tei> [options]

set -e

# Default values
INPUT_FILE=""
OUTPUT_DIR="transformed_output"
FORMAT="all"

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
	echo "Transform Script - Integral Philosophy Publishing System"
	echo ""
	echo "Transform TEI XML to multiple output formats"
	echo ""
	echo "Usage: $0 <input_tei> [options]"
	echo ""
	echo "Arguments:"
	echo "  input_tei       TEI XML file to transform"
	echo ""
	echo "Options:"
	echo "  -o, --output    Output directory (default: transformed_output)"
	echo "  -f, --format    Output format (html, latex, pdf, epub, docx, all)"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Examples:"
	echo "  $0 site_document.xml"
	echo "  $0 site_document.xml -o published -f html"
	echo "  $0 site_document.xml -f pdf"
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
	-f | --format)
		FORMAT="$2"
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
	echo -e "${RED}Error: Input TEI XML file is required${NC}"
	show_help
	exit 1
fi

if [ ! -f "$INPUT_FILE" ]; then
	echo -e "${RED}Error: Input file not found: $INPUT_FILE${NC}"
	exit 1
fi

# Check if it's an XML file
if [[ ! "$INPUT_FILE" =~ \.xml$ ]]; then
	echo -e "${RED}Error: Input file must be an XML file${NC}"
	exit 1
fi

# Check dependencies
missing_deps=()

if ! command -v python3 &>/dev/null; then
	missing_deps+=("python3")
fi

if ! command -v pandoc &>/dev/null; then
	missing_deps+=("pandoc")
fi

if [[ "$FORMAT" == "pdf" || "$FORMAT" == "all" ]] && ! command -v lualatex &>/dev/null && ! command -v pdflatex &>/dev/null; then
	missing_deps+=("lualatex/pdflatex")
fi

if [ ${#missing_deps[@]} -gt 0 ]; then
	echo -e "${RED}Error: Missing dependencies: ${missing_deps[*]}${NC}"
	echo -e "${YELLOW}Install with: sudo apt-get install ${missing_deps[*]}${NC}"
	exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Run transformation
print_status $BLUE "🔄 Transforming TEI XML..."
print_status $BLUE "📄 Input: $INPUT_FILE"
print_status $BLUE "📁 Output: $OUTPUT_DIR"
print_status $BLUE "🎨 Format: $FORMAT"

python3 scripts/xslt_transformer.py "$INPUT_FILE" -o "$OUTPUT_DIR" -f "$FORMAT"

# Show results
format_files=("html" "latex" "pdf" "epub" "docx")
generated_files=()

for format in "${format_files[@]}"; do
	file_path="$OUTPUT_DIR/document.$format"
	if [ -f "$file_path" ]; then
		generated_files+=("$format")
		file_size=$(du -h "$file_path" | cut -f1)
		echo "  ✅ $format ($file_size)"
	else
		echo "  ❌ $format (not generated)"
	fi
done

if [ ${#generated_files[@]} -gt 0 ]; then
	echo ""
	print_status $GREEN "✅ Transformation completed successfully!"
	echo ""
	print_status $BLUE "Generated formats:"

	# List all generated files
	for file in "$OUTPUT_DIR"/*; do
		if [ -f "$file" ]; then
			file_size=$(du -h "$file" | cut -f1)
			echo "  📄 $(basename "$file") ($file_size)"
		fi
	done

	echo ""
	print_status $BLUE "Next steps:"
	for format in "${generated_files[@]}"; do
		case $format in
		"html")
			echo "  • Open in browser: open $OUTPUT_DIR/document.html"
			;;
		"pdf")
			echo "  • View PDF: open $OUTPUT_DIR/document.pdf"
			;;
		"epub")
			echo "  • Read EPUB: open $OUTPUT_DIR/document.epub"
			;;
		"docx")
			echo "  • Edit DOCX: open $OUTPUT_DIR/document.docx"
			;;
		esac
	done

else
	print_status $RED "❌ Transformation failed. Check logs for details."
	exit 1
fi
