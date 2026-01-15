#!/bin/bash
# UML Generator Script
# Usage: ./uml.sh <input_json> [options]

set -e

# Default values
INPUT_FILE=""
OUTPUT_DIR="uml_diagrams"
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
	echo "UML Generator Script - Integral Philosophy Publishing System"
	echo ""
	echo "Usage: $0 <input_json> [options]"
	echo ""
	echo "Arguments:"
	echo "  input_json      Site AST JSON file"
	echo ""
	echo "Options:"
	echo "  -o, --output    Output directory (default: uml_diagrams)"
	echo "  -f, --format    Output format (plantuml, mermaid, graphviz, all)"
	echo "  -h, --help      Show this help message"
	echo ""
	echo "Examples:"
	echo "  $0 scraped_content/site_ast.json"
	echo "  $0 site_ast.json -o my_uml -f mermaid"
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

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Run UML generator
print_status $BLUE "📊 Generating UML diagrams..."
print_status $BLUE "📄 Input: $INPUT_FILE"
print_status $BLUE "📁 Output: $OUTPUT_DIR"
print_status $BLUE "🎨 Format: $FORMAT"

python3 scripts/ast_to_uml.py "$INPUT_FILE" -o "$OUTPUT_DIR" -f "$FORMAT"

# Show results
if [ -f "$OUTPUT_DIR/site_structure.puml" ] || [ -f "$OUTPUT_DIR/site_structure.mmd" ] || [ -f "$OUTPUT_DIR/site_structure.dot" ]; then
	print_status $GREEN "✅ UML diagrams generated successfully!"

	# List generated files
	echo ""
	print_status $BLUE "Generated files:"
	for file in "$OUTPUT_DIR"/*; do
		if [ -f "$file" ]; then
			echo "  📄 $(basename "$file")"
		fi
	done

	echo ""
	print_status $BLUE "Next steps:"
	echo "  • PlantUML: Use plantuml tool to render .puml files"
	echo "  • Mermaid: Use mermaid-cli or online editor for .mmd files"
	echo "  • Graphviz: Use 'dot -Tpng site_structure.dot -o diagram.png'"

else
	print_status $RED "❌ UML generation failed. Check logs for details."
	exit 1
fi
