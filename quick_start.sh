#!/bin/bash

# Integral Philosophy Publishing System - Quick Start Script
# Get the system up and running in minutes

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Integral Philosophy Publishing System - Quick Start${NC}"
echo "=================================================="
echo

# Check if virtual environment exists
if [ ! -d "venv" ]; then
	echo -e "${YELLOW}📦 Creating virtual environment...${NC}"
	./setup_venv.sh
else
	echo -e "${GREEN}✓ Virtual environment exists${NC}"
fi

# Activate virtual environment
echo -e "${YELLOW}🔧 Activating virtual environment...${NC}"
source venv/bin/activate

# Check if dependencies are installed
echo -e "${YELLOW}📚 Checking dependencies...${NC}"
python -c "import lxml, selenium, pypandoc" 2>/dev/null || {
	echo -e "${RED}❌ Dependencies not found. Installing...${NC}"
	pip install -r requirements.txt
}

echo -e "${GREEN}✓ Dependencies installed${NC}"

# Create test content
echo -e "${YELLOW}📝 Creating test content...${NC}"
cat >quick_start_test.md <<'EOF'
# Quick Start Test Document

This is a test document to verify the Integral Philosophy Publishing System is working correctly.

## Mathematical Formula Test

Here's an inline formula: $E = mc^2$

And here's a block formula:
$$\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}$$

## Structure Test

### Section 1
This is a subsection with some text.

#### Subsection 1.1
- Item 1
- Item 2
- Item 3

### Section 2
More content here.

## Link Structure Test

See [Section 1](#section-1) for more details.

External link: [Wikipedia](https://wikipedia.org)
EOF

echo -e "${GREEN}✓ Test document created: quick_start_test.md${NC}"

# Test basic conversion
echo -e "${YELLOW}🔄 Testing format conversion...${NC}"
./convert.sh quick_start_test.md -f html -o quick_start_test.html

if [ -f "quick_start_test.html" ]; then
	echo -e "${GREEN}✓ HTML conversion successful${NC}"
	echo "📄 Output file: quick_start_test.html"
else
	echo -e "${RED}❌ HTML conversion failed${NC}"
	exit 1
fi

# Test TEI generation
echo -e "${YELLOW}📚 Testing TEI XML generation...${NC}"
./tei.sh quick_start_test.md -o quick_start_test.tei

if [ -f "quick_start_test.tei" ]; then
	echo -e "${GREEN}✓ TEI XML generation successful${NC}"
	echo "📄 Output file: quick_start_test.tei"
else
	echo -e "${RED}❌ TEI XML generation failed${NC}"
	exit 1
fi

# Test UML generation
echo -e "${YELLOW}🎨 Testing UML diagram generation...${NC}"
./uml.sh quick_start_test.md -f plantuml -o quick_start_test.puml

if [ -f "quick_start_test.puml" ]; then
	echo -e "${GREEN}✓ UML diagram generation successful${NC}"
	echo "📄 Output file: quick_start_test.puml"
else
	echo -e "${RED}❌ UML diagram generation failed${NC}"
	exit 1
fi

# Summary
echo
echo -e "${GREEN}🎉 Quick Start Complete!${NC}"
echo "=================================================="
echo "All tests passed successfully! The system is ready to use."
echo
echo -e "${BLUE}📚 Generated Files:${NC}"
echo "• quick_start_test.html - HTML version"
echo "• quick_start_test.tei - TEI XML version"
echo "• quick_start_test.puml - UML diagram"
echo
echo -e "${BLUE}🚀 Next Steps:${NC}"
echo "1. Process a website: ./pipeline.sh --url https://example.com"
echo "2. Convert formats: ./convert.sh --input file.md --to pdf"
echo "3. Generate UML: ./uml.sh --input site.md --format mermaid"
echo "4. Read documentation: cat docs/CONTENT_PIPELINE.md"
echo
echo -e "${YELLOW}💡 To exit the virtual environment when done: deactivate${NC}"
echo
echo -e "${GREEN}✨ Happy publishing! ✨${NC}"
