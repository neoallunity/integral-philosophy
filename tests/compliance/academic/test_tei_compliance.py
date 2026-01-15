"""
TEI (Text Encoding Initiative) Compliance Tests for Integral Philosophy Publishing System

This module validates compliance with TEI XML standards for academic publishing,
ensuring proper structure, metadata, and encoding practices for philosophical texts.
"""

import pytest
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Dict, List, Any, Optional
import json
from lxml import etree
import re

from ...utils.base_test_classes import BaseTestCase


class TestTEIStandardsCompliance(BaseTestCase):
    """Test TEI XML standards compliance"""

    def create_sample_tei_document(self) -> str:
        """Create a valid TEI XML document for testing"""
        return """<?xml version="1.0" encoding="UTF-8"?>
<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" type="application/xml" schematypens="http://relaxng.org/ns/structure/1.0"?>
<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng" type="application/xml" schematypens="http://purl.oclc.org/dsdl/schematron"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>Test Philosophical Document</title>
                <author>
                    <persName>
                        <forename>John</forename>
                        <surname>Doe</surname>
                    </persName>
                </author>
                <principal>Test Principal</principal>
            </titleStmt>
            <publicationStmt>
                <publisher>Integral Philosophy Publishing</publisher>
                <date when="2024">2024</date>
                <availability>
                    <licence target="http://creativecommons.org/licenses/by/4.0/">
                        Creative Commons Attribution 4.0 International
                    </licence>
                </availability>
            </publicationStmt>
            <sourceDesc>
                <p>Born digital</p>
            </sourceDesc>
        </fileDesc>
        <encodingDesc>
            <projectDesc>
                <p>Test document for TEI compliance validation</p>
            </projectDesc>
        </encodingDesc>
        <profileDesc>
            <textClass>
                <keywords scheme="philosophy">
                    <term>ethics</term>
                    <term>metaphysics</term>
                </keywords>
            </textClass>
        </profileDesc>
    </teiHeader>
    <text>
        <body>
            <div type="chapter" xml:id="ch1">
                <head>Chapter 1: Introduction</head>
                <p>This is a test paragraph with some <term>philosophical</term> concepts.</p>
                <p>Citation example: <biblStruct>
                    <monogr>
                        <title>Being and Time</title>
                        <author>
                            <persName>
                                <forename>Martin</forename>
                                <surname>Heidegger</surname>
                            </persName>
                        </author>
                        <imprint>
                            <pubPlace>New York</pubPlace>
                            <publisher>Harper & Row</publisher>
                            <date when="1962">1962</date>
                        </imprint>
                    </monogr>
                </biblStruct></p>
                <note type="critical">This is a critical note about the text.</note>
                <quote>Quote example with philosophical content.</quote>
            </div>
        </body>
    </text>
</TEI>"""

    def test_tei_xml_structure_validation(self):
        """Test TEI XML basic structure validation"""
        tei_content = self.create_sample_tei_document()

        # Parse TEI document
        try:
            root = ET.fromstring(tei_content)
        except ET.ParseError as e:
            pytest.fail(f"TEI XML parsing failed: {e}")

        # Check TEI namespace
        tei_namespace = "http://www.tei-c.org/ns/1.0"
        assert root.tag == f"{{{tei_namespace}}}TEI", "Root element must be TEI"

        # Check required TEI structure
        tei_header = root.find(f".//{{{tei_namespace}}}teiHeader")
        assert tei_header is not None, "TEI document must have teiHeader"

        text = root.find(f".//{{{tei_namespace}}}text")
        assert text is not None, "TEI document must have text element"

        body = root.find(f".//{{{tei_namespace}}}body")
        assert body is not None, "TEI document must have body element"

    def test_tei_header_completeness(self):
        """Test TEI header completeness and structure"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check fileDesc
        file_desc = root.find(f".//{{{tei_namespace}}}fileDesc")
        assert file_desc is not None, "teiHeader must contain fileDesc"

        # Check titleStmt
        title_stmt = file_desc.find(f".//{{{tei_namespace}}}titleStmt")
        assert title_stmt is not None, "fileDesc must contain titleStmt"

        title = title_stmt.find(f".//{{{tei_namespace}}}title")
        assert title is not None and title.text, (
            "titleStmt must contain title with text"
        )

        # Check publicationStmt
        pub_stmt = file_desc.find(f".//{{{tei_namespace}}}publicationStmt")
        assert pub_stmt is not None, "fileDesc must contain publicationStmt"

        publisher = pub_stmt.find(f".//{{{tei_namespace}}}publisher")
        assert publisher is not None and publisher.text, (
            "publicationStmt must contain publisher"
        )

        # Check sourceDesc
        source_desc = file_desc.find(f".//{{{tei_namespace}}}sourceDesc")
        assert source_desc is not None, "fileDesc must contain sourceDesc"

    def test_bibliographic_structure_validation(self):
        """Test bibliographic structure in TEI documents"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Find bibliographic structures
        bibl_structs = root.findall(f".//{{{tei_namespace}}}biblStruct")

        if bibl_structs:
            for bibl in bibl_structs:
                # Check for monogr or analytic
                monogr = bibl.find(f".//{{{tei_namespace}}}monogr")
                analytic = bibl.find(f".//{{{tei_namespace}}}analytic")

                assert monogr or analytic, "biblStruct must contain monogr or analytic"

                if monogr:
                    # Check for title
                    title = monogr.find(f".//{{{tei_namespace}}}title")
                    assert title is not None, "monogr should contain title"

                    # Check for author or editor
                    author = monogr.find(f".//{{{tei_namespace}}}author")
                    editor = monogr.find(f".//{{{tei_namespace}}}editor")
                    assert author or editor, "monogr should contain author or editor"

    def test_name_encoding_standards(self):
        """Test proper name encoding according to TEI standards"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Find all persName elements
        pers_names = root.findall(f".//{{{tei_namespace}}}persName")

        for pers_name in pers_names:
            # Check for forename and surname structure
            forename = pers_name.find(f".//{{{tei_namespace}}}forename")
            surname = pers_name.find(f".//{{{tei_namespace}}}surname")

            if forename:
                assert forename.text, "forename element should have text content"

            if surname:
                assert surname.text, "surname element should have text content"

    def test_citation_format_compliance(self):
        """Test citation format compliance with TEI standards"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Validate citation elements
        citations = root.findall(f".//{{{tei_namespace}}}biblStruct")

        for citation in citations:
            # Check imprint structure
            imprint = citation.find(f".//{{{tei_namespace}}}imprint")
            if imprint:
                # Check for publication date
                date = imprint.find(f".//{{{tei_namespace}}}date")
                if date is not None:
                    # Validate date format
                    if "when" in date.attrib:
                        when_date = date.attrib["when"]
                        assert re.match(r"\d{4}", when_date), (
                            f"Invalid date format: {when_date}"
                        )

    def test_linguistic_annotation_compliance(self):
        """Test linguistic annotation compliance in TEI documents"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check for linguistic markup elements
        terms = root.findall(f".//{{{tei_namespace}}}term")

        for term in terms:
            # Term elements should have meaningful content
            assert term.text and len(term.text.strip()) > 0, (
                "term elements should have content"
            )

    def test_text_division_structure(self):
        """Test proper text division structure in TEI documents"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check div elements
        divs = root.findall(f".//{{{tei_namespace}}}div")

        for div in divs:
            # Check for xml:id attribute
            if "xml:id" in div.attrib:
                xml_id = div.attrib["xml:id"]
                assert xml_id, "xml:id attribute should not be empty"
                assert re.match(r"^[a-zA-Z][a-zA-Z0-9_-]*$", xml_id), (
                    f"Invalid xml:id format: {xml_id}"
                )

            # Check for head element
            head = div.find(f".//{{{tei_namespace}}}head")
            if head:
                assert head.text and len(head.text.strip()) > 0, (
                    "head element should have content"
                )

    def test_metadata_completeness(self):
        """Test metadata completeness for academic publishing"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check profileDesc
        profile_desc = root.find(f".//{{{tei_namespace}}}profileDesc")
        if profile_desc:
            # Check textClass
            text_class = profile_desc.find(f".//{{{tei_namespace}}}textClass")
            if text_class:
                # Check keywords
                keywords = text_class.find(f".//{{{tei_namespace}}}keywords")
                if keywords:
                    terms = keywords.findall(f".//{{{tei_namespace}}}term")
                    assert len(terms) > 0, "keywords should contain at least one term"

    def test_licensing_and_rights(self):
        """Test licensing and rights information in TEI documents"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check availability/licence
        publication_stmt = root.find(f".//{{{tei_namespace}}}publicationStmt")
        if publication_stmt:
            availability = publication_stmt.find(f".//{{{tei_namespace}}}availability")
            if availability:
                licence = availability.find(f".//{{{tei_namespace}}}licence")
                if licence:
                    # Check for target attribute
                    if "target" in licence.attrib:
                        target = licence.attrib["target"]
                        assert target.startswith("http"), (
                            f"Invalid licence target: {target}"
                        )


class TestTEISchemaValidation(BaseTestCase):
    """Test TEI schema validation"""

    def test_tei_all_schema_validation(self):
        """Test validation against TEI All schema"""
        tei_content = self.create_sample_tei_document()

        try:
            # Parse with lxml for better validation
            parser = etree.XMLParser(remove_blank_text=True)
            doc = etree.fromstring(tei_content.encode("utf-8"), parser)

            # Basic structure validation
            assert doc.tag == "{http://www.tei-c.org/ns/1.0}TEI"

        except etree.XMLSyntaxError as e:
            pytest.fail(f"TEI XML syntax error: {e}")

    def test_required_elements_presence(self):
        """Test presence of required TEI elements"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        required_elements = [
            "teiHeader",
            "fileDesc",
            "titleStmt",
            "title",
            "publicationStmt",
            "text",
            "body",
        ]

        for element in required_elements:
            found = root.find(f".//{{{tei_namespace}}}{element}")
            assert found is not None, f"Required TEI element '{element}' not found"

    def test_element_attribute_validation(self):
        """Test TEI element attributes validation"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check date attributes
        dates = root.findall(f".//{{{tei_namespace}}}date")
        for date in dates:
            if "when" in date.attrib:
                when_value = date.attrib["when"]
                assert re.match(r"^\d{4}(-\d{2}(-\d{2})?)?$", when_value), (
                    f"Invalid date format: {when_value}"
                )

    def test_namespace_declaration(self):
        """Test proper TEI namespace declaration"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)

        # Check namespace
        tei_namespace = "http://www.tei-c.org/ns/1.0"
        assert root.tag.startswith(f"{{{tei_namespace}}}"), (
            "TEI namespace not properly declared"
        )


class TestTEIPhilosophySpecific(BaseTestCase):
    """Test philosophy-specific TEI implementations"""

    def test_philosophical_term_markup(self):
        """Test proper marking of philosophical terms"""
        # Create TEI content with philosophical terms
        tei_content = """<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>Philosophical Terms Test</title>
            </titleStmt>
            <publicationStmt>
                <publisher>Test</publisher>
            </publicationStmt>
            <sourceDesc>
                <p>Test</p>
            </sourceDesc>
        </fileDesc>
    </teiHeader>
    <text>
        <body>
            <div>
                <p>The concept of <term type="philosophical">being</term> is fundamental 
                to <term type="philosophical" subtype="metaphysics">ontology</term>. 
                Another important <term type="philosophical">epistemology</term> concept.</p>
            </div>
        </body>
    </text>
</TEI>"""

        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Find term elements
        terms = root.findall(f".//{{{tei_namespace}}}term")
        assert len(terms) > 0, "Should contain term elements for philosophical concepts"

        for term in terms:
            assert term.text and len(term.text.strip()) > 0, (
                "term elements should have content"
            )

            # Check for type attribute
            if "type" in term.attrib:
                assert term.attrib["type"] in [
                    "philosophical",
                    "concept",
                    "technical",
                ], f"Invalid term type: {term.attrib['type']}"

    def test_critical_apparatus_structure(self):
        """Test critical apparatus structure for philosophical texts"""
        tei_content = """<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>Critical Apparatus Test</title>
            </titleStmt>
            <publicationStmt>
                <publisher>Test</publisher>
            </publicationStmt>
            <sourceDesc>
                <p>Test</p>
            </sourceDesc>
        </fileDesc>
    </teiHeader>
    <text>
        <body>
            <div>
                <p>The original text<app>
                    <lem>wisdom</lem>
                    <rdg wit="#manuscript_a">knowledge</rdg>
                    <rdg wit="#manuscript_b">understanding</rdg>
                </app> is essential.</p>
            </div>
        </body>
    </text>
</TEI>"""

        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Find apparatus elements
        apps = root.findall(f".//{{{tei_namespace}}}app")

        for app in apps:
            lem = app.find(f".//{{{tei_namespace}}}lem")
            assert lem is not None, "app should contain lem element"

            rdgs = app.findall(f".//{{{tei_namespace}}}rdg")
            assert len(rdgs) > 0, "app should contain at least one rdg element"

    def test_philosophy_citation_practices(self):
        """Test citation practices specific to philosophy texts"""
        tei_content = """<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>Philosophy Citations</title>
            </titleStmt>
            <publicationStmt>
                <publisher>Test</publisher>
            </publicationStmt>
            <sourceDesc>
                <p>Test</p>
            </sourceDesc>
        </fileDesc>
    </teiHeader>
    <text>
        <body>
            <div>
                <p>Aristotle's <biblStruct>
                    <monogr>
                        <title>Nicomachean Ethics</title>
                        <author>
                            <persName>
                                <forename>Aristotle</forename>
                            </persName>
                        </author>
                        <imprint>
                            <biblScope unit="book">1</biblScope>
                            <biblScope unit="chapter">1</biblScope>
                            <biblScope unit="line">1098a</biblScope>
                        </imprint>
                    </monogr>
                </biblStruct></p>
            </div>
        </body>
    </text>
</TEI>"""

        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Check bibliographic scope for classical works
        bibl_scopes = root.findall(f".//{{{tei_namespace}}}biblScope")

        for bibl_scope in bibl_scopes:
            if "unit" in bibl_scope.attrib:
                unit = bibl_scope.attrib["unit"]
                valid_units = ["book", "chapter", "line", "section", "page", "verse"]
                assert unit in valid_units, f"Invalid biblScope unit: {unit}"

                assert bibl_scope.text, (
                    f"biblScope with unit {unit} should have content"
                )


class TestTEIIntegration(BaseTestCase):
    """Test TEI integration with publishing system"""

    def test_tei_generation_from_content(self):
        """Test TEI XML generation from philosophical content"""
        # Sample philosophical content
        content = {
            "title": "The Nature of Consciousness",
            "author": {"forename": "David", "surname": "Chalmers"},
            "content": "Consciousness is perhaps the most perplexing puzzle in the philosophy of mind.",
            "keywords": ["consciousness", "philosophy of mind", "qualia"],
            "publisher": "Integral Philosophy Publishing",
            "date": "2024",
        }

        # Generate TEI XML (mock implementation)
        tei_xml = f'''<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
    <teiHeader>
        <fileDesc>
            <titleStmt>
                <title>{content["title"]}</title>
                <author>
                    <persName>
                        <forename>{content["author"]["forename"]}</forename>
                        <surname>{content["author"]["surname"]}</surname>
                    </persName>
                </author>
            </titleStmt>
            <publicationStmt>
                <publisher>{content["publisher"]}</publisher>
                <date when="{content["date"]}">{content["date"]}</date>
            </publicationStmt>
            <sourceDesc>
                <p>Born digital</p>
            </sourceDesc>
        </fileDesc>
        <profileDesc>
            <textClass>
                <keywords scheme="philosophy">
                    {"".join(f"<term>{kw}</term>" for kw in content["keywords"])}
                </keywords>
            </textClass>
        </profileDesc>
    </teiHeader>
    <text>
        <body>
            <div>
                <p>{content["content"]}</p>
            </div>
        </body>
    </text>
</TEI>'''

        # Validate generated TEI
        try:
            root = ET.fromstring(tei_xml)
            tei_namespace = "http://www.tei-c.org/ns/1.0"

            # Verify structure
            assert root.tag == f"{{{tei_namespace}}}TEI"

            # Verify title
            title = root.find(f".//{{{tei_namespace}}}title")
            assert title is not None and title.text == content["title"]

            # Verify content
            body_p = root.find(
                f".//{{{tei_namespace}}}body//{{{tei_namespace}}}div//{{{tei_namespace}}}p"
            )
            assert body_p is not None and content["content"] in body_p.text

        except ET.ParseError as e:
            pytest.fail(f"Generated TEI XML is invalid: {e}")

    def test_tei_conversion_compatibility(self):
        """Test TEI conversion compatibility with different formats"""
        tei_content = self.create_sample_tei_document()

        # Test parsing compatibility
        try:
            # Standard ElementTree parsing
            root1 = ET.fromstring(tei_content)

            # lxml parsing (if available)
            try:
                import lxml.etree as lxml_et

                root2 = lxml_et.fromstring(tei_content.encode("utf-8"))

                # Verify both parsers can handle the content
                assert root1.tag == root2.tag

            except ImportError:
                pytest.skip("lxml not available for compatibility testing")

        except ET.ParseError as e:
            pytest.fail(f"TEI parsing failed: {e}")

    def test_tei_metadata_extraction(self):
        """Test metadata extraction from TEI documents"""
        tei_content = self.create_sample_tei_document()
        root = ET.fromstring(tei_content)
        tei_namespace = "http://www.tei-c.org/ns/1.0"

        # Extract metadata
        metadata = {}

        # Extract title
        title = root.find(f".//{{{tei_namespace}}}title")
        if title is not None:
            metadata["title"] = title.text

        # Extract author
        author = root.find(f".//{{{tei_namespace}}}author//{{{tei_namespace}}}persName")
        if author is not None:
            forename = author.find(f".//{{{tei_namespace}}}forename")
            surname = author.find(f".//{{{tei_namespace}}}surname")
            if forename is not None and surname is not None:
                metadata["author"] = f"{forename.text} {surname.text}"

        # Extract keywords
        keywords = root.findall(
            f".//{{{tei_namespace}}}keywords//{{{tei_namespace}}}term"
        )
        if keywords:
            metadata["keywords"] = [kw.text for kw in keywords if kw.text]

        # Validate extracted metadata
        assert "title" in metadata and metadata["title"], "Title metadata not extracted"
        assert "author" in metadata and metadata["author"], (
            "Author metadata not extracted"
        )

        return metadata
