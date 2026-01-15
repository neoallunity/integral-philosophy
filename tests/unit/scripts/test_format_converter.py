#!/usr/bin/env python3
"""
Comprehensive unit tests for Format Converter component
Tests multi-format conversions, Pandoc integration, and error handling
"""

import pytest
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch, call
import subprocess

from tests.utils.base_test_classes import ComponentTestCase


class TestFormatConverter(ComponentTestCase):
    """Test cases for FormatConverter class"""

    def setup_method(self):
        """Setup test environment"""
        super().setup_method()
        self.work_dir = self.temp_dir / "format_conversion"
        self.converter_work_dir = self.work_dir / "converter"

        # Sample files for testing
        self.sample_files = {
            "markdown": self.temp_dir / "test.md",
            "html": self.temp_dir / "test.html",
            "latex": self.temp_dir / "test.tex",
            "org": self.temp_dir / "test.org",
            "rst": self.temp_dir / "test.rst",
            "asciidoc": self.temp_dir / "test.adoc",
            "typst": self.temp_dir / "test.typ",
        }

        # Create sample content
        self.sample_files["markdown"].write_text(
            """# Test Document

This is a **test** document with *emphasis*.

## Section 1

- Item 1
- Item 2

## Section 2

Here's some `code` and a [link](https://example.com).
""",
            encoding="utf-8",
        )

        self.sample_files["html"].write_text(
            """<!DOCTYPE html>
<html>
<head><title>Test Document</title></head>
<body>
<h1>Test Document</h1>
<p>This is a <strong>test</strong> document with <em>emphasis</em>.</p>
<h2>Section 1</h2>
<ul>
<li>Item 1</li>
<li>Item 2</li>
</ul>
<h2>Section 2</h2>
<p>Here's some <code>code</code> and a <a href="https://example.com">link</a>.</p>
</body>
</html>""",
            encoding="utf-8",
        )

    def test_converter_initialization(self):
        """Test FormatConverter initialization"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter()

        assert converter.work_dir == Path("format_conversion")
        assert "markdown" in converter.SUPPORTED_FORMATS
        assert "html" in converter.SUPPORTED_FORMATS
        assert "latex" in converter.SUPPORTED_FORMATS
        assert converter.work_dir.exists()

    def test_converter_initialization_custom_work_dir(self):
        """Test FormatConverter initialization with custom work directory"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        assert converter.work_dir == self.converter_work_dir
        assert converter.work_dir.exists()

    @patch("shutil.which")
    def test_dependency_check_pandoc_available(self, mock_which):
        """Test dependency checking when Pandoc is available"""
        mock_which.return_value = "/usr/bin/pandoc"

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stdout="pandoc 2.19.2")

            from scripts.format_converter import FormatConverter

            converter = FormatConverter(self.converter_work_dir)

            assert converter.pandoc_available is True
            assert converter.pandoc_version is not None
            mock_which.assert_called_once_with("pandoc")

    @patch("shutil.which")
    def test_dependency_check_pandoc_unavailable(self, mock_which):
        """Test dependency checking when Pandoc is not available"""
        mock_which.return_value = None

        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        assert converter.pandoc_available is False
        assert converter.pandoc_version is None

    def test_detect_format(self):
        """Test format detection from file extensions"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        # Test various extensions
        assert converter.detect_format(Path("test.md")) == "markdown"
        assert converter.detect_format(Path("test.markdown")) == "markdown"
        assert converter.detect_format(Path("test.html")) == "html"
        assert converter.detect_format(Path("test.htm")) == "html"
        assert converter.detect_format(Path("test.tex")) == "latex"
        assert converter.detect_format(Path("test.latex")) == "latex"
        assert converter.detect_format(Path("test.org")) == "org"
        assert converter.detect_format(Path("test.adoc")) == "asciidoc"
        assert converter.detect_format(Path("test.asc")) == "asciidoc"
        assert converter.detect_format(Path("test.rst")) == "rst"
        assert converter.detect_format(Path("test.typ")) == "typst"
        assert converter.detect_format(Path("test.json")) == "json"

        # Test unknown extension
        assert converter.detect_format(Path("test.xyz")) is None

    def test_convert_success(self):
        """Test successful format conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch.object(converter, "_convert_with_pandoc") as mock_convert:
            mock_convert.return_value = True

            success, output_file = converter.convert(
                self.sample_files["markdown"], "html"
            )

            assert success is True
            assert output_file.name.endswith(".html")
            mock_convert.assert_called_once()

    def test_convert_pandoc_unavailable(self):
        """Test conversion when Pandoc is unavailable"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = False

        success, output_file = converter.convert(self.sample_files["markdown"], "html")

        assert success is False
        assert output_file == Path()

    def test_convert_input_format_not_detected(self):
        """Test conversion with undetectable input format"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        unknown_file = self.temp_dir / "test.xyz"
        unknown_file.write_text("content", encoding="utf-8")

        success, output_file = converter.convert(unknown_file, "html")

        assert success is False
        assert output_file == Path()

    def test_convert_unsupported_output_format(self):
        """Test conversion to unsupported output format"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        success, output_file = converter.convert(
            self.sample_files["markdown"], "unsupported_format"
        )

        assert success is False
        assert output_file == Path()

    def test_convert_with_custom_output_file(self):
        """Test conversion with custom output file"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True
        custom_output = self.temp_dir / "custom.html"

        with patch.object(converter, "_convert_with_pandoc") as mock_convert:
            mock_convert.return_value = True

            success, output_file = converter.convert(
                self.sample_files["markdown"], "html", output_file=custom_output
            )

            assert success is True
            assert output_file == custom_output

    def test_convert_with_pandoc_success(self):
        """Test successful Pandoc conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            success = converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
            )

            assert success is True
            mock_run.assert_called_once()

            # Check command arguments
            call_args = mock_run.call_args[0][0]
            assert "pandoc" in call_args
            assert "-f" in call_args
            assert "-t" in call_args

    def test_convert_with_pandoc_failure(self):
        """Test failed Pandoc conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=1, stderr="Pandoc conversion failed"
            )

            success = converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
            )

            assert success is False

    def test_convert_with_pandoc_timeout(self):
        """Test Pandoc conversion timeout"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.side_effect = subprocess.TimeoutExpired("pandoc", 300)

            success = converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
            )

            assert success is False

    def test_convert_with_pandoc_not_found(self):
        """Test Pandoc conversion when pandoc is not found"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.side_effect = FileNotFoundError("pandoc not found")

            success = converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
            )

            assert success is False

    def test_convert_format_specific_options_latex(self):
        """Test LaTeX format-specific options"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.tex",
                "latex",
            )

            call_args = mock_run.call_args[0][0]
            assert "--pdf-engine=xelatex" in call_args
            assert "--variable=geometry:margin=1in" in call_args

    def test_convert_format_specific_options_html(self):
        """Test HTML format-specific options"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
            )

            call_args = mock_run.call_args[0][0]
            assert "--standalone" in call_args
            assert "--css=style.css" in call_args

    def test_convert_with_metadata(self):
        """Test conversion with metadata"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        metadata = {"title": "Custom Title", "author": "Test Author"}

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            converter._convert_with_pandoc(
                self.sample_files["markdown"],
                "markdown",
                self.temp_dir / "output.html",
                "html",
                metadata=metadata,
            )

            call_args = mock_run.call_args[0][0]
            assert "-M" in call_args
            assert "title=Custom Title" in call_args
            assert "author=Test Author" in call_args

    def test_convert_to_ast_success(self):
        """Test successful AST conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            ast_data = converter.convert_to_ast(self.sample_files["markdown"])

            assert ast_data is not None
            mock_run.assert_called_once()

            # Check that temporary AST file was created
            call_args = mock_run.call_args[0][0]
            assert "-t" in call_args
            assert "json" in call_args

    def test_convert_to_ast_failure(self):
        """Test failed AST conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=1, stderr="AST conversion failed")

            ast_data = converter.convert_to_ast(self.sample_files["markdown"])

            assert ast_data is None

    def test_convert_from_ast_success(self):
        """Test successful conversion from AST"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        ast_data = {
            "blocks": [
                {
                    "t": "Header",
                    "c": [1, ["Test Title"], []],
                }
            ]
        }

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            success = converter.convert_from_ast(
                ast_data, "html", self.temp_dir / "from_ast.html"
            )

            assert success is True
            mock_run.assert_called_once()

    def test_convert_from_ast_unsupported_format(self):
        """Test conversion from AST with unsupported format"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        ast_data = {"blocks": []}

        success = converter.convert_from_ast(
            ast_data, "unsupported_format", self.temp_dir / "output.xyz"
        )

        assert success is False

    def test_batch_convert(self):
        """Test batch conversion"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        input_files = [self.sample_files["markdown"], self.sample_files["html"]]

        with patch.object(converter, "convert") as mock_convert:
            mock_convert.side_effect = [
                (True, self.temp_dir / "test1.html"),
                (True, self.temp_dir / "test2.html"),
            ]

            results = converter.batch_convert(input_files, "html")

            assert len(results) == 2
            assert all(success for success, _ in results.values())
            assert mock_convert.call_count == 2

    def test_create_conversion_chain_success(self):
        """Test successful conversion chain"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        output_formats = ["html", "latex", "typst"]

        with patch.object(converter, "convert") as mock_convert:
            mock_convert.side_effect = [
                (True, self.temp_dir / "test.html"),
                (True, self.temp_dir / "test.tex"),
                (True, self.temp_dir / "test.typ"),
            ]

            results = converter.create_conversion_chain(
                self.sample_files["markdown"], output_formats
            )

            assert len(results) == 3
            assert all(results[fmt][0] for fmt in output_formats)
            assert mock_convert.call_count == 3

    def test_create_conversion_chain_break_on_failure(self):
        """Test conversion chain breaking on failure"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        output_formats = ["html", "latex", "typst"]

        with patch.object(converter, "convert") as mock_convert:
            mock_convert.side_effect = [
                (True, self.temp_dir / "test.html"),
                (False, Path()),  # Failure breaks chain
            ]

            results = converter.create_conversion_chain(
                self.sample_files["markdown"], output_formats
            )

            assert results["html"][0] is True
            assert results["latex"][0] is False
            assert "typst" not in results  # Chain should break
            assert mock_convert.call_count == 2

    def test_calculate_text_similarity(self):
        """Test text similarity calculation"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        # Identical texts
        similarity = converter._calculate_text_similarity(
            "This is a test document.", "This is a test document."
        )
        assert similarity == 1.0

        # Completely different texts
        similarity = converter._calculate_text_similarity(
            "First document text", "Second different text"
        )
        assert similarity < 1.0

        # Empty texts
        similarity = converter._calculate_text_similarity("", "")
        assert similarity == 1.0

        # One empty text
        similarity = converter._calculate_text_similarity("Some text", "")
        assert similarity == 0.0

    def test_compare_conversions(self):
        """Test conversion comparison"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        # Create converted files
        html_file = self.temp_dir / "converted.html"
        tex_file = self.temp_dir / "converted.tex"

        html_file.write_text("<h1>Test</h1><p>Content here.</p>", encoding="utf-8")
        tex_file.write_text("\\section{Test}\\nContent here.", encoding="utf-8")

        conversions = {"html": html_file, "latex": tex_file}

        comparison = converter.compare_conversions(
            self.sample_files["markdown"], conversions
        )

        assert "original" in comparison
        assert "conversions" in comparison
        assert "text_similarity" in comparison
        assert "structure_comparison" in comparison
        assert "html" in comparison["conversions"]
        assert "latex" in comparison["conversions"]

    def test_extract_headings_markdown(self):
        """Test heading extraction from Markdown"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        markdown_content = """# Main Title

## Section 1

### Subsection 1.1

## Section 2

Some content here."""

        md_file = self.temp_dir / "test.md"
        md_file.write_text(markdown_content, encoding="utf-8")

        headings = converter._extract_headings(md_file)

        assert "#1 Main Title" in headings
        assert "#2 Section 1" in headings
        assert "#3 Subsection 1.1" in headings
        assert "#2 Section 2" in headings

    def test_extract_headings_html(self):
        """Test heading extraction from HTML"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        html_content = """<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
<h1>Main Title</h1>
<h2>Section 1</h2>
<h3>Subsection 1.1</h3>
<h2>Section 2</h2>
</body>
</html>"""

        html_file = self.temp_dir / "test.html"
        html_file.write_text(html_content, encoding="utf-8")

        headings = converter._extract_headings(html_file)

        assert "h1 Main Title" in headings
        assert "h2 Section 1" in headings
        assert "h3 Subsection 1.1" in headings
        assert "h2 Section 2" in headings

    def test_extract_headings_latex(self):
        """Test heading extraction from LaTeX"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        latex_content = r"""\documentclass{article}
\begin{document}
\section{Main Title}
\subsection{Section 1}
\subsubsection{Subsection 1.1}
\section{Section 2}
\end{document}"""

        tex_file = self.temp_dir / "test.tex"
        tex_file.write_text(latex_content, encoding="utf-8")

        headings = converter._extract_headings(tex_file)

        assert "\\section{Main Title}" in headings
        assert "\\subsection{Section 1}" in headings
        assert "\\subsubsection{Subsection 1.1}" in headings
        assert "\\section{Section 2}" in headings

    def test_extract_headings_org(self):
        """Test heading extraction from Org mode"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        org_content = """* Main Title

** Section 1

*** Subsection 1.1

** Section 2

Content here."""

        org_file = self.temp_dir / "test.org"
        org_file.write_text(org_content, encoding="utf-8")

        headings = converter._extract_headings(org_file)

        assert "* Main Title" in headings
        assert "** Section 1" in headings
        assert "*** Subsection 1.1" in headings
        assert "** Section 2" in headings

    def test_extract_headings_rst(self):
        """Test heading extraction from reStructuredText"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        rst_content = """Main Title
==========

Section 1
---------

Subsection 1.1
~~~~~~~~~~~~~

Section 2
---------

Content here."""

        rst_file = self.temp_dir / "test.rst"
        rst_file.write_text(rst_content, encoding="utf-8")

        headings = converter._extract_headings(rst_file)

        assert "Main Title" in headings
        assert "Section 1" in headings
        assert "Subsection 1.1" in headings
        assert "Section 2" in headings

    def test_compare_structure(self):
        """Test structure comparison"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        # Create two files with same structure
        file1 = self.temp_dir / "file1.md"
        file2 = self.temp_dir / "file2.md"

        file1.write_text("# Title\\n## Section 1\\n## Section 2", encoding="utf-8")
        file2.write_text("# Title\\n## Section 1\\n## Section 2", encoding="utf-8")

        comparison = converter._compare_structure(file1, file2)

        assert comparison["headings_match"] is True
        assert len(comparison["headings_file1"]) > 0
        assert comparison["headings_file1"] == comparison["headings_file2"]

    def test_create_format_matrix(self):
        """Test conversion matrix creation"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)
        converter.pandoc_available = True

        with patch.object(converter, "convert") as mock_convert:
            # Mock some successful conversions, some failures
            def convert_side_effect(input_file, output_format):
                if output_format in ["html", "latex"]:
                    return (True, self.temp_dir / f"test.{output_format}")
                else:
                    return (False, Path())

            mock_convert.side_effect = convert_side_effect

            matrix = converter.create_format_matrix(self.sample_files["markdown"])

            assert matrix["input_file"] == str(self.sample_files["markdown"])
            assert matrix["input_format"] == "markdown"
            assert matrix["success_rate"] == 2 / (len(converter.SUPPORTED_FORMATS) - 1)
            assert "conversions" in matrix
            assert matrix["conversions"]["html"]["success"] is True
            assert matrix["conversions"]["latex"]["success"] is True
            assert matrix["conversions"]["org"]["success"] is False

    def test_edge_cases(self):
        """Test edge cases and error conditions"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        # Test with non-existent file
        non_existent = Path("non_existent.md")
        assert converter.detect_format(non_existent) is None

        # Test with empty file
        empty_file = self.temp_dir / "empty.md"
        empty_file.write_text("", encoding="utf-8")
        assert converter.detect_format(empty_file) == "markdown"

        # Test text similarity with unicode
        similarity = converter._calculate_text_similarity(
            "Café naïve 中文", "Café naïve 中文"
        )
        assert similarity == 1.0

        # Test comparison with non-existent files
        with patch("builtins.open", side_effect=FileNotFoundError):
            comparison = converter.compare_conversions(
                self.sample_files["markdown"], {}
            )
            assert "error" in comparison

    def test_pandoc_version_parsing(self):
        """Test Pandoc version parsing"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=0, stdout="pandoc 2.19.2\\nCompiled with GHC 8.10.7"
            )

            version = converter._get_pandoc_version()

            assert version is not None
            assert "pandoc 2.19.2" in version

    def test_pandoc_version_parsing_failure(self):
        """Test Pandoc version parsing failure"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        with patch("subprocess.run") as mock_run:
            mock_run.side_effect = Exception("Command failed")

            version = converter._get_pandoc_version()

            assert version is None

    def test_format_support_coverage(self):
        """Test that all expected formats are supported"""
        from scripts.format_converter import FormatConverter

        converter = FormatConverter(self.converter_work_dir)

        expected_formats = [
            "markdown",
            "org",
            "asciidoc",
            "rst",
            "typst",
            "html",
            "latex",
            "tei",
            "docbook",
            "jats",
            "json",
        ]

        for format_name in expected_formats:
            assert format_name in converter.SUPPORTED_FORMATS
            assert isinstance(converter.SUPPORTED_FORMATS[format_name], list)
            assert len(converter.SUPPORTED_FORMATS[format_name]) > 0
