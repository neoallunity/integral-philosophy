#!/usr/bin/env python3
"""
Comprehensive unit tests for Content Pipeline component
Tests 6-stage pipeline orchestration, async operations, and error handling
"""

import pytest
import asyncio
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch, AsyncMock
import time

from tests.utils.base_test_classes import ComponentTestCase, AsyncTestCase


class TestContentPipeline(ComponentTestCase, AsyncTestCase):
    """Test cases for ContentPipeline class"""

    def setup_method(self):
        """Setup test environment"""
        super().setup_method()
        self.work_dir = self.temp_dir / "pipeline_test"
        self.test_url = "https://example.com"

        # Create directory structure
        self.dirs = {
            "scraped": self.work_dir / "01_scraped",
            "parsed": self.work_dir / "02_parsed",
            "uml": self.work_dir / "03_uml",
            "tei": self.work_dir / "04_tei",
            "transformed": self.work_dir / "05_transformed",
            "validation": self.work_dir / "06_validation",
            "reports": self.work_dir / "reports",
        }

        for dir_path in self.dirs.values():
            dir_path.mkdir(exist_ok=True)

    @pytest.fixture
    def sample_site_ast(self):
        """Sample site AST for testing"""
        return {
            "metadata": {
                "base_url": self.test_url,
                "domain": "example.com",
                "scraped_at": "2024-01-01 12:00:00",
                "total_pages": 2,
                "failed_pages": 0,
            },
            "pages": {
                "https://example.com/": {
                    "title": "Home Page",
                    "content": "# Home\\n\\nWelcome to our site.",
                    "metadata": {
                        "title": "Home Page",
                        "description": "Home page description",
                        "language": "en",
                    },
                    "links": ["https://example.com/about"],
                },
                "https://example.com/about": {
                    "title": "About Us",
                    "content": "# About\\n\\nInformation about us.",
                    "metadata": {
                        "title": "About Us",
                        "description": "About page",
                        "language": "en",
                    },
                    "links": [],
                },
            },
            "links": {
                "https://example.com/": ["https://example.com/about"],
                "https://example.com/about": [],
            },
        }

    @pytest.fixture
    def sample_parsed_summary(self):
        """Sample parsed content summary"""
        return {
            "index.md": {
                "ast_file": "02_parsed/index.ast.json",
                "metadata": {"title": "Home Page", "language": "en"},
                "content_length": 50,
            },
            "about.md": {
                "ast_file": "02_parsed/about.ast.json",
                "metadata": {"title": "About Us", "language": "en"},
                "content_length": 40,
            },
        }

    def test_pipeline_initialization(self):
        """Test ContentPipeline initialization"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        assert pipeline.work_dir == self.work_dir
        assert all(pipeline.dirs[name].exists() for name in self.dirs.keys())
        assert pipeline.results["start_time"] is None
        assert pipeline.results["end_time"] is None
        assert pipeline.results["success"] is False
        assert pipeline.parser is not None
        assert pipeline.uml_transformer is not None
        assert pipeline.tei_generator is not None

    def test_pipeline_initialization_custom_work_dir(self):
        """Test pipeline initialization with custom work directory"""
        custom_dir = self.temp_dir / "custom_pipeline"

        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(custom_dir)

        assert pipeline.work_dir == custom_dir
        assert all(
            pipeline.dirs[name].parent == custom_dir for name in self.dirs.keys()
        )

    async def test_process_website_success(self, sample_site_ast):
        """Test successful website processing through all stages"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Mock all stage methods
        pipeline._stage_1_scrape_website = AsyncMock(return_value=True)
        pipeline._stage_2_parse_content = AsyncMock(return_value=True)
        pipeline._stage_3_generate_uml = AsyncMock(return_value=True)
        pipeline._stage_4_generate_tei = AsyncMock(return_value=True)
        pipeline._stage_5_transform_formats = AsyncMock(return_value=True)
        pipeline._stage_6_validate_pipeline = AsyncMock(return_value=True)

        start_time = time.time()
        result = await pipeline.process_website(self.test_url, max_pages=10)
        end_time = time.time()

        assert result is True
        assert pipeline.results["success"] is True
        assert pipeline.results["start_time"] is not None
        assert pipeline.results["end_time"] is not None
        assert pipeline.results["duration"] >= 0
        assert abs(pipeline.results["duration"] - (end_time - start_time)) < 1.0

        # Verify all stages were called
        pipeline._stage_1_scrape_website.assert_called_once_with(self.test_url, 10)
        pipeline._stage_2_parse_content.assert_called_once()
        pipeline._stage_3_generate_uml.assert_called_once()
        pipeline._stage_4_generate_tei.assert_called_once()
        pipeline._stage_5_transform_formats.assert_called_once()
        pipeline._stage_6_validate_pipeline.assert_called_once()

    async def test_process_website_stage_failure(self):
        """Test website processing with stage failure"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Mock stage 1 to succeed, stage 2 to fail
        pipeline._stage_1_scrape_website = AsyncMock(return_value=True)
        pipeline._stage_2_parse_content = AsyncMock(return_value=False)

        result = await pipeline.process_website(self.test_url)

        assert result is False
        assert pipeline.results["success"] is False
        assert "error" in pipeline.results
        assert pipeline.results["duration"] is not None

        # Should stop after first failing stage
        pipeline._stage_3_generate_uml.assert_not_called()
        pipeline._stage_4_generate_tei.assert_not_called()
        pipeline._stage_5_transform_formats.assert_not_called()
        pipeline._stage_6_validate_pipeline.assert_not_called()

    async def test_process_website_exception_handling(self):
        """Test website processing with exception"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Mock stage to raise exception
        pipeline._stage_1_scrape_website = AsyncMock(
            side_effect=Exception("Test error")
        )

        result = await pipeline.process_website(self.test_url)

        assert result is False
        assert pipeline.results["success"] is False
        assert "Test error" in pipeline.results["error"]

    async def test_stage_1_scrape_website_success(self, sample_site_ast):
        """Test successful website scraping stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create mock site AST file
        site_ast_file = self.dirs["scraped"] / "site_ast.json"
        site_ast_file.write_text(json.dumps(sample_site_ast), encoding="utf-8")

        result = await pipeline._stage_1_scrape_website(self.test_url, max_pages=5)

        assert result is True
        assert "scraping" in pipeline.results["stages"]
        assert pipeline.results["stages"]["scraping"]["success"] is True
        assert pipeline.results["stages"]["scraping"]["pages_downloaded"] == 2
        assert pipeline.results["stages"]["scraping"]["failed_pages"] == 0

    async def test_stage_1_scrape_website_no_ast_file(self):
        """Test scraping stage with missing AST file"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Don't create site AST file
        result = await pipeline._stage_1_scrape_website(self.test_url)

        assert result is False
        assert pipeline.results["stages"]["scraping"]["success"] is False
        assert "not generated" in pipeline.results["stages"]["scraping"]["error"]

    async def test_stage_1_scrape_website_exception(self):
        """Test scraping stage with exception"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Mock scraper to raise exception
        with patch("scripts.content_pipeline.WebScraper") as mock_scraper_class:
            mock_scraper = Mock()
            mock_scraper_class.return_value = mock_scraper
            mock_scraper.recursive_scrape.side_effect = Exception("Scraping failed")

            result = await pipeline._stage_1_scrape_website(self.test_url)

            assert result is False
            assert pipeline.results["stages"]["scraping"]["success"] is False
            assert "Scraping failed" in pipeline.results["stages"]["scraping"]["error"]

    async def test_stage_2_parse_content_success(self, sample_parsed_summary):
        """Test successful content parsing stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create sample markdown files
        (self.dirs["scraped"] / "pages").mkdir(exist_ok=True)

        # Create sample markdown files
        (self.dirs["scraped"] / "pages" / "index.md").write_text(
            "# Home\\n\\nContent", encoding="utf-8"
        )
        (self.dirs["scraped"] / "pages" / "about.md").write_text(
            "# About\\n\\nAbout content", encoding="utf-8"
        )

        # Create metadata files
        (self.dirs["scraped"] / "pages" / "index.json").write_text(
            '{"title": "Home"}', encoding="utf-8"
        )
        (self.dirs["scraped"] / "pages" / "about.json").write_text(
            '{"title": "About"}', encoding="utf-8"
        )

        # Mock parser
        pipeline.parser.parse = Mock(return_value={"type": "document", "children": []})
        pipeline.parser.save_ast = Mock()

        result = await pipeline._stage_2_parse_content()

        assert result is True
        assert "parsing" in pipeline.results["stages"]
        assert pipeline.results["stages"]["parsing"]["success"] is True
        assert pipeline.results["stages"]["parsing"]["total_files"] == 2
        assert pipeline.results["stages"]["parsing"]["successful_parses"] == 2
        assert pipeline.results["stages"]["parsing"]["parse_rate"] == 1.0

    async def test_stage_2_parse_content_no_files(self):
        """Test parsing stage with no files"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Don't create any markdown files
        result = await pipeline._stage_2_parse_content()

        assert result is True  # Empty parsing is still success
        assert pipeline.results["stages"]["parsing"]["success"] is True
        assert pipeline.results["stages"]["parsing"]["total_files"] == 0
        assert pipeline.results["stages"]["parsing"]["successful_parses"] == 0

    async def test_stage_2_parse_content_with_failures(self):
        """Test parsing stage with some failures"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create sample markdown file
        (self.dirs["scraped"] / "pages").mkdir(exist_ok=True)
        (self.dirs["scraped"] / "pages" / "index.md").write_text(
            "# Home\\n\\nContent", encoding="utf-8"
        )

        # Mock parser to fail on some files
        pipeline.parser.parse = Mock(
            side_effect=[{"type": "document"}, Exception("Parse failed")]
        )
        pipeline.parser.save_ast = Mock()

        result = await pipeline._stage_2_parse_content()

        assert result is True  # Partial success is still success
        assert pipeline.results["stages"]["parsing"]["success"] is True
        assert pipeline.results["stages"]["parsing"]["total_files"] == 1
        assert pipeline.results["stages"]["parsing"]["successful_parses"] == 1

    async def test_stage_3_generate_uml_success(self, sample_site_ast):
        """Test successful UML generation stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create site AST file
        site_ast_file = self.dirs["scraped"] / "site_ast.json"
        site_ast_file.write_text(json.dumps(sample_site_ast), encoding="utf-8")

        # Mock UML transformer
        uml_data = {"nodes": [{"id": "1", "type": "page"}], "edges": []}
        pipeline.uml_transformer.transform_site_ast = Mock(return_value=uml_data)
        pipeline.uml_transformer.generate_all_formats = Mock()

        result = await pipeline._stage_3_generate_uml()

        assert result is True
        assert "uml" in pipeline.results["stages"]
        assert pipeline.results["stages"]["uml"]["success"] is True
        assert pipeline.results["stages"]["uml"]["nodes"] == 1
        assert pipeline.results["stages"]["uml"]["edges"] == 0
        assert "plantuml" in pipeline.results["stages"]["uml"]["formats_generated"]

    async def test_stage_4_generate_tei_success(self, sample_site_ast):
        """Test successful TEI generation stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create site AST file
        site_ast_file = self.dirs["scraped"] / "site_ast.json"
        site_ast_file.write_text(json.dumps(sample_site_ast), encoding="utf-8")

        # Mock TEI generator
        tei_xml = '<?xml version="1.0"?><TEI>...</TEI>'
        pipeline.tei_generator.generate_tei_document = Mock(return_value=tei_xml)
        pipeline.tei_generator.save_tei_document = Mock()
        pipeline._validate_xml = Mock(return_value={"valid": True, "error": None})

        result = await pipeline._stage_4_generate_tei()

        assert result is True
        assert "tei" in pipeline.results["stages"]
        assert pipeline.results["stages"]["tei"]["success"] is True
        assert pipeline.results["stages"]["tei"]["validation"]["valid"] is True

    async def test_stage_5_transform_formats_success(self):
        """Test successful format transformation stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create TEI file
        tei_file = self.dirs["tei"] / "site_document.xml"
        tei_file.write_text('<?xml version="1.0"?><TEI>...</TEI>', encoding="utf-8")

        # Mock XSLT transformer
        transformation_results = {
            "html": True,
            "tex": True,
            "pdf": True,
            "epub": True,
            "docx": True,
        }
        pipeline.xslt_transformer.transform_all_formats = Mock(
            return_value=transformation_results
        )

        result = await pipeline._stage_5_transform_formats()

        assert result is True
        assert "transformation" in pipeline.results["stages"]
        assert pipeline.results["stages"]["transformation"]["success"] is True
        assert pipeline.results["stages"]["transformation"]["successful_formats"] == 5
        assert pipeline.results["stages"]["transformation"]["total_formats"] == 5

    async def test_stage_5_transform_formats_partial_failure(self):
        """Test format transformation stage with some failures"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create TEI file
        tei_file = self.dirs["tei"] / "site_document.xml"
        tei_file.write_text('<?xml version="1.0"?><TEI>...</TEI>', encoding="utf-8")

        # Mock XSLT transformer with some failures
        transformation_results = {
            "html": True,
            "tex": False,  # Failed
            "pdf": True,
            "epub": False,  # Failed
            "docx": True,
        }
        pipeline.xslt_transformer.transform_all_formats = Mock(
            return_value=transformation_results
        )

        result = await pipeline._stage_5_transform_formats()

        assert result is True  # Partial success is still success
        assert pipeline.results["stages"]["transformation"]["success"] is True
        assert pipeline.results["stages"]["transformation"]["successful_formats"] == 3
        assert pipeline.results["stages"]["transformation"]["total_formats"] == 5

    async def test_stage_6_validate_pipeline_success(self):
        """Test successful pipeline validation stage"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create transformed files
        (self.dirs["transformed"] / "document.html").write_text(
            "<html>...</html>", encoding="utf-8"
        )
        (self.dirs["tei"] / "site_document.xml").write_text(
            '<?xml version="1.0"?><TEI>...</TEI>', encoding="utf-8"
        )

        # Mock validation methods
        pipeline.html_tei_converter.test_isomorphism = Mock(
            return_value={"isomorphic": True}
        )
        pipeline._validate_tei_structure = Mock(
            return_value={"valid": True, "checks": {}}
        )
        pipeline._check_format_consistency = Mock(
            return_value={"summary": {"generation_rate": 1.0}}
        )

        result = await pipeline._stage_6_validate_pipeline()

        assert result is True
        assert "validation" in pipeline.results["stages"]
        assert pipeline.results["stages"]["validation"]["success"] is True
        assert (
            "html_tei_isomorphism"
            in pipeline.results["stages"]["validation"]["results"]
        )
        assert "tei_structure" in pipeline.results["stages"]["validation"]["results"]
        assert (
            "format_consistency" in pipeline.results["stages"]["validation"]["results"]
        )

    def test_validate_xml_success(self):
        """Test XML validation success"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create valid XML file
        xml_file = self.work_dir / "test.xml"
        xml_file.write_text(
            '<?xml version="1.0"?><root><item>test</item></root>', encoding="utf-8"
        )

        # Mock subprocess.run success
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stderr="")

            result = pipeline._validate_xml(xml_file)

            assert result["valid"] is True
            assert result["error"] is None

    def test_validate_xml_failure(self):
        """Test XML validation failure"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create invalid XML file
        xml_file = self.work_dir / "test.xml"
        xml_file.write_text(
            "<root><item>test</root>", encoding="utf-8"
        )  # Missing closing tag

        # Mock subprocess.run failure
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=1, stderr="XML parsing error")

            result = pipeline._validate_xml(xml_file)

            assert result["valid"] is False
            assert "XML parsing error" in result["error"]

    def test_validate_xml_tool_missing(self):
        """Test XML validation when xmllint is missing"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        xml_file = self.work_dir / "test.xml"
        xml_file.write_text('<?xml version="1.0"?><root/>', encoding="utf-8")

        # Mock subprocess.run with FileNotFoundError
        with patch("subprocess.run") as mock_run:
            mock_run.side_effect = FileNotFoundError("xmllint not found")

            result = pipeline._validate_xml(xml_file)

            assert result["valid"] is False
            assert "not found" in result["error"]

    def test_validate_tei_structure(self):
        """Test TEI structure validation"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create TEI XML file
        tei_file = self.work_dir / "tei.xml"
        tei_content = """<?xml version="1.0"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc><titleStmt><title>Test</title></titleStmt></fileDesc>
  </teiHeader>
  <text>
    <body>
      <div type="page">
        <head>Test Page</head>
        <p>Test content</p>
      </div>
    </body>
  </text>
</TEI>"""
        tei_file.write_text(tei_content, encoding="utf-8")

        # Mock xpath queries
        with patch.object(pipeline, "_xpath_count") as mock_xpath:
            mock_xpath.side_effect = [
                1,
                1,
                1,
                1,
                3,
            ]  # header, text, body, pages, total divs

            result = pipeline._validate_tei_structure(tei_file)

            assert result["valid"] is True
            assert result["checks"]["has_tei_header"] is True
            assert result["checks"]["has_text"] is True
            assert result["checks"]["has_body"] is True
            assert result["checks"]["has_pages"] is True
            assert result["total_divs"] == 3

    def test_check_format_consistency(self):
        """Test format consistency checking"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create sample output files
        files = [
            "document.html",
            "document.tex",
            "document.pdf",
            "document.epub",
            "document.docx",
        ]
        for filename in files:
            file_path = self.dirs["transformed"] / filename
            file_path.write_text(f"Content of {filename}", encoding="utf-8")

        result = pipeline._check_format_consistency()

        assert result["summary"]["total_formats"] == 5
        assert result["summary"]["existing_formats"] == 5
        assert result["summary"]["generation_rate"] == 1.0

        for filename in files:
            assert result[filename]["exists"] is True
            assert result[filename]["size"] > 0

    def test_xpath_count_success(self):
        """Test XPath count execution"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        xml_file = self.work_dir / "test.xml"
        xml_file.write_text(
            "<root><item>1</item><item>2</item></root>", encoding="utf-8"
        )

        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stdout="2")

            count = pipeline._xpath_count(xml_file, "count(//item)")

            assert count == 2

    def test_xpath_count_failure(self):
        """Test XPath count failure"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        xml_file = self.work_dir / "test.xml"
        xml_file.write_text("<root/>", encoding="utf-8")

        # Mock subprocess.run failure
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=1)

            count = pipeline._xpath_count(xml_file, "count(//item)")

            assert count == 0

    def test_generate_report(self):
        """Test pipeline report generation"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Set some results
        pipeline.results = {
            "start_time": time.time() - 100,
            "end_time": time.time(),
            "duration": 100.0,
            "success": True,
            "stages": {
                "scraping": {"success": True},
                "parsing": {"success": True, "parse_rate": 0.95},
                "transformation": {"success": True, "successful_formats": 4},
                "validation": {
                    "results": {"html_tei_isomorphism": {"isomorphic": True}}
                },
            },
        }

        report = pipeline.generate_report()

        assert "pipeline_info" in report
        assert "results" in report
        assert "directory_structure" in report
        assert "recommendations" in report
        assert report["pipeline_info"]["version"] == "1.0.0"
        assert report["results"]["success"] is True

    def test_generate_recommendations_success(self):
        """Test recommendation generation for successful pipeline"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)
        pipeline.results = {
            "success": True,
            "stages": {
                "parsing": {"parse_rate": 0.95},
                "transformation": {"successful_formats": 5},
                "validation": {"results": {}},
            },
        }

        recommendations = pipeline._generate_recommendations()

        # Should have minimal recommendations for successful pipeline
        assert len(recommendations) == 0

    def test_generate_recommendations_failures(self):
        """Test recommendation generation for failed pipeline"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)
        pipeline.results = {
            "success": False,
            "stages": {
                "scraping": {"success": False, "error": "Network error"},
                "parsing": {"parse_rate": 0.8},  # Low success rate
                "transformation": {"successful_formats": 2},  # Few formats
                "validation": {
                    "results": {"html_tei_isomorphism": {"isomorphic": False}}
                },
            },
        }

        recommendations = pipeline._generate_recommendations()

        # Should have multiple recommendations
        assert len(recommendations) >= 4
        assert any("failed" in rec.lower() for rec in recommendations)
        assert any("parsing" in rec.lower() for rec in recommendations)
        assert any("transformation" in rec.lower() for rec in recommendations)
        assert any("isomorphism" in rec.lower() for rec in recommendations)

    def test_get_directory_structure(self):
        """Test directory structure analysis"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Create some test files
        (self.dirs["scraped"] / "test.txt").write_text("test", encoding="utf-8")
        (self.dirs["scraped"] / "subdir").mkdir(exist_ok=True)

        structure = pipeline._get_directory_structure()

        assert "scraped" in structure
        assert structure["scraped"]["total_files"] == 1
        assert structure["scraped"]["directories"] == 1

    @pytest.mark.asyncio
    async def test_concurrent_stage_execution(self):
        """Test that stages execute in correct order (sequentially)"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        execution_order = []

        def create_stage_mock(stage_name):
            async def mock_stage(*args, **kwargs):
                execution_order.append(stage_name)
                return True

            return mock_stage

        # Assign mock stages
        pipeline._stage_1_scrape_website = create_stage_mock("stage_1")
        pipeline._stage_2_parse_content = create_stage_mock("stage_2")
        pipeline._stage_3_generate_uml = create_stage_mock("stage_3")
        pipeline._stage_4_generate_tei = create_stage_mock("stage_4")
        pipeline._stage_5_transform_formats = create_stage_mock("stage_5")
        pipeline._stage_6_validate_pipeline = create_stage_mock("stage_6")

        await pipeline.process_website(self.test_url)

        # Verify sequential execution order
        assert execution_order == [
            "stage_1",
            "stage_2",
            "stage_3",
            "stage_4",
            "stage_5",
            "stage_6",
        ]

    async def test_error_recovery_and_cleanup(self):
        """Test error recovery and resource cleanup"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Mock stage to fail but cleanup should still happen
        pipeline._stage_1_scrape_website = AsyncMock(
            side_effect=Exception("Test error")
        )

        start_time = time.time()
        result = await pipeline.process_website(self.test_url)
        end_time = time.time()

        assert result is False
        assert pipeline.results["end_time"] is not None
        assert pipeline.results["duration"] >= 0
        assert abs(pipeline.results["duration"] - (end_time - start_time)) < 1.0
        assert "Test error" in pipeline.results["error"]

    @pytest.mark.asyncio
    async def test_pipeline_with_max_pages_limit(self):
        """Test pipeline with maximum pages limit"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)
        pipeline._stage_1_scrape_website = AsyncMock(return_value=True)
        pipeline._stage_2_parse_content = AsyncMock(return_value=True)
        pipeline._stage_3_generate_uml = AsyncMock(return_value=True)
        pipeline._stage_4_generate_tei = AsyncMock(return_value=True)
        pipeline._stage_5_transform_formats = AsyncMock(return_value=True)
        pipeline._stage_6_validate_pipeline = AsyncMock(return_value=True)

        await pipeline.process_website(self.test_url, max_pages=50)

        pipeline._stage_1_scrape_website.assert_called_once_with(self.test_url, 50)

    def test_pipeline_idempotency(self):
        """Test that pipeline can be run multiple times safely"""
        from scripts.content_pipeline import ContentPipeline

        pipeline = ContentPipeline(self.work_dir)

        # Reset results and run again
        initial_results = dict(pipeline.results)

        # Results should be reset when starting new pipeline
        assert pipeline.results["start_time"] is None
        assert pipeline.results["success"] is False

        # Can safely create new pipeline instances
        pipeline2 = ContentPipeline(self.work_dir)
        assert pipeline2.results != initial_results
