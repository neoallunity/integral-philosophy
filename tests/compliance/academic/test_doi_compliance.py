"""
DOI (Digital Object Identifier) Compliance Tests for Integral Philosophy Publishing System

This module validates DOI compliance according to ISO 26324 standard and Crossref best practices
for academic publishing in the philosophy domain.
"""

import pytest
import re
import json
from pathlib import Path
from typing import Dict, List, Any, Optional
from urllib.parse import urlparse
import requests
from unittest.mock import Mock, patch

from ...utils.base_test_classes import BaseTestCase


class TestDOIStandardsCompliance(BaseTestCase):
    """Test DOI standards compliance"""

    def create_sample_doi_metadata(self) -> Dict[str, Any]:
        """Create sample DOI metadata for testing"""
        return {
            "doi": "10.1234/philosophy.2024.001",
            "creators": [
                {
                    "name": "John Doe",
                    "orcid": "0000-0002-1825-0097",
                    "affiliation": "University of Philosophy",
                }
            ],
            "title": "The Nature of Being: A Phenomenological Analysis",
            "publication_year": "2024",
            "resource_type": "journal-article",
            "license": "http://creativecommons.org/licenses/by/4.0/",
            "publisher": "Integral Philosophy Publishing",
            "volume": "1",
            "issue": "1",
            "pages": "1-25",
            "keywords": ["phenomenology", "ontology", "being"],
            "abstract": "This article explores the nature of being through phenomenological analysis...",
        }

    def test_doi_format_validation(self):
        """Test DOI format validation according to ISO 26324"""
        metadata = self.create_sample_doi_metadata()
        doi = metadata["doi"]

        # Test DOI format pattern
        doi_pattern = r"^10\.\d{4,9}/.+"
        assert re.match(doi_pattern, doi), f"Invalid DOI format: {doi}"

        # Test directory and registrant components
        if "/" in doi:
            prefix, suffix = doi.split("/", 1)

            # Validate prefix (10. + registrant code)
            assert prefix.startswith("10."), "DOI prefix must start with 10."
            registrant_code = prefix[3:]
            assert registrant_code.isdigit(), "Registrant code must be numeric"
            assert len(registrant_code) >= 4, (
                "Registrant code must be at least 4 digits"
            )

            # Validate suffix (can contain various characters)
            assert len(suffix) > 0, "DOI suffix cannot be empty"

            # Test for invalid characters in suffix
            invalid_chars = [" ", "\t", "\n", "\r"]
            for char in invalid_chars:
                assert char not in suffix, (
                    f"DOI suffix contains invalid character: {repr(char)}"
                )

    def test_doi_uniqueness(self):
        """Test DOI uniqueness requirements"""
        metadata = self.create_sample_doi_metadata()
        doi = metadata["doi"]

        # Mock DOI registry for testing
        existing_dois = [
            "10.1234/philosophy.2024.001",
            "10.1234/philosophy.2024.002",
            "10.5678/ethics.2024.001",
        ]

        # Check uniqueness
        assert doi not in existing_dois, f"DOI {doi} already exists"

        # Test format-based uniqueness
        year_pattern = re.search(r"\.(\d{4})\.", doi)
        if year_pattern:
            year = year_pattern.group(1)
            assert len(year) == 4 and year.isdigit(), "Year in DOI should be 4 digits"

    def test_doi_resolution_structure(self):
        """Test DOI resolution structure"""
        metadata = self.create_sample_doi_metadata()
        doi = metadata["doi"]

        # Test DOI resolution URL structure
        resolver_url = f"https://doi.org/{doi}"

        # Validate URL format
        parsed = urlparse(resolver_url)
        assert parsed.scheme == "https", "DOI resolver should use HTTPS"
        assert parsed.netloc == "doi.org", "DOI resolver should use doi.org"
        assert parsed.path == f"/{doi}", "DOI resolver path should include full DOI"

    def test_crossref_metadata_compliance(self):
        """Test Crossref metadata compliance"""
        metadata = self.create_sample_doi_metadata()

        # Required Crossref metadata fields
        required_fields = [
            "doi",
            "creators",
            "title",
            "publication_year",
            "resource_type",
        ]

        for field in required_fields:
            assert field in metadata, f"Required Crossref field missing: {field}"

        # Validate creators structure
        creators = metadata["creators"]
        assert isinstance(creators, list), "Creators must be a list"
        assert len(creators) > 0, "At least one creator is required"

        for creator in creators:
            assert "name" in creator, "Creator must have a name"
            assert isinstance(creator["name"], str), "Creator name must be a string"
            assert len(creator["name"].strip()) > 0, "Creator name cannot be empty"

        # Validate resource type
        valid_types = [
            "journal-article",
            "book",
            "book-chapter",
            "proceedings-article",
            "dataset",
            "software",
            "other",
        ]
        assert metadata["resource_type"] in valid_types, (
            f"Invalid resource type: {metadata['resource_type']}"
        )

    def test_doi_registration_requirements(self):
        """Test DOI registration requirements"""
        metadata = self.create_sample_doi_metadata()

        # Test publication year format
        year = str(metadata["publication_year"])
        assert len(year) == 4, "Publication year must be 4 digits"
        assert year.isdigit(), "Publication year must be numeric"

        # Test title requirements
        title = metadata["title"]
        assert isinstance(title, str), "Title must be a string"
        assert len(title.strip()) > 0, "Title cannot be empty"
        assert len(title) <= 5000, "Title is too long (max 5000 characters)"

        # Test publisher
        publisher = metadata.get("publisher")
        if publisher:
            assert isinstance(publisher, str), "Publisher must be a string"
            assert len(publisher.strip()) > 0, "Publisher cannot be empty"

    def test_license_compliance(self):
        """Test license compliance for DOI registration"""
        metadata = self.create_sample_doi_metadata()
        license_url = metadata.get("license")

        if license_url:
            # Validate URL format
            parsed = urlparse(license_url)
            assert parsed.scheme in ["http", "https"], "License URL must use HTTP/HTTPS"
            assert parsed.netloc, "License URL must have a domain"

            # Test for common license URLs
            common_licenses = ["creativecommons.org", "opensource.org", "gnu.org"]

            is_common_license = any(
                license in license_url.lower() for license in common_licenses
            )
            if not is_common_license:
                pytest.warns(UserWarning, f"Uncommon license URL: {license_url}")


class TestDOIMetadata(BaseTestCase):
    """Test DOI metadata completeness and accuracy"""

    def test_creator_metadata_validation(self):
        """Test creator metadata validation"""
        creators = [
            {
                "name": "Jane Smith",
                "orcid": "0000-0002-1825-0097",
                "affiliation": "Philosophy Department, University of Example",
                "sequence": "first",
            },
            {
                "name": "Bob Johnson",
                "affiliation": "Institute of Advanced Studies",
                "sequence": "additional",
            },
        ]

        for creator in creators:
            # Validate name
            name = creator["name"]
            assert isinstance(name, str), "Creator name must be string"
            assert len(name.strip()) > 0, "Creator name cannot be empty"

            # Validate ORCID if present
            orcid = creator.get("orcid")
            if orcid:
                orcid_pattern = r"^\d{4}-\d{4}-\d{4}-\d{3}[\dX]$"
                assert re.match(orcid_pattern, orcid), f"Invalid ORCID format: {orcid}"

            # Validate affiliation if present
            affiliation = creator.get("affiliation")
            if affiliation:
                assert isinstance(affiliation, str), "Affiliation must be string"
                assert len(affiliation.strip()) > 0, "Affiliation cannot be empty"

    def test_title_metadata_validation(self):
        """Test title metadata validation"""
        test_titles = [
            "Being and Time",
            "The Phenomenology of Spirit: A Critical Analysis",
            "Ethics in the Age of Artificial Intelligence: Philosophical Considerations",
        ]

        for title in test_titles:
            assert isinstance(title, str), "Title must be string"
            assert len(title.strip()) > 0, "Title cannot be empty"

            # Test for HTML/XML tags (should be encoded, not raw)
            html_tags = re.findall(r"<[^>]+>", title)
            assert len(html_tags) == 0, f"Title contains raw HTML tags: {title}"

            # Test for reasonable length
            assert len(title) <= 5000, f"Title too long: {len(title)} characters"

    def test_abstract_metadata_validation(self):
        """Test abstract metadata validation"""
        abstract = """This article examines the fundamental questions of ontology through 
        a phenomenological lens. By analyzing the structures of consciousness and 
        intentional directedness, we develop a new framework for understanding being."""

        # Test abstract length
        assert len(abstract.strip()) > 0, "Abstract cannot be empty"
        assert len(abstract) <= 5000, "Abstract too long (max 5000 characters)"

        # Test for meaningful content
        words = abstract.split()
        assert len(words) >= 50, "Abstract should be at least 50 words"
        assert len(words) <= 500, "Abstract should not exceed 500 words"

    def test_keyword_metadata_validation(self):
        """Test keyword metadata validation"""
        keywords = [
            "phenomenology",
            "ontology",
            "consciousness",
            "Heidegger",
            "existentialism",
        ]

        assert isinstance(keywords, list), "Keywords must be a list"
        assert len(keywords) > 0, "At least one keyword required"
        assert len(keywords) <= 10, "Too many keywords (max 10)"

        for keyword in keywords:
            assert isinstance(keyword, str), "Keyword must be string"
            assert len(keyword.strip()) > 0, "Keyword cannot be empty"
            assert len(keyword) <= 100, "Keyword too long (max 100 characters)"

            # Test for valid characters
            keyword_pattern = r"^[a-zA-Z0-9\s\-_]+$"
            assert re.match(keyword_pattern, keyword), (
                f"Invalid keyword format: {keyword}"
            )

    def test_bibliographic_metadata(self):
        """Test bibliographic metadata"""
        bibliographic_data = {
            "volume": "12",
            "issue": "3",
            "pages": "45-67",
            "issn": "1234-5678",
            "isbn": "978-0-123456-78-9",
        }

        # Validate volume
        volume = bibliographic_data.get("volume")
        if volume:
            assert isinstance(volume, str), "Volume must be string"
            assert re.match(r"^\d+$", volume), f"Invalid volume format: {volume}"

        # Validate issue
        issue = bibliographic_data.get("issue")
        if issue:
            assert isinstance(issue, str), "Issue must be string"
            assert re.match(r"^\d+$", issue), f"Invalid issue format: {issue}"

        # Validate pages
        pages = bibliographic_data.get("pages")
        if pages:
            assert isinstance(pages, str), "Pages must be string"
            pages_pattern = r"^\d+(\-\d+)?$"
            assert re.match(pages_pattern, pages), f"Invalid pages format: {pages}"

        # Validate ISSN
        issn = bibliographic_data.get("issn")
        if issn:
            issn_clean = issn.replace("-", "")
            assert len(issn_clean) == 8, "ISSN must be 8 digits"
            assert issn_clean.isdigit(), "ISSN must contain only digits"

    def test_publication_date_validation(self):
        """Test publication date validation"""
        dates = [
            "2024-01-15",  # Full date
            "2024-03",  # Year-month
            "2024",  # Year only
        ]

        for date in dates:
            assert isinstance(date, str), "Publication date must be string"

            # Test different date formats
            if len(date) == 4:  # Year only
                assert date.isdigit(), "Year must be numeric"
                year = int(date)
                assert 1900 <= year <= 2100, f"Year out of range: {year}"

            elif len(date) == 7:  # Year-month
                year, month = date.split("-")
                assert year.isdigit() and month.isdigit(), "Year-month must be numeric"
                assert 1 <= int(month) <= 12, f"Invalid month: {month}"

            elif len(date) == 10:  # Full date
                import datetime

                try:
                    datetime.datetime.strptime(date, "%Y-%m-%d")
                except ValueError:
                    pytest.fail(f"Invalid date format: {date}")


class TestDOIIntegration(BaseTestCase):
    """Test DOI integration with publishing system"""

    def test_doi_generation_workflow(self):
        """Test DOI generation workflow"""
        # Simulate DOI generation process
        base_doi = "10.1234"
        publisher_code = "philosophy"
        year = "2024"
        article_number = "001"

        generated_doi = f"{base_doi}/{publisher_code}.{year}.{article_number}"

        # Validate generated DOI
        doi_pattern = r"^10\.\d{4,9}/.+"
        assert re.match(doi_pattern, generated_doi), (
            f"Generated DOI invalid: {generated_doi}"
        )

        # Test uniqueness
        existing_dois = [
            f"{base_doi}/philosophy.2024.000",
            f"{base_doi}/philosophy.2024.002",
        ]
        assert generated_doi not in existing_dois, "Generated DOI already exists"

    def test_doi_registration_simulation(self):
        """Test DOI registration process simulation"""
        metadata = self.create_sample_doi_metadata()

        # Mock registration request
        registration_data = {
            "doi": metadata["doi"],
            "url": f"https://philosophy.journal.org/article/{metadata['doi']}",
            "metadata": metadata,
        }

        # Validate registration data
        assert "doi" in registration_data, "Registration missing DOI"
        assert "url" in registration_data, "Registration missing URL"
        assert "metadata" in registration_data, "Registration missing metadata"

        # Validate URL format
        url = registration_data["url"]
        parsed = urlparse(url)
        assert parsed.scheme in ["http", "https"], "URL must use HTTP/HTTPS"
        assert parsed.netloc, "URL must have domain"

    def test_doi_resolution_service(self):
        """Test DOI resolution service integration"""
        test_doi = "10.1234/philosophy.2024.001"
        target_url = "https://philosophy.journal.org/articles/2024/001"

        # Mock DOI resolution
        resolution_service = {test_doi: target_url}

        # Test resolution
        resolved_url = resolution_service.get(test_doi)
        assert resolved_url == target_url, f"DOI resolution failed for {test_doi}"

        # Test invalid DOI
        invalid_doi = "10.9999/invalid.doi"
        resolved_url = resolution_service.get(invalid_doi)
        assert resolved_url is None, "Invalid DOI should not resolve"

    def test_doi_update_mechanism(self):
        """Test DOI update mechanism"""
        initial_metadata = self.create_sample_doi_metadata()
        updated_metadata = initial_metadata.copy()
        updated_metadata["title"] = "Updated Title: The Nature of Being"
        updated_metadata["keywords"] = ["phenomenology", "ontology", "updated"]

        # Test update validation
        assert initial_metadata["doi"] == updated_metadata["doi"], (
            "DOI should not change during update"
        )
        assert initial_metadata["title"] != updated_metadata["title"], (
            "Title should be updated"
        )

        # Fields that shouldn't change
        immutable_fields = ["doi", "publication_year"]
        for field in immutable_fields:
            assert initial_metadata[field] == updated_metadata[field], (
                f"Field {field} should not change"
            )

    def test_doi_crossref_submission(self):
        """Test Crossref submission format"""
        metadata = self.create_sample_doi_metadata()

        # Create Crossref submission format
        crossref_submission = {
            "doi_batch_id": f"philosophy_{metadata['publication_year']}_batch_001",
            "timestamp": "2024-01-15T10:30:00Z",
            "deposits": [
                {
                    "doi": metadata["doi"],
                    "resource_type": metadata["resource_type"],
                    "title": metadata["title"],
                    "creators": metadata["creators"],
                    "publication_year": metadata["publication_year"],
                }
            ],
        }

        # Validate submission format
        assert "doi_batch_id" in crossref_submission, "Missing batch ID"
        assert "deposits" in crossref_submission, "Missing deposits"
        assert len(crossref_submission["deposits"]) > 0, "At least one deposit required"

        # Validate deposit structure
        deposit = crossref_submission["deposits"][0]
        required_deposit_fields = ["doi", "resource_type", "title", "creators"]
        for field in required_deposit_fields:
            assert field in deposit, f"Deposit missing required field: {field}"


class TestDOIQualityAssurance(BaseTestCase):
    """Test DOI quality assurance and validation"""

    def test_doi_consistency_checks(self):
        """Test DOI consistency across metadata"""
        metadata = self.create_sample_doi_metadata()
        doi = metadata["doi"]
        year = str(metadata["publication_year"])

        # Check year consistency
        doi_year_pattern = re.search(r"\.(\d{4})\.", doi)
        if doi_year_pattern:
            doi_year = doi_year_pattern.group(1)
            assert doi_year == year, (
                f"Year mismatch: DOI has {doi_year}, metadata has {year}"
            )

    def test_doi_data_integrity(self):
        """Test DOI data integrity"""
        metadata = self.create_sample_doi_metadata()

        # Check for required fields with non-empty values
        required_non_empty = ["title", "creators", "resource_type"]
        for field in required_non_empty:
            value = metadata[field]
            if isinstance(value, str):
                assert len(value.strip()) > 0, f"Field {field} is empty"
            elif isinstance(value, list):
                assert len(value) > 0, f"Field {field} is empty list"

    def test_doi_error_handling(self):
        """Test DOI error handling scenarios"""
        # Test invalid DOI formats
        invalid_dois = [
            "not-a-doi",
            "10.1234",  # Missing suffix
            "10.1234/",  # Empty suffix
            "10.1234/invalid chars",  # Invalid characters
            "10.123/short",  # Short registrant code
        ]

        for invalid_doi in invalid_dois:
            doi_pattern = r"^10\.\d{4,9}/.+"
            assert not re.match(doi_pattern, invalid_doi), (
                f"Invalid DOI should not match pattern: {invalid_doi}"
            )

    def test_doi_batch_validation(self):
        """Test DOI batch validation"""
        dois_metadata = [
            self.create_sample_doi_metadata(),
            {
                **self.create_sample_doi_metadata(),
                "doi": "10.1234/philosophy.2024.002",
                "title": "Ethics in Modern Philosophy",
            },
        ]

        # Check for duplicates in batch
        dois = [item["doi"] for item in dois_metadata]
        assert len(dois) == len(set(dois)), "Duplicate DOIs found in batch"

        # Validate each DOI format
        for metadata in dois_metadata:
            doi = metadata["doi"]
            doi_pattern = r"^10\.\d{4,9}/.+"
            assert re.match(doi_pattern, doi), f"Invalid DOI in batch: {doi}"
