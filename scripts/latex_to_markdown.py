#!/usr/bin/env python3
"""
LaTeX to Markdown+TeX Source Reconstruction Tool
Reconstructs original markdown+TeX source from LaTeX files in the Integral Philosophy project.
"""

import os
import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import json


class TeXAnalyzer:
    """Analyzes LaTeX files and reconstructs markdown+TeX source."""

    def __init__(self):
        self.content_map = {}
        self.structure_map = {}
        self.math_content = {}
        self.metadata = {}

    def analyze_file(self, filepath: Path) -> Dict:
        """Analyze individual LaTeX file."""
        try:
            with open(filepath, "r", encoding="utf-8") as f:
                content = f.read()
        except UnicodeDecodeError:
            try:
                with open(filepath, "r", encoding="latin1") as f:
                    content = f.read()
            except Exception as e:
                return {"error": f"Cannot read file: {e}"}

        analysis = {
            "path": str(filepath),
            "size": len(content),
            "sections": self._extract_sections(content),
            "math_content": self._extract_math(content),
            "citations": self._extract_citations(content),
            "tables": self._extract_tables(content),
            "figures": self._extract_figures(content),
            "languages": self._detect_languages(content),
            "structure": self._analyze_structure(content),
            "type": self._classify_file(filepath, content),
        }

        return analysis

    def _classify_file(self, filepath: Path, content: str) -> str:
        """Classify file type based on path and content."""
        path_parts = filepath.parts

        if "cfg" in path_parts:
            return "configuration"
        elif "frontmatter" in path_parts:
            return "frontmatter"
        elif "chapters" in path_parts:
            if any(
                template in content for template in ["JournalArticle", "addarticle"]
            ):
                return "article"
            else:
                return "chapter"
        elif "articles" in path_parts:
            return "full_article"
        elif filepath.name == "main.tex":
            return "main_document"
        elif filepath.name == "preamble.tex":
            return "preamble"
        else:
            return "other"

    def _extract_sections(self, content: str) -> List[Dict]:
        """Extract section hierarchy."""
        sections = []
        section_pattern = (
            r"\\(section|subsection|subsubsection|paragraph)\*?\{([^}]+)\}"
        )

        for match in re.finditer(section_pattern, content):
            sections.append(
                {
                    "level": match.group(1),
                    "title": match.group(2).strip(),
                    "type": "structure",
                }
            )

        return sections

    def _extract_math(self, content: str) -> List[Dict]:
        """Extract mathematical content."""
        math_content = []

        # Display math
        display_pattern = r"\\\[([^\\]+)\\\]"
        for match in re.finditer(display_pattern, content):
            math_content.append(
                {
                    "type": "display_math",
                    "content": match.group(1).strip(),
                    "format": "tex",
                }
            )

        # Inline math
        inline_pattern = r"\$([^$]+)\$"
        for match in re.finditer(inline_pattern, content):
            math_content.append(
                {
                    "type": "inline_math",
                    "content": match.group(1).strip(),
                    "format": "tex",
                }
            )

        # Equation environments
        equation_pattern = r"\\begin\{(equation|align|gather)\}([^\\]+)\\end\{\1\}"
        for match in re.finditer(equation_pattern, content):
            math_content.append(
                {
                    "type": "equation_environment",
                    "env": match.group(1),
                    "content": match.group(2).strip(),
                    "format": "tex",
                }
            )

        return math_content

    def _extract_citations(self, content: str) -> List[str]:
        """Extract bibliographic citations."""
        citations = []

        # Biblatex citations
        citation_pattern = r"\\(cite|citep|citet|parencite)\{([^}]+)\}"
        for match in re.finditer(citation_pattern, content):
            citations.append(match.group(2))

        return list(set(citations))

    def _extract_tables(self, content: str) -> List[Dict]:
        """Extract table structures."""
        tables = []

        table_pattern = r"\\begin\{tabular\}.*?\\end\{tabular\}"
        for match in re.finditer(table_pattern, content, re.DOTALL):
            tables.append({"type": "table", "content": match.group(0), "format": "tex"})

        return tables

    def _extract_figures(self, content: str) -> List[Dict]:
        """Extract figure references."""
        figures = []

        figure_pattern = r"\\begin\{figure\}.*?\\end\{figure\}"
        for match in re.finditer(figure_pattern, content, re.DOTALL):
            figures.append(
                {"type": "figure", "content": match.group(0), "format": "tex"}
            )

        # Includegraphics
        include_pattern = r"\\includegraphics[^{]*\{([^}]+)\}"
        for match in re.finditer(include_pattern, content):
            figures.append({"type": "image", "file": match.group(1), "format": "tex"})

        return figures

    def _detect_languages(self, content: str) -> List[str]:
        """Detect content languages."""
        languages = []

        # Check for Russian text
        if re.search(r"[а-яА-Я]", content):
            languages.append("russian")

        # Check for English text
        if re.search(r"[a-zA-Z]{3,}", content) and "begin{english}" in content:
            languages.append("english")

        # Check for other language switches
        if "begin{russian}" in content:
            languages.append("russian")
        if "begin{english}" in content:
            languages.append("english")

        return list(set(languages))

    def _analyze_structure(self, content: str) -> Dict:
        """Analyze document structure."""
        structure = {
            "has_abstract": bool(
                re.search(r"\\(RUAbstract|ENAbstract|abstract)", content)
            ),
            "has_keywords": bool(re.search(r"(Keywords|Ключевые слова)", content)),
            "has_bibliography": bool(
                re.search(r"\\(bibliography|printbibliography)", content)
            ),
            "has_toc": bool(re.search(r"\\tableofcontents|\\printtoc", content)),
            "article_count": len(re.findall(r"\\(addarticle|JournalArticle)", content)),
            "page_count_estimate": len(content) // 2000,  # Rough estimate
        }

        return structure

    def reconstruct_markdown_tex(self, analysis: Dict) -> str:
        """Reconstruct markdown+TeX from analysis."""
        if analysis.get("error"):
            return f"# Error\n\n{analysis['error']}"

        file_type = analysis["type"]

        if file_type == "configuration":
            return self._reconstruct_config(analysis)
        elif file_type in ["article", "full_article"]:
            return self._reconstruct_article(analysis)
        elif file_type == "frontmatter":
            return self._reconstruct_frontmatter(analysis)
        elif file_type == "main_document":
            return self._reconstruct_main(analysis)
        else:
            return self._reconstruct_generic(analysis)

    def _reconstruct_article(self, analysis: Dict) -> str:
        """Reconstruct article content as markdown+TeX."""
        md_content = []

        # Extract title from first section or metadata
        sections = analysis["sections"]
        if sections:
            title = sections[0]["title"]
            md_content.append(f"# {title}")
            md_content.append("")

        # Add abstract if present
        if analysis["structure"]["has_abstract"]:
            # This would need more sophisticated parsing
            md_content.append("## Abstract")
            md_content.append("")
            md_content.append("[Abstract content to be extracted from LaTeX]")
            md_content.append("")

        # Process sections in order
        current_level = 1
        for section in analysis["sections"]:
            level_map = {
                "section": 1,
                "subsection": 2,
                "subsubsection": 3,
                "paragraph": 4,
            }

            level = level_map.get(section["level"], 2)
            md_content.append(f"{'#' * level} {section['title']}")
            md_content.append("")

        # Add mathematical content
        if analysis["math_content"]:
            md_content.append("## Mathematical Content")
            md_content.append("")
            for math_item in analysis["math_content"]:
                if math_item["type"] == "display_math":
                    md_content.append(f"$$\n{math_item['content']}\n$$")
                    md_content.append("")
                elif math_item["type"] == "inline_math":
                    md_content.append(f"Inline math: ${math_item['content']}$")
                    md_content.append("")

        # Add citations
        if analysis["citations"]:
            md_content.append("## References")
            md_content.append("")
            for citation in analysis["citations"]:
                md_content.append(f"- [{citation}]")
            md_content.append("")

        return "\n".join(md_content)

    def _reconstruct_config(self, analysis: Dict) -> str:
        """Reconstruct configuration as commented LaTeX."""
        content = f"# Configuration File: {Path(analysis['path']).name}\n\n"
        content += "```latex\n"
        content += "% This is a LaTeX configuration file\n"
        content += "% Contains package imports, settings, and custom commands\n"
        content += "% Should be preserved as-is for TeX compatibility\n\n"
        content += "% [Full LaTeX content would be preserved here]\n"
        content += "```"
        return content

    def _reconstruct_frontmatter(self, analysis: Dict) -> str:
        """Reconstruct frontmatter content."""
        return (
            f"# Frontmatter\n\n[Frontmatter content from {Path(analysis['path']).name}]"
        )

    def _reconstruct_main(self, analysis: Dict) -> str:
        """Reconstruct main document structure."""
        md_content = []
        md_content.append("# Integral Philosophy Journal")
        md_content.append("")
        md_content.append("## Document Structure")
        md_content.append("")

        structure = analysis["structure"]
        md_content.append(f"- Articles: {structure['article_count']}")
        md_content.append(f"- Has abstracts: {structure['has_abstract']}")
        md_content.append(f"- Has bibliography: {structure['has_bibliography']}")
        md_content.append(f"- Languages: {', '.join(analysis['languages'])}")
        md_content.append("")

        return "\n".join(md_content)

    def _reconstruct_generic(self, analysis: Dict) -> str:
        """Generic reconstruction for other file types."""
        return f"# {Path(analysis['path']).name}\n\n[Generic content reconstruction]"


def main():
    """Main reconstruction function."""
    analyzer = TeXAnalyzer()

    # Find all .tex files
    tex_files = list(Path(".").rglob("*.tex"))
    print(f"Found {len(tex_files)} LaTeX files")

    reconstruction_results = {}

    for tex_file in tex_files:
        print(f"Analyzing: {tex_file}")
        analysis = analyzer.analyze_file(tex_file)
        reconstruction = analyzer.reconstruct_markdown_tex(analysis)

        reconstruction_results[str(tex_file)] = {
            "analysis": analysis,
            "reconstruction": reconstruction,
        }

    # Generate summary
    summary = {
        "total_files": len(tex_files),
        "file_types": {},
        "languages_found": set(),
        "total_math_content": 0,
        "total_articles": 0,
    }

    for file_data in reconstruction_results.values():
        analysis = file_data["analysis"]
        file_type = analysis.get("type", "unknown")
        summary["file_types"][file_type] = summary["file_types"].get(file_type, 0) + 1
        summary["languages_found"].update(analysis.get("languages", []))
        summary["total_math_content"] += len(analysis.get("math_content", []))
        summary["total_articles"] += analysis.get("structure", {}).get(
            "article_count", 0
        )

    summary["languages_found"] = list(summary["languages_found"])

    # Save results
    output_dir = Path("reconstructed_sources")
    output_dir.mkdir(exist_ok=True)

    # Save summary
    with open(output_dir / "reconstruction_summary.json", "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)

    # Save individual reconstructions
    for filepath, data in reconstruction_results.items():
        safe_name = str(Path(filepath).stem) + ".md"
        safe_path = output_dir / safe_name

        try:
            with open(safe_path, "w", encoding="utf-8") as f:
                f.write(data["reconstruction"])
        except Exception as e:
            print(f"Error saving {safe_path}: {e}")

    print(f"\nReconstruction Summary:")
    print(f"- Total files processed: {summary['total_files']}")
    print(f"- File types: {summary['file_types']}")
    print(f"- Languages: {summary['languages_found']}")
    print(f"- Mathematical expressions: {summary['total_math_content']}")
    print(f"- Articles found: {summary['total_articles']}")
    print(f"- Output saved to: {output_dir}")


if __name__ == "__main__":
    main()
