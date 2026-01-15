#!/usr/bin/env python3
"""
Quick Reconstruction Tool - Fixed version
"""

import json
from pathlib import Path
import re


def quick_reconstruct():
    """Quick reconstruction of articles to markdown."""

    # Find article files
    article_files = list(Path("articles").glob("*/main.tex"))
    output_dir = Path("reconstructed_sources")
    output_dir.mkdir(exist_ok=True)

    reconstructed_articles = {}

    for article_file in article_files:
        print(f"Processing: {article_file}")

        try:
            with open(article_file, "r", encoding="utf-8") as f:
                content = f.read()
        except UnicodeDecodeError:
            with open(article_file, "r", encoding="latin1") as f:
                content = f.read()

        # Extract title and author
        title_match = re.search(r"\\subsubsection\{([^}]+)\\\\([^}]+)\}", content)
        if title_match:
            author = title_match.group(1).strip()
            title = title_match.group(2).strip()
        else:
            title = article_file.parent.name.title()
            author = "Unknown Author"

        # Extract English abstract
        english_abstract = ""
        if r"\begin{english}" in content:
            english_match = re.search(
                r"\\begin\{english\}(.*?)\\end\{english\}", content, re.DOTALL
            )
            if english_match:
                english_abstract = english_match.group(1).strip()
                # Clean up
                english_abstract = re.sub(r"\\[a-zA-Z]+\{[^}]*\}", "", english_abstract)
                english_abstract = re.sub(r"\\[a-zA-Z]+", "", english_abstract)
                english_abstract = re.sub(r"\{[^}]*\}", "", english_abstract)

        # Extract Russian content
        russian_content = ""
        if r"\else" in content:
            russian_match = re.search(r"\\else(.*?)\\fi", content, re.DOTALL)
            if russian_match:
                russian_content = russian_match.group(1).strip()

        # Build markdown
        md_parts = []
        md_parts.append(f"# {title}")
        md_parts.append(f"**{author}**")
        md_parts.append("")

        if english_abstract:
            md_parts.append("## English Abstract")
            md_parts.append("")
            md_parts.append(english_abstract)
            md_parts.append("")

        if russian_content:
            md_parts.append("## Russian Content")
            md_parts.append("")
            # Clean Russian content
            clean_russian = re.sub(r"\\[a-zA-Z]+\{[^}]*\}", "", russian_content)
            clean_russian = re.sub(r"\\[a-zA-Z]+", "", clean_russian)
            clean_russian = re.sub(r"\{[^}]*\}", "", clean_russian)
            clean_russian = re.sub(r"\\\\", "\n\n", clean_russian)
            md_parts.append(clean_russian.strip())
            md_parts.append("")

        markdown = "\n".join(md_parts)
        filename = f"{article_file.parent.name}_reconstructed.md"

        with open(output_dir / filename, "w", encoding="utf-8") as f:
            f.write(markdown)

        reconstructed_articles[filename] = {
            "title": title,
            "author": author,
            "source": str(article_file),
        }

    # Save summary
    summary = {
        "total_articles": len(article_files),
        "articles_processed": [str(f) for f in article_files],
        "output_files": list(reconstructed_articles.keys()),
        "reconstruction_details": reconstructed_articles,
    }

    with open(output_dir / "reconstruction_summary.json", "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)

    print(f"\nReconstruction complete!")
    print(f"- Articles processed: {len(article_files)}")
    print(f"- Output directory: {output_dir}")


if __name__ == "__main__":
    quick_reconstruct()
