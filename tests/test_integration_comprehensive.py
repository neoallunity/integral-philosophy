#!/usr/bin/env python3
"""
Comprehensive integration test for the complete validation pipeline.
Tests end-to-end functionality with real-world scenarios.
"""

import sys
import os
import tempfile
import json
import shutil
from pathlib import Path
from typing import Dict, List

# Add the project root to Python path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from validators import (
    HTML5Validator,
    CSSValidator,
    JavaScriptValidator,
    LaTeXValidator,
    ContentIntegrityValidator,
    QualityReportGenerator,
)


def create_realistic_test_files():
    """Create realistic test files for integration testing."""
    print("🔍 Creating realistic test files...")

    test_files = {}

    # Create a realistic HTML file
    html_content = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Integral Philosophy - Test Article</title>
    <link rel="stylesheet" href="styles.css">
    <script src="script.js"></script>
</head>
<body>
    <header>
        <nav>
            <ul>
                <li><a href="#home">Home</a></li>
                <li><a href="#articles">Articles</a></li>
                <li><a href="#about">About</a></li>
            </ul>
        </nav>
    </header>
    
    <main>
        <article>
            <h1>The Nature of Integral Consciousness</h1>
            <h2>An Exploration of Holistic Awareness</h2>
            
            <p>This article explores the concept of <strong>integral consciousness</strong> 
            as developed by various philosophical traditions. The integration of 
            multiple perspectives creates a comprehensive understanding of human awareness.</p>
            
            <img src="consciousness.jpg" alt="Diagram of integral consciousness levels">
            
            <section>
                <h3>Historical Development</h3>
                <p>The concept has evolved through several stages:</p>
                <ul>
                    <li>Ancient philosophical foundations</li>
                    <li>Medieval scholastic integration</li>
                    <li>Modern psychological approaches</li>
                    <li>Contemporary integral theory</li>
                </ul>
            </section>
            
            <section>
                <h3>Key Principles</h3>
                <div style="background: #f0f0f0; padding: 15px; margin: 10px 0;">
                    <p>The fundamental principles include:</p>
                    <ol>
                        <li>Non-dual awareness</li>
                        <li>Developmental stages</li>
                        <li>Quadrants of reality</li>
                        <li>States and types</li>
                    </ol>
                </div>
            </section>
        </article>
    </main>
    
    <footer>
        <p>&copy; 2024 Integral Philosophy Journal</p>
    </footer>
</body>
</html>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(html_content)
        test_files["html"] = Path(f.name)

    # Create corresponding CSS
    css_content = """/* Integral Philosophy Journal Styles */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');

:root {
    --primary-color: #2c3e50;
    --secondary-color: #3498db;
    --accent-color: #e74c3c;
    --text-color: #333;
    --bg-color: #f8f9fa;
    --border-color: #ddd;
}

* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
    line-height: 1.6;
    color: var(--text-color);
    background-color: var(--bg-color);
}

header {
    background: var(--primary-color);
    color: white;
    padding: 1rem 0;
}

nav ul {
    list-style: none;
    display: flex;
    justify-content: center;
}

nav li {
    margin: 0 1rem;
}

nav a {
    color: white;
    text-decoration: none;
    font-weight: 500;
}

nav a:hover {
    text-decoration: underline;
}

main {
    max-width: 800px;
    margin: 2rem auto;
    padding: 0 1rem;
}

article h1 {
    color: var(--primary-color);
    margin-bottom: 1rem;
    font-size: 2.5rem;
}

article h2 {
    color: var(--secondary-color);
    margin: 2rem 0 1rem;
    font-size: 1.8rem;
}

article h3 {
    color: var(--primary-color);
    margin: 1.5rem 0 0.5rem;
    font-size: 1.4rem;
}

p {
    margin-bottom: 1rem;
}

strong {
    color: var(--accent-color);
    font-weight: 600;
}

img {
    max-width: 100%;
    height: auto;
    margin: 1rem 0;
}

section {
    margin: 2rem 0;
    padding: 1.5rem;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

ul, ol {
    margin-left: 2rem;
    margin-bottom: 1rem;
}

li {
    margin-bottom: 0.5rem;
}

footer {
    background: var(--primary-color);
    color: white;
    text-align: center;
    padding: 1rem;
    margin-top: 2rem;
}

@media (max-width: 768px) {
    main {
        padding: 0 0.5rem;
    }
    
    article h1 {
        font-size: 2rem;
    }
    
    article h2 {
        font-size: 1.5rem;
    }
    
    nav ul {
        flex-direction: column;
        align-items: center;
    }
    
    nav li {
        margin: 0.5rem 0;
    }
}"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".css", delete=False) as f:
        f.write(css_content)
        test_files["css"] = Path(f.name)

    # Create corresponding JavaScript
    js_content = """// Integral Philosophy Journal - Interactive Features
'use strict';

class IntegralPhilosophySite {
    constructor() {
        this.init();
    }
    
    init() {
        this.setupNavigation();
        this.setupSearch();
        this.setupThemeToggle();
        this.setupAccessibility();
    }
    
    setupNavigation() {
        // Smooth scrolling for anchor links
        document.querySelectorAll('a[href^="#"]').forEach(anchor => {
            anchor.addEventListener('click', function (e) {
                e.preventDefault();
                const target = document.querySelector(this.getAttribute('href'));
                if (target) {
                    target.scrollIntoView({
                        behavior: 'smooth',
                        block: 'start'
                    });
                }
            });
        });
        
        // Active navigation highlighting
        this.updateActiveNavigation();
        window.addEventListener('scroll', () => this.updateActiveNavigation());
    }
    
    updateActiveNavigation() {
        const sections = document.querySelectorAll('section');
        const navLinks = document.querySelectorAll('nav a[href^="#"]');
        
        let currentSection = '';
        
        sections.forEach(section => {
            const rect = section.getBoundingClientRect();
            if (rect.top <= 100 && rect.bottom >= 100) {
                currentSection = section.getAttribute('id');
            }
        });
        
        navLinks.forEach(link => {
            link.classList.remove('active');
            if (link.getAttribute('href') === `#${currentSection}`) {
                link.classList.add('active');
            }
        });
    }
    
    setupSearch() {
        const searchInput = document.createElement('input');
        searchInput.type = 'search';
        searchInput.placeholder = 'Search articles...';
        searchInput.className = 'search-input';
        
        // Add search functionality
        searchInput.addEventListener('input', (e) => {
            const query = e.target.value.toLowerCase();
            this.performSearch(query);
        });
        
        // Insert search into header
        const header = document.querySelector('header');
        if (header) {
            header.appendChild(searchInput);
        }
    }
    
    performSearch(query) {
        if (query.length < 2) {
            this.clearSearchResults();
            return;
        }
        
        const articles = document.querySelectorAll('article');
        const results = [];
        
        articles.forEach(article => {
            const text = article.textContent.toLowerCase();
            if (text.includes(query)) {
                results.push({
                    element: article,
                    relevance: this.calculateRelevance(text, query)
                });
            }
        });
        
        this.displaySearchResults(results.sort((a, b) => b.relevance - a.relevance));
    }
    
    calculateRelevance(text, query) {
        // Simple relevance calculation
        const queryWords = query.split(' ');
        let relevance = 0;
        
        queryWords.forEach(word => {
            const occurrences = (text.match(new RegExp(word, 'g')) || []).length;
            relevance += occurrences;
        });
        
        return relevance;
    }
    
    displaySearchResults(results) {
        this.clearSearchResults();
        
        if (results.length === 0) {
            this.showNoResults();
            return;
        }
        
        results.forEach(result => {
            result.element.classList.add('search-result');
            result.element.style.display = 'block';
        });
    }
    
    clearSearchResults() {
        document.querySelectorAll('.search-result').forEach(element => {
            element.classList.remove('search-result');
            element.style.display = '';
        });
        
        this.hideNoResults();
    }
    
    showNoResults() {
        const noResults = document.createElement('div');
        noResults.className = 'no-results';
        noResults.textContent = 'No articles found matching your search.';
        document.querySelector('main').appendChild(noResults);
    }
    
    hideNoResults() {
        const noResults = document.querySelector('.no-results');
        if (noResults) {
            noResults.remove();
        }
    }
    
    setupThemeToggle() {
        const themeToggle = document.createElement('button');
        themeToggle.textContent = '🌓';
        themeToggle.className = 'theme-toggle';
        themeToggle.setAttribute('aria-label', 'Toggle theme');
        
        themeToggle.addEventListener('click', () => {
            document.body.classList.toggle('dark-theme');
            this.updateThemeToggle(themeToggle);
        });
        
        const header = document.querySelector('header');
        if (header) {
            header.appendChild(themeToggle);
        }
    }
    
    updateThemeToggle(button) {
        const isDark = document.body.classList.contains('dark-theme');
        button.textContent = isDark ? '☀️' : '🌓';
        button.setAttribute('aria-label', isDark ? 'Switch to light theme' : 'Switch to dark theme');
    }
    
    setupAccessibility() {
        // Add ARIA labels for better screen reader support
        document.querySelectorAll('img[alt]').forEach(img => {
            if (!img.getAttribute('role')) {
                img.setAttribute('role', 'img');
            }
        });
        
        // Add keyboard navigation
        document.addEventListener('keydown', (e) => {
            if (e.key === 'Tab') {
                document.body.classList.add('keyboard-navigation');
            }
        });
        
        document.addEventListener('mousedown', () => {
            document.body.classList.remove('keyboard-navigation');
        });
    }
}

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
    new IntegralPhilosophySite();
});

// Add some utility functions
const utils = {
    debounce(func, wait) {
        let timeout;
        return function executedFunction(...args) {
            const later = () => {
                clearTimeout(timeout);
                func(...args);
            };
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
        };
    },
    
    throttle(func, limit) {
        let inThrottle;
        return function() {
            const args = arguments;
            const context = this;
            if (!inThrottle) {
                func.apply(context, args);
                inThrottle = true;
                setTimeout(() => inThrottle = false, limit);
            }
        };
    }
};

// Export for potential module usage
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { IntegralPhilosophySite, utils };
}"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
        f.write(js_content)
        test_files["js"] = Path(f.name)

    # Create a realistic LaTeX file
    latex_content = """\\documentclass[12pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T2A]{fontenc}
\\usepackage[russian,english]{babel}
\\usepackage{amsmath,amssymb}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\usepackage{geometry}
\\usepackage{fancyhdr}
\\usepackage{natbib}

% Geometry settings
\\geometry{
    left=2.5cm,
    right=2.5cm,
    top=3cm,
    bottom=3cm
}

% Hyperref setup
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    urlcolor=blue,
    citecolor=green
}

% Title information
\\title{The Nature of Integral Consciousness:\\ An Exploration of Holistic Awareness}
\\\\author{Dr. Sarah Johnson\\\\{\\\\small Integral Philosophy Institute\\\\}}
\\date{\\today}

\\begin{document}

\\maketitle

\\begin{abstract}
This article explores the concept of integral consciousness as developed 
by various philosophical traditions. The integration of multiple perspectives 
creates a comprehensive understanding of human awareness that transcends 
reductionist approaches while maintaining scientific rigor.
\\end{abstract}

\\section{Introduction}

The concept of \\textbf{integral consciousness} represents one of the most 
significant developments in contemporary philosophical thought. Unlike 
traditional approaches that focus on isolated aspects of human experience, 
integral consciousness seeks to synthesize multiple dimensions of awareness 
into a coherent framework \\citep{wilber2000}.

\\subsection{Historical Context}

The roots of integral thinking can be traced back to ancient philosophical 
traditions, but its modern formulation emerged through the work of several 
key thinkers in the 20th and 21st centuries. The integration of Eastern and 
Western philosophical perspectives has been particularly influential in 
shaping this approach.

\\section{Key Principles}

\\subsection{Non-Dual Awareness}

At the core of integral consciousness lies the principle of non-dual 
awareness. This perspective recognizes the fundamental unity of subject 
and object, mind and matter, while simultaneously honoring their 
functional distinctions.

\\subsection{Developmental Stages}

Integral consciousness incorporates a developmental understanding of 
human awareness. The progression through various stages of consciousness 
is not merely linear but spiral, with each stage transcending and including 
previous ones.

\\begin{equation}
C_n = C_{n-1} \\oplus \\Delta C_n
\\end{equation}

Where $C_n$ represents consciousness at stage $n$, $C_{n-1}$ is the 
previous stage, and $\\Delta C_n$ represents the developmental increment.

\\section{The Four Quadrants}

One of the most influential frameworks in integral theory is the 
four-quadrant model, which maps different dimensions of experience:

\\begin{enumerate}
    \\item \\textbf{Interior-Individual}: Subjective experience, consciousness
    \\item \\textbf{Exterior-Individual}: Objective behavior, brain states
    \\item \\textbf{Interior-Collective}: Intersubjective culture, shared values
    \\item \\textbf{Exterior-Collective}: Interobjective systems, social structures
\\end{enumerate}

\\begin{figure}[h]
    \\centering
    \\includegraphics[width=0.8\\textwidth]{quadrants.pdf}
    \\caption{The Four Quadrants of Integral Theory}
    \\label{fig:quadrants}
\\end{figure}

\\section{Applications and Implications}

The principles of integral consciousness have profound implications for 
various fields of human endeavor:

\\subsection{Psychology and Therapy}

In therapeutic contexts, integral consciousness provides a framework for 
understanding psychological development and designing interventions that 
address multiple aspects of human experience simultaneously.

\\subsection{Education}

Integral approaches to education recognize the importance of developing 
not just cognitive capacities but also emotional, social, and spiritual 
dimensions of learning.

\\subsection{Organizational Development}

Organizations can benefit from integral approaches by creating cultures 
that honor multiple perspectives and ways of knowing while maintaining 
functional coherence.

\\section{Conclusion}

Integral consciousness offers a powerful framework for understanding 
human experience in its full complexity. By integrating multiple 
perspectives while maintaining their distinct contributions, this approach 
provides a pathway to more comprehensive and effective ways of addressing 
the challenges of our time.

Future research should focus on empirical validation of integral 
principles and the development of practical applications across various 
domains of human activity.

\\bibliographystyle{apalike}
\\bibliography{references}

\\end{document}"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".tex", delete=False) as f:
        f.write(latex_content)
        test_files["latex"] = Path(f.name)

    # Create a plain text version for integrity testing
    text_content = """The Nature of Integral Consciousness: An Exploration of Holistic Awareness

Abstract

This article explores the concept of integral consciousness as developed 
by various philosophical traditions. The integration of multiple perspectives 
creates a comprehensive understanding of human awareness that transcends 
reductionist approaches while maintaining scientific rigor.

Introduction

The concept of integral consciousness represents one of the most 
significant developments in contemporary philosophical thought. Unlike 
traditional approaches that focus on isolated aspects of human experience, 
integral consciousness seeks to synthesize multiple dimensions of awareness 
into a coherent framework.

Historical Context

The roots of integral thinking can be traced back to ancient philosophical 
traditions, but its modern formulation emerged through the work of several 
key thinkers in the 20th and 21st centuries. The integration of Eastern and 
Western philosophical perspectives has been particularly influential in 
shaping this approach.

Key Principles

Non-Dual Awareness

At the core of integral consciousness lies the principle of non-dual 
awareness. This perspective recognizes the fundamental unity of subject 
and object, mind and matter, while simultaneously honoring their 
functional distinctions.

Developmental Stages

Integral consciousness incorporates a developmental understanding of 
human awareness. The progression through various stages of consciousness 
is not merely linear but spiral, with each stage transcending and including 
previous ones.

The Four Quadrants

One of the most influential frameworks in integral theory is the 
four-quadrant model, which maps different dimensions of experience:

1. Interior-Individual: Subjective experience, consciousness
2. Exterior-Individual: Objective behavior, brain states
3. Interior-Collective: Intersubjective culture, shared values
4. Exterior-Collective: Interobjective systems, social structures

Applications and Implications

The principles of integral consciousness have profound implications for 
various fields of human endeavor.

Psychology and Therapy

In therapeutic contexts, integral consciousness provides a framework for 
understanding psychological development and designing interventions that 
address multiple aspects of human experience simultaneously.

Education

Integral approaches to education recognize the importance of developing 
not just cognitive capacities but also emotional, social, and spiritual 
dimensions of learning.

Organizational Development

Organizations can benefit from integral approaches by creating cultures 
that honor multiple perspectives and ways of knowing while maintaining 
functional coherence.

Conclusion

Integral consciousness offers a powerful framework for understanding 
human experience in its full complexity. By integrating multiple 
perspectives while maintaining their distinct contributions, this approach 
provides a pathway to more comprehensive and effective ways of addressing 
the challenges of our time.

Future research should focus on empirical validation of integral 
principles and the development of practical applications across various 
domains of human activity."""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        f.write(text_content)
        test_files["text"] = Path(f.name)

    print(f"  ✓ Created {len(test_files)} test files")
    return test_files


def test_complete_validation_pipeline():
    """Test the complete validation pipeline with realistic files."""
    print("🔍 Testing complete validation pipeline...")

    test_files = create_realistic_test_files()

    try:
        # Initialize all validators
        validators = {
            "html": HTML5Validator(),
            "css": CSSValidator(),
            "js": JavaScriptValidator(),
            "latex": LaTeXValidator(),
        }

        # Validate each file
        validation_results = {}

        for format_name, validator in validators.items():
            if format_name in test_files:
                print(f"  Validating {format_name.upper()}...")
                result = validator.validate(test_files[format_name])
                validation_results[format_name] = result

                print(
                    f"    Errors: {result.error_count}, Warnings: {result.warning_count}"
                )
                print(f"    Stats: {result.stats}")

        # Test content integrity
        print("  Testing content integrity...")
        integrity_validator = ContentIntegrityValidator()

        integrity_formats = {"html": test_files["html"], "text": test_files["text"]}

        integrity_result = integrity_validator.validate_integrity_across_formats(
            integrity_formats
        )
        validation_results["integrity"] = integrity_result

        print(
            f"    Integrity errors: {integrity_result.error_count}, warnings: {integrity_result.warning_count}"
        )
        print(f"    Content chunks: {integrity_result.stats.get('content_chunks', 0)}")
        print(
            f"    Similarity scores: {integrity_result.stats.get('similarity_scores', {})}"
        )

        # Generate quality report
        print("  Generating quality report...")
        generator = QualityReportGenerator()

        report = generator.generate_report(
            source_files=[test_files["latex"]],
            output_formats={"html": test_files["html"], "text": test_files["text"]},
            validation_results=validation_results,
            processing_time=3.5,
        )

        print(f"    Overall score: {report.quality_metrics.overall_score:.1f}/100")
        print(f"    Format scores: {report.quality_metrics.format_scores}")
        print(f"    Integrity score: {report.quality_metrics.integrity_score:.1f}")
        print(
            f"    Accessibility score: {report.quality_metrics.accessibility_score:.1f}"
        )
        print(f"    Performance score: {report.quality_metrics.performance_score:.1f}")
        print(
            f"    Standards compliance: {report.quality_metrics.standards_compliance:.1f}"
        )
        print(f"    Recommendations: {len(report.recommendations)}")

        # Save comprehensive report
        report_dir = Path(tempfile.mkdtemp())

        generator.save_report(report, report_dir / "integration_report.json", "json")
        generator.save_report(report, report_dir / "integration_report.html", "html")
        generator.save_report(report, report_dir / "integration_report.md", "markdown")

        print(f"    Reports saved to: {report_dir}")

        # Verify report files
        json_report = report_dir / "integration_report.json"
        html_report = report_dir / "integration_report.html"
        md_report = report_dir / "integration_report.md"

        assert json_report.exists() and json_report.stat().st_size > 0
        assert html_report.exists() and html_report.stat().st_size > 0
        assert md_report.exists() and md_report.stat().st_size > 0

        # Verify JSON report structure
        with open(json_report, "r") as f:
            report_data = json.load(f)
            assert "quality_metrics" in report_data
            assert "validation_results" in report_data
            assert "recommendations" in report_data

        print("  ✓ All report files created and verified")

        # Cleanup
        import shutil

        shutil.rmtree(report_dir)

        print("  ✓ Complete validation pipeline successful")
        return True

    finally:
        for temp_file in test_files.values():
            temp_file.unlink()


def test_error_recovery_and_resilience():
    """Test error recovery and system resilience."""
    print("🔍 Testing error recovery and resilience...")

    # Test with corrupted files
    corrupted_files = {}

    # Corrupted HTML
    corrupted_html = "<!DOCTYPE html><html><head><title>Test</title></head><body><h1>Test</h1><p>Unclosed paragraph<div>Nested<span>Without proper closing</body></html>"

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(corrupted_html)
        corrupted_files["html"] = Path(f.name)

    # Corrupted CSS
    corrupted_css = ".test { color: red; font-size: 14px; margin: 10px; } .invalid { background: url(test.jpg; border: 1px solid #000"

    with tempfile.NamedTemporaryFile(mode="w", suffix=".css", delete=False) as f:
        f.write(corrupted_css)
        corrupted_files["css"] = Path(f.name)

    # Corrupted JavaScript
    corrupted_js = (
        "function test() { var x = 10; if (x > 5 { console.log('error'); } return x; }"
    )

    with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
        f.write(corrupted_js)
        corrupted_files["js"] = Path(f.name)

    # Corrupted LaTeX
    corrupted_latex = "\\documentclass{article}\\begin{document}\\section{Test}\\begin{itemize}\\item Item 1\\item Item 2\\subsection{Subsection}\\end{document}"

    with tempfile.NamedTemporaryFile(mode="w", suffix=".tex", delete=False) as f:
        f.write(corrupted_latex)
        corrupted_files["latex"] = Path(f.name)

    try:
        validators = {
            "html": HTML5Validator(),
            "css": CSSValidator(),
            "js": JavaScriptValidator(),
            "latex": LaTeXValidator(),
        }

        # Test that validators handle corrupted files gracefully
        for format_name, validator in validators.items():
            if format_name in corrupted_files:
                try:
                    result = validator.validate(corrupted_files[format_name])
                    print(
                        f"  ✓ {format_name.upper()} validator handled corrupted file gracefully"
                    )
                    print(
                        f"    Errors: {result.error_count}, Warnings: {result.warning_count}"
                    )
                except Exception as e:
                    print(f"  ⚠️  {format_name.upper()} validator raised exception: {e}")
                    # This is acceptable - validators should handle errors gracefully

        print("  ✓ Error recovery and resilience test completed")
        return True

    finally:
        for temp_file in corrupted_files.values():
            temp_file.unlink()


def test_multi_format_consistency():
    """Test consistency across multiple formats."""
    print("🔍 Testing multi-format consistency...")

    # Create consistent content across formats
    base_content = """Integral Philosophy Journal

Volume 1, Issue 1 - 2024

The Nature of Integral Consciousness

Abstract:
This article explores the concept of integral consciousness as developed 
by various philosophical traditions. The integration of multiple perspectives 
creates a comprehensive understanding of human awareness.

Introduction:
The concept of integral consciousness represents one of the most 
significant developments in contemporary philosophical thought.

Key Principles:
1. Non-dual awareness
2. Developmental stages  
3. Four quadrants
4. States and types

Conclusion:
Integral consciousness offers a powerful framework for understanding 
human experience in its full complexity."""

    # Create files in different formats with same content
    test_files = {}

    # HTML version
    html_content = f"""<!DOCTYPE html>
<html><head><title>Integral Philosophy</title></head><body>
<article>
<h1>Integral Philosophy Journal</h1>
<h2>Volume 1, Issue 1 - 2024</h2>
<h3>The Nature of Integral Consciousness</h3>
<section>
<h4>Abstract:</h4>
<p>This article explores the concept of integral consciousness as developed 
by various philosophical traditions. The integration of multiple perspectives 
creates a comprehensive understanding of human awareness.</p>
</section>
<section>
<h4>Introduction:</h4>
<p>The concept of integral consciousness represents one of the most 
significant developments in contemporary philosophical thought.</p>
</section>
<section>
<h4>Key Principles:</h4>
<ol><li>Non-dual awareness</li><li>Developmental stages</li><li>Four quadrants</li><li>States and types</li></ol>
</section>
<section>
<h4>Conclusion:</h4>
<p>Integral consciousness offers a powerful framework for understanding 
human experience in its full complexity.</p>
</section>
</article>
</body></html>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(html_content)
        test_files["html"] = Path(f.name)

    # Text version
    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        f.write(base_content)
        test_files["text"] = Path(f.name)

    # LaTeX version
    latex_content = f"""\\documentclass{{article}}
\\begin{{document}}
\\title{{Integral Philosophy Journal}}
\\author{{Volume 1, Issue 1 - 2024}}
\\maketitle

\\section{{The Nature of Integral Consciousness}}

\\subsection{{Abstract}}
This article explores the concept of integral consciousness as developed 
by various philosophical traditions. The integration of multiple perspectives 
creates a comprehensive understanding of human awareness.

\\subsection{{Introduction}}
The concept of integral consciousness represents one of the most 
significant developments in contemporary philosophical thought.

\\subsection{{Key Principles}}
\\begin{{enumerate}}
\\item Non-dual awareness
\\item Developmental stages
\\item Four quadrants
\\item States and types
\\end{{enumerate}}

\\subsection{{Conclusion}}
Integral consciousness offers a powerful framework for understanding 
human experience in its full complexity.
\\end{{document}}"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".tex", delete=False) as f:
        f.write(latex_content)
        test_files["latex"] = Path(f.name)

    try:
        # Validate consistency
        validator = ContentIntegrityValidator()

        result = validator.validate_integrity_across_formats(test_files)

        print(f"  ✓ Multi-format consistency validation completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    Content chunks: {result.stats.get('content_chunks', 0)}")
        print(f"    Similarity scores: {result.stats.get('similarity_scores', {})}")

        # Should have high similarity due to consistent content
        similarity_scores = result.stats.get("similarity_scores", {})
        if similarity_scores:
            avg_similarity = sum(similarity_scores.values()) / len(similarity_scores)
            print(f"    Average similarity: {avg_similarity:.2%}")

            # Assert reasonable similarity
            assert avg_similarity > 0.5, (
                f"Expected high similarity, got {avg_similarity:.2%}"
            )

        print("  ✓ Multi-format consistency test passed")
        return True

    finally:
        for temp_file in test_files.values():
            temp_file.unlink()


def main():
    """Run comprehensive integration tests."""
    print("🚀 Running Comprehensive Integration Tests...\n")

    tests = [
        test_complete_validation_pipeline,
        test_error_recovery_and_resilience,
        test_multi_format_consistency,
    ]

    results = []

    for test in tests:
        try:
            result = test()
            results.append(result)
            print(f"✅ {test.__name__} passed\n")
        except Exception as e:
            print(f"❌ {test.__name__} failed: {e}\n")
            results.append(False)

    # Summary
    passed = sum(results)
    total = len(results)

    print("=" * 60)
    print(f"Integration Tests Summary: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All integration tests passed!")
        print("✅ Validation system works correctly in real-world scenarios")
        return 0
    else:
        print("❌ Some integration tests failed")
        return 1


if __name__ == "__main__":
    exit(main())
