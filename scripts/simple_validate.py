#!/usr/bin/env python3
"""
Simple validation script for Integral Philosophy publication.
Validates LaTeX files and generates basic quality report.
"""

import sys
import os
from pathlib import Path

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from validators import LaTeXValidator, QualityReportGenerator


def main():
    """Run simple LaTeX validation."""
    print("🚀 Integral Philosophy LaTeX Validation\n")

    validator = LaTeXValidator()
    results = []

    # Validate main LaTeX files
    latex_files = [
        "main.tex",
        "preamble.tex",
        "cfg/cfg-fonts.tex",
        "cfg/cfg-bibliography.tex",
        "cfg/cfg-structure.tex",
    ]

    print("📝 Validating LaTeX files...")
    for latex_file in latex_files:
        if Path(latex_file).exists():
            result = validator.validate(Path(latex_file))
            results.append(result)
            print(
                f"  ✓ {latex_file}: {result.error_count} errors, {result.warning_count} warnings"
            )

    # Generate simple quality report
    if results:
        print("\n📊 Quality Summary:")

        total_errors = sum(r.error_count for r in results)
        total_warnings = sum(r.warning_count for r in results)

        print(f"  • Total Errors: {total_errors}")
        print(f"  • Total Warnings: {total_warnings}")
        print(f"  • Files Checked: {len(results)}")

        # Calculate basic score
        max_possible_issues = len(results) * 10  # Assume 10 is max issues per file
        issues_found = total_errors + (total_warnings * 0.3)  # Weight warnings less
        score = max(0, 100 - (issues_found / max_possible_issues * 100))

        print(f"  • Quality Score: {score:.1f}/100")

        # Save simple report
        report_data = {
            "total_errors": total_errors,
            "total_warnings": total_warnings,
            "files_checked": len(results),
            "quality_score": score,
            "recommendations": [],
        }

        if total_errors > 0:
            report_data["recommendations"].append("Fix critical LaTeX syntax errors")
        if total_warnings > 0:
            report_data["recommendations"].append("Review and address LaTeX warnings")
        if score < 80:
            report_data["recommendations"].append(
                "Consider improving LaTeX code quality"
            )

        # Save to out directory
        out_dir = Path("out/validation_reports")
        out_dir.mkdir(parents=True, exist_ok=True)

        import json

        with open(out_dir / "latex_quality_report.json", "w") as f:
            json.dump(report_data, f, indent=2)

        print(f"\n📄 Report saved to: {out_dir}/latex_quality_report.json")

        if score >= 90:
            print("🎉 Excellent LaTeX quality!")
        elif score >= 70:
            print("👍 Good LaTeX quality with room for improvement")
        else:
            print("⚠️  LaTeX quality needs attention")

        return 0

    print("❌ No LaTeX files found to validate")
    return 1


if __name__ == "__main__":
    exit(main())
