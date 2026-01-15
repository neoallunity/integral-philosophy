#!/usr/bin/env python3
"""
Simple Performance Test for Integral Philosophy Publishing System
"""

import time
import subprocess
import json
import os
from pathlib import Path


def run_command(cmd, timeout=60):
    """Run command and return timing info"""
    start_time = time.time()
    try:
        result = subprocess.run(
            cmd, shell=True, capture_output=True, text=True, timeout=timeout
        )
        end_time = time.time()
        return {
            "success": result.returncode == 0,
            "time_seconds": end_time - start_time,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "return_code": result.returncode,
        }
    except subprocess.TimeoutExpired:
        return {"success": False, "time_seconds": timeout, "error": "Command timed out"}


def test_format_conversion_performance():
    """Test format conversion performance"""
    print("=== Format Conversion Performance Test ===")

    # Test file
    test_file = "final_test.md"
    formats = ["html", "latex", "org", "asciidoc", "rst", "tei"]
    results = {}

    for fmt in formats:
        print(f"Testing conversion to {fmt}...")

        cmd = f"source venv/bin/activate && python3 scripts/format_converter.py {test_file} -f {fmt} --work-dir ."
        result = run_command(cmd)

        results[fmt] = {
            "success": result["success"],
            "time_seconds": result["time_seconds"],
        }

        if result["success"]:
            print(f"  ✅ {fmt}: {result['time_seconds']:.3f}s")
        else:
            print(f"  ❌ {fmt}: Failed")

    return results


def test_tei_generation_performance():
    """Test TEI generation performance"""
    print("=== TEI Generation Performance Test ===")

    # First create AST
    cmd = "source venv/bin/activate && python3 scripts/format_converter.py final_test.md --ast --work-dir ."
    result = run_command(cmd)

    if not result["success"]:
        return {"success": False, "error": "AST generation failed"}

    ast_time = result["time_seconds"]

    # Generate TEI
    cmd = "source venv/bin/activate && ./tei.sh final_test.ast.json -o perf_test.tei"
    result = run_command(cmd)

    return {
        "success": result["success"],
        "ast_generation_time": ast_time,
        "tei_generation_time": result["time_seconds"],
        "total_time": ast_time + result["time_seconds"],
    }


def test_uml_generation_performance():
    """Test UML generation performance"""
    print("=== UML Generation Performance Test ===")

    formats = ["plantuml", "mermaid", "graphviz"]
    results = {}

    for fmt in formats:
        print(f"Testing UML generation ({fmt})...")

        cmd = f"source venv/bin/activate && ./uml.sh final_test.ast.json -f {fmt} -o ./perf_uml"
        result = run_command(cmd)

        results[fmt] = {
            "success": result["success"],
            "time_seconds": result["time_seconds"],
        }

        if result["success"]:
            print(f"  ✅ {fmt}: {result['time_seconds']:.3f}s")
        else:
            print(f"  ❌ {fmt}: Failed")

    return results


def test_conversion_matrix_performance():
    """Test conversion matrix creation performance"""
    print("=== Conversion Matrix Performance Test ===")

    cmd = "source venv/bin/activate && python3 scripts/format_converter.py final_test.md --matrix --work-dir ."
    result = run_command(cmd)

    return {"success": result["success"], "time_seconds": result["time_seconds"]}


def test_chain_conversion_performance():
    """Test chain conversion performance"""
    print("=== Chain Conversion Performance Test ===")

    chains = [
        ["org", "html"],
        ["org", "asciidoc", "html"],
        ["org", "asciidoc", "rst", "html"],
    ]

    results = {}

    for i, chain in enumerate(chains):
        chain_str = " ".join(chain)
        print(f"Testing chain: {chain_str}")

        cmd = f"source venv/bin/activate && python3 scripts/format_converter.py final_test.md --chain {chain_str} --work-dir ."
        result = run_command(cmd)

        results[f"chain_{i + 1}"] = {
            "chain": chain,
            "success": result["success"],
            "time_seconds": result["time_seconds"],
        }

        if result["success"]:
            print(f"  ✅ Chain {i + 1}: {result['time_seconds']:.3f}s")
        else:
            print(f"  ❌ Chain {i + 1}: Failed")

    return results


def generate_performance_report(results):
    """Generate performance report"""
    report = {"test_timestamp": time.strftime("%Y-%m-%d %H:%M:%S"), "results": results}

    # Calculate statistics
    success_rates = {}

    # Format conversion stats
    if "format_conversion" in results:
        conv_results = results["format_conversion"]
        successful = sum(1 for r in conv_results.values() if r["success"])
        total = len(conv_results)
        success_rates["format_conversion"] = successful / total if total > 0 else 0

        # Fastest/slowest
        times = [
            (fmt, r["time_seconds"]) for fmt, r in conv_results.items() if r["success"]
        ]
        if times:
            fastest = min(times, key=lambda x: x[1])
            slowest = max(times, key=lambda x: x[1])
            report["fastest_conversion"] = {"format": fastest[0], "time": fastest[1]}
            report["slowest_conversion"] = {"format": slowest[0], "time": slowest[1]}

    # UML generation stats
    if "uml_generation" in results:
        uml_results = results["uml_generation"]
        successful = sum(1 for r in uml_results.values() if r["success"])
        total = len(uml_results)
        success_rates["uml_generation"] = successful / total if total > 0 else 0

    report["success_rates"] = success_rates

    return report


def main():
    """Run performance tests"""
    print("Integral Philosophy Publishing System - Performance Test")
    print("=" * 60)

    # Check if test files exist
    if not Path("final_test.md").exists():
        print("Error: final_test.md not found")
        return

    results = {}

    try:
        # Format conversion test
        results["format_conversion"] = test_format_conversion_performance()

        # TEI generation test
        results["tei_generation"] = test_tei_generation_performance()

        # UML generation test
        results["uml_generation"] = test_uml_generation_performance()

        # Conversion matrix test
        results["conversion_matrix"] = test_conversion_matrix_performance()

        # Chain conversion test
        results["chain_conversion"] = test_chain_conversion_performance()

    except Exception as e:
        print(f"Performance test failed: {e}")
        results["error"] = str(e)

    # Generate report
    report = generate_performance_report(results)

    # Save report
    with open("performance_report.json", "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False)

    # Print summary
    print("\nPerformance Test Summary")
    print("-" * 30)

    if "success_rates" in report:
        for test_name, rate in report["success_rates"].items():
            print(f"{test_name}: {rate:.1%} success rate")

    if "fastest_conversion" in report:
        print(
            f"Fastest conversion: {report['fastest_conversion']['format']} ({report['fastest_conversion']['time']:.3f}s)"
        )
        print(
            f"Slowest conversion: {report['slowest_conversion']['format']} ({report['slowest_conversion']['time']:.3f}s)"
        )

    if "tei_generation" in results and results["tei_generation"]["success"]:
        tei_result = results["tei_generation"]
        print(f"TEI generation: {tei_result['total_time']:.3f}s total")

    print(f"\nReport saved to: performance_report.json")

    return report


if __name__ == "__main__":
    main()
