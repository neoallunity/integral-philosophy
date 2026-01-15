#!/usr/bin/env python3
"""
Integral Philosophy Publishing System
Main entry point for elegant academic content processing pipeline
"""

import sys
import argparse
from pathlib import Path

# Add core modules to path
sys.path.insert(0, str(Path(__file__).parent / "core"))


def main():
    """Main CLI interface"""
    parser = argparse.ArgumentParser(
        prog="integral-publisher",
        description="🌟 Integral Philosophy Publishing System - Elegant Academic Content Processing",
        epilog="Transform ideas into published works with style and precision",
    )

    parser.add_argument("--version", action="version", version="%(prog)s 2.0.0")

    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Scrape command
    scrape_parser = subparsers.add_parser("scrape", help="Scrape web content")
    scrape_parser.add_argument("url", help="URL to scrape")
    scrape_parser.add_argument("--depth", type=int, default=2, help="Scraping depth")
    scrape_parser.add_argument("--output", help="Output directory")

    # Convert command
    convert_parser = subparsers.add_parser("convert", help="Convert between formats")
    convert_parser.add_argument("input", help="Input file")
    convert_parser.add_argument("--to", required=True, help="Target format")
    convert_parser.add_argument("--output", help="Output file")

    # Process command
    process_parser = subparsers.add_parser(
        "process", help="Complete processing pipeline"
    )
    process_parser.add_argument("url", help="URL to process")
    process_parser.add_argument(
        "--formats", nargs="+", default=["html", "pdf"], help="Output formats"
    )
    process_parser.add_argument("--output", required=True, help="Output directory")
    process_parser.add_argument(
        "--uml", action="store_true", help="Generate UML diagrams"
    )

    # Web command
    web_parser = subparsers.add_parser("web", help="Start web interface")
    web_parser.add_argument(
        "--port", type=int, default=8000, help="Port for web interface"
    )
    web_parser.add_argument(
        "--host", default="localhost", help="Host for web interface"
    )

    # API command
    api_parser = subparsers.add_parser("api", help="Start API server")
    api_parser.add_argument(
        "--port", type=int, default=8001, help="Port for API server"
    )
    api_parser.add_argument("--host", default="localhost", help="Host for API server")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return

    try:
        if args.command == "scrape":
            from core.scrapers import WebScraper

            scraper = WebScraper()
            print(f"🕷️  Scraping {args.url} (depth: {args.depth})")
            print("✅ Web scraping functionality available")
            print("   Note: This is a test demonstration")

        elif args.command == "convert":
            from core.converters import FormatConverter

            converter = FormatConverter()
            print(f"🔄  Converting {args.input} to {args.to}")
            print("✅ Format conversion functionality available")
            print("   Note: This is a test demonstration")

        elif args.command == "process":
            from core.content_pipeline import ContentPipeline

            pipeline = ContentPipeline()
            print(f"🚀  Processing {args.url}")
            print(f"   Output: {args.output}")
            print(f"   Formats: {args.formats}")
            print(f"   UML: {args.uml}")
            print("✅ Complete pipeline functionality available")
            print("   Note: This is a test demonstration")

        elif args.command == "web":
            from web.ui import app

            if app:
                print(f"🌐  Starting web interface on {args.host}:{args.port}")
                print("✅ Web interface available")
                print("   Note: This is a test demonstration")
                print(f"   Would start: http://{args.host}:{args.port}")
            else:
                print("❌ Web interface module not available")

        elif args.command == "api":
            from web.api import app

            if app:
                print(f"🔌  Starting API server on {args.host}:{args.port}")
                print("✅ API server available")
                print("   Note: This is a test demonstration")
                print(f"   Would start: http://{args.host}:{args.port}")
            else:
                print("❌ API server module not available")

    except ImportError as e:
        print(f"❌ Module not found: {e}")
        print("Please ensure all dependencies are installed:")
        print("  pip install -r docs/user/requirements.txt")
        return 1
    except Exception as e:
        print(f"❌ Error: {e}")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
