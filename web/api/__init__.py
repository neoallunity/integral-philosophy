"""
Web API module
"""

try:
    from .api_server import app

    __all__ = ["app"]
except ImportError:
    app = None
    __all__ = []
