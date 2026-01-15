"""
Security tests for authentication mechanisms
Testing API key brute force attacks, session hijacking prevention, authentication bypass, and rate limiting
"""

import pytest
import time
import hashlib
import hmac
import secrets
import threading
from typing import Dict, Any, List, Optional, Tuple
from unittest.mock import Mock, patch, MagicMock

from tests.utils.base_test_classes import SecurityTestCase


class TestAuthenticationSecurity(SecurityTestCase):
    """Test authentication security vulnerabilities"""

    @pytest.fixture(autouse=True)
    def setup_authentication_security(self):
        """Setup authentication security testing environment"""
        # Mock authentication system
        self.auth_system = Mock()
        self.auth_system.failed_attempts = {}
        self.auth_system.blocked_ips = set()
        self.auth_system.active_sessions = {}
        self.auth_system.api_keys = {
            "valid_key_2024": {"user_id": 1, "permissions": ["read", "write"]},
            "admin_key_2024": {"user_id": 2, "permissions": ["read", "write", "admin"]},
            "limited_key_2024": {"user_id": 3, "permissions": ["read"]},
        }

        # Rate limiting
        self.auth_system.rate_limits = {
            "login": {
                "max_attempts": 5,
                "window_seconds": 300,
            },  # 5 attempts per 5 minutes
            "api": {
                "max_requests": 100,
                "window_seconds": 3600,
            },  # 100 requests per hour
        }

        # Session configuration
        self.auth_system.session_timeout = 1800  # 30 minutes
        self.auth_system.csrf_token_length = 32

        yield

        # Cleanup authentication state
        self.auth_system.failed_attempts.clear()
        self.auth_system.blocked_ips.clear()
        self.auth_system.active_sessions.clear()

    def generate_api_key(self, length: int = 32) -> str:
        """Generate a secure API key"""
        return secrets.token_urlsafe(length)

    def hash_password(self, password: str, salt: str = None) -> Tuple[str, str]:
        """Hash password with salt"""
        if salt is None:
            salt = secrets.token_hex(16)

        hash_obj = hashlib.pbkdf2_hmac(
            "sha256", password.encode(), salt.encode(), 100000
        )
        return hash_obj.hex(), salt

    def verify_password(self, password: str, hashed: str, salt: str) -> bool:
        """Verify password against hash"""
        hash_obj = hashlib.pbkdf2_hmac(
            "sha256", password.encode(), salt.encode(), 100000
        )
        return hmac.compare_digest(hash_obj.hex(), hashed)

    def test_api_key_brute_force_protection(self):
        """Test protection against API key brute force attacks"""

        # Mock API key validation with rate limiting
        def validate_api_key_with_rate_limit(
            ip_address: str, provided_key: str
        ) -> Dict[str, Any]:
            """Validate API key with brute force protection"""

            # Check if IP is blocked
            if ip_address in self.auth_system.blocked_ips:
                return {
                    "valid": False,
                    "reason": "IP blocked due to too many failed attempts",
                }

            # Initialize failed attempts counter
            if ip_address not in self.auth_system.failed_attempts:
                self.auth_system.failed_attempts[ip_address] = []

            # Clean old attempts (outside rate limit window)
            current_time = time.time()
            window_start = (
                current_time - self.auth_system.rate_limits["login"]["window_seconds"]
            )
            self.auth_system.failed_attempts[ip_address] = [
                attempt_time
                for attempt_time in self.auth_system.failed_attempts[ip_address]
                if attempt_time > window_start
            ]

            # Check if rate limit exceeded
            max_attempts = self.auth_system.rate_limits["login"]["max_attempts"]
            if len(self.auth_system.failed_attempts[ip_address]) >= max_attempts:
                self.auth_system.blocked_ips.add(ip_address)
                return {
                    "valid": False,
                    "reason": "Too many failed attempts - IP blocked",
                }

            # Validate API key
            if provided_key in self.auth_system.api_keys:
                return {
                    "valid": True,
                    "user_data": self.auth_system.api_keys[provided_key],
                }
            else:
                # Record failed attempt
                self.auth_system.failed_attempts[ip_address].append(current_time)
                return {"valid": False, "reason": "Invalid API key"}

        # Test brute force attack
        attacker_ip = "192.168.1.100"
        invalid_keys = [self.generate_api_key() for _ in range(10)]

        attempts = []
        for i, invalid_key in enumerate(invalid_keys):
            result = validate_api_key_with_rate_limit(attacker_ip, invalid_key)
            attempts.append(result)

            # Should fail first few attempts
            if i < self.auth_system.rate_limits["login"]["max_attempts"] - 1:
                assert result["valid"] is False
                assert result["reason"] == "Invalid API key"

            # Should block after max attempts
            if i >= self.auth_system.rate_limits["login"]["max_attempts"]:
                assert result["valid"] is False
                assert result["reason"] in [
                    "Too many failed attempts - IP blocked",
                    "IP blocked due to too many failed attempts",
                ]

        # Verify IP is blocked
        assert attacker_ip in self.auth_system.blocked_ips

        # Test that even valid key is rejected for blocked IP
        valid_key_result = validate_api_key_with_rate_limit(
            attacker_ip, "valid_key_2024"
        )
        assert valid_key_result["valid"] is False
        assert "blocked" in valid_key_result["reason"].lower()

    def test_session_hijacking_prevention(self):
        """Test session hijacking prevention mechanisms"""

        # Mock session management with security features
        def create_secure_session(
            user_id: int, ip_address: str, user_agent: str
        ) -> Dict[str, Any]:
            """Create secure session with anti-hijacking measures"""
            session_id = secrets.token_urlsafe(32)
            csrf_token = secrets.token_urlsafe(self.auth_system.csrf_token_length)

            session_data = {
                "session_id": session_id,
                "user_id": user_id,
                "ip_address": ip_address,
                "user_agent_hash": hashlib.sha256(user_agent.encode()).hexdigest(),
                "csrf_token": csrf_token,
                "created_at": time.time(),
                "last_activity": time.time(),
                "is_active": True,
            }

            self.auth_system.active_sessions[session_id] = session_data
            return session_data

        def validate_session(
            session_id: str, ip_address: str, user_agent: str
        ) -> Dict[str, Any]:
            """Validate session with hijacking protection"""

            # Check if session exists
            if session_id not in self.auth_system.active_sessions:
                return {"valid": False, "reason": "Session not found"}

            session = self.auth_system.active_sessions[session_id]

            # Check if session is active
            if not session["is_active"]:
                return {"valid": False, "reason": "Session inactive"}

            # Check session timeout
            current_time = time.time()
            if (
                current_time - session["last_activity"]
                > self.auth_system.session_timeout
            ):
                session["is_active"] = False
                return {"valid": False, "reason": "Session expired"}

            # Check IP address consistency
            if session["ip_address"] != ip_address:
                return {
                    "valid": False,
                    "reason": "IP address mismatch - possible hijacking",
                }

            # Check user agent consistency
            current_user_agent_hash = hashlib.sha256(user_agent.encode()).hexdigest()
            if session["user_agent_hash"] != current_user_agent_hash:
                return {
                    "valid": False,
                    "reason": "User agent mismatch - possible hijacking",
                }

            # Update last activity
            session["last_activity"] = current_time

            return {"valid": True, "user_id": session["user_id"]}

        # Test legitimate session
        legitimate_ip = "192.168.1.50"
        legitimate_user_agent = (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        )

        session = create_secure_session(1, legitimate_ip, legitimate_user_agent)

        # Valid session access
        result = validate_session(
            session["session_id"], legitimate_ip, legitimate_user_agent
        )
        assert result["valid"] is True
        assert result["user_id"] == 1

        # Test hijacking attempt with different IP
        hijacker_ip = "10.0.0.100"
        result = validate_session(
            session["session_id"], hijacker_ip, legitimate_user_agent
        )
        assert result["valid"] is False
        assert "IP address mismatch" in result["reason"]

        # Test hijacking attempt with different user agent
        hijacker_user_agent = (
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
        )
        result = validate_session(
            session["session_id"], legitimate_ip, hijacker_user_agent
        )
        assert result["valid"] is False
        assert "User agent mismatch" in result["reason"]

        # Test session timeout
        # Manually expire session
        session["last_activity"] = time.time() - (self.auth_system.session_timeout + 1)
        result = validate_session(
            session["session_id"], legitimate_ip, legitimate_user_agent
        )
        assert result["valid"] is False
        assert "expired" in result["reason"]

    def test_authentication_bypass_attempts(self):
        """Test various authentication bypass attempt scenarios"""

        # Mock authentication endpoint with bypass protection
        def authenticate_user(credentials: Dict[str, Any]) -> Dict[str, Any]:
            """Authenticate user with bypass protection"""

            # Prevent null/empty authentication
            if not credentials or not isinstance(credentials, dict):
                return {"success": False, "reason": "Invalid credentials format"}

            username = credentials.get("username", "").strip()
            password = credentials.get("password", "")
            api_key = credentials.get("api_key", "").strip()

            # Prevent bypass attempts with special characters
            bypass_patterns = [
                "' OR '1'='1",
                "' OR 1=1 --",
                '" OR "1"="1',
                "1=1 --",
                "admin'--",
                "admin'#",
                "' UNION SELECT",
                "NULL",
                "undefined",
                "",
                " ",
            ]

            for auth_field in [username, password, api_key]:
                if auth_field.lower() in [
                    pattern.lower() for pattern in bypass_patterns
                ]:
                    return {
                        "success": False,
                        "reason": "SQL injection attempt detected",
                    }

            # Normal authentication logic
            if api_key and api_key in self.auth_system.api_keys:
                return {
                    "success": True,
                    "user_data": self.auth_system.api_keys[api_key],
                    "auth_method": "api_key",
                }
            elif username and password:
                # Simulate database user lookup (mock)
                valid_users = {
                    "admin": self.hash_password("admin_password")[0],
                    "user": self.hash_password("user_password")[0],
                }

                if username in valid_users:
                    stored_hash, salt = self.hash_password(
                        password
                    )  # Mock salt retrieval
                    # In real implementation, would use stored salt
                    if self.verify_password(
                        password, valid_users[username], "mock_salt"
                    ):
                        return {
                            "success": True,
                            "username": username,
                            "auth_method": "password",
                        }

            return {"success": False, "reason": "Invalid credentials"}

        # Test various bypass attempts
        bypass_attempts = [
            # SQL injection attempts
            {"username": "' OR '1'='1", "password": "anything"},
            {"username": "admin'--", "password": ""},
            {"username": '" OR "1"="1', "password": "password"},
            {"api_key": "' UNION SELECT * FROM users --"},
            # Null/empty attempts
            {"username": None, "password": "password"},
            {"username": "", "password": ""},
            {"api_key": "NULL"},
            {"api_key": "undefined"},
            # Special character attempts
            {"username": "admin\x00", "password": "password"},
            {"api_key": "admin\r\nDELETE FROM users"},
            # Array/object attempts (if parser supports)
            {"username": ["admin", "user"], "password": "password"},
        ]

        for attempt in bypass_attempts:
            result = authenticate_user(attempt)
            assert result["success"] is False, (
                f"Bypass attempt should have failed: {attempt}"
            )
            assert (
                "invalid" in result["reason"].lower()
                or "injection" in result["reason"].lower()
            )

        # Test legitimate authentication
        legitimate_auth = {"api_key": "valid_key_2024"}
        result = authenticate_user(legitimate_auth)
        assert result["success"] is True
        assert result["auth_method"] == "api_key"

    def test_rate_limiting_effectiveness(self):
        """Test rate limiting effectiveness against various attack patterns"""

        # Mock rate limiting system
        class RateLimiter:
            def __init__(self):
                self.requests = {}
                self.limits = {
                    "auth": {"max_requests": 10, "window_seconds": 60},
                    "api": {"max_requests": 100, "window_seconds": 60},
                    "upload": {"max_requests": 5, "window_seconds": 60},
                }

            def is_allowed(self, ip_address: str, endpoint_type: str) -> Dict[str, Any]:
                """Check if request is allowed based on rate limits"""
                current_time = time.time()

                # Initialize IP tracking
                if ip_address not in self.requests:
                    self.requests[ip_address] = {}

                if endpoint_type not in self.requests[ip_address]:
                    self.requests[ip_address][endpoint_type] = []

                # Clean old requests outside window
                window_start = (
                    current_time - self.limits[endpoint_type]["window_seconds"]
                )
                self.requests[ip_address][endpoint_type] = [
                    req_time
                    for req_time in self.requests[ip_address][endpoint_type]
                    if req_time > window_start
                ]

                # Check limit
                request_count = len(self.requests[ip_address][endpoint_type])
                max_requests = self.limits[endpoint_type]["max_requests"]

                if request_count >= max_requests:
                    return {
                        "allowed": False,
                        "reason": f"Rate limit exceeded: {request_count}/{max_requests}",
                        "retry_after": self.limits[endpoint_type]["window_seconds"],
                    }

                # Record request
                self.requests[ip_address][endpoint_type].append(current_time)

                return {"allowed": True, "remaining": max_requests - request_count - 1}

        rate_limiter = RateLimiter()

        # Test rate limiting for different endpoints
        test_cases = [
            ("192.168.1.100", "auth", 15),  # Should be limited after 10
            ("192.168.1.101", "api", 105),  # Should be limited after 100
            ("192.168.1.102", "upload", 7),  # Should be limited after 5
        ]

        for ip, endpoint_type, request_count in test_cases:
            allowed_requests = 0
            blocked_requests = 0

            for i in range(request_count):
                result = rate_limiter.is_allowed(ip, endpoint_type)

                if result["allowed"]:
                    allowed_requests += 1
                else:
                    blocked_requests += 1
                    assert "Rate limit exceeded" in result["reason"]
                    assert "retry_after" in result

            # Verify rate limiting worked
            expected_max = rate_limiter.limits[endpoint_type]["max_requests"]
            assert allowed_requests == expected_max, (
                f"Expected {expected_max} allowed requests, got {allowed_requests}"
            )
            assert blocked_requests > 0, (
                f"Expected some blocked requests for {endpoint_type}"
            )

    def test_input_sanitization(self):
        """Test input sanitization in authentication contexts"""

        # Input sanitization functions
        def sanitize_string_input(input_str: str) -> str:
            """Sanitize string input for authentication"""
            if not isinstance(input_str, str):
                return ""

            # Remove null bytes
            sanitized = input_str.replace("\x00", "")

            # Remove control characters except newlines and tabs
            sanitized = "".join(
                char for char in sanitized if ord(char) >= 32 or char in "\n\t"
            )

            # Trim whitespace
            sanitized = sanitized.strip()

            # Limit length
            if len(sanitized) > 1000:
                sanitized = sanitized[:1000]

            return sanitized

        def sanitize_api_key(api_key: str) -> str:
            """Sanitize API key input"""
            sanitized = sanitize_string_input(api_key)

            # Only allow alphanumeric and specific symbols in API keys
            allowed_chars = set(
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
            )
            sanitized = "".join(char for char in sanitized if char in allowed_chars)

            return sanitized

        # Test sanitization of dangerous inputs
        dangerous_inputs = [
            "admin\x00password",  # Null byte injection
            "admin\r\nDELETE FROM users",  # Command injection
            "admin<script>alert('XSS')</script>",  # XSS attempt
            "admin' OR '1'='1",  # SQL injection
            "../../../etc/passwd",  # Path traversal
            "admin" * 1000,  # Buffer overflow attempt
            "",  # Empty input
            None,  # None input
        ]

        for dangerous_input in dangerous_inputs:
            # Test string sanitization
            sanitized = sanitize_string_input(dangerous_input)

            # Verify dangerous characters are removed/escaped
            assert "\x00" not in sanitized, "Null bytes should be removed"
            assert "\r" not in sanitized or "\n" not in sanitized, (
                "Control chars should be limited"
            )
            assert len(sanitized) <= 1000, "Length should be limited"

            # Test API key sanitization
            if dangerous_input:
                sanitized_key = sanitize_api_key(str(dangerous_input))
                # Should only contain allowed characters
                for char in sanitized_key:
                    assert (
                        char
                        in "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
                    ), f"Invalid character in API key: {char}"

        # Test legitimate inputs are preserved
        legitimate_inputs = [
            "valid_user_123",
            "api-key-2024-valid",
            "user@example.com",
            "Normal string input 123",
        ]

        for legitimate_input in legitimate_inputs:
            sanitized = sanitize_string_input(legitimate_input)
            assert legitimate_input.strip() == sanitized, (
                f"Legitimate input should be preserved: {legitimate_input}"
            )

    def test_concurrent_authentication_security(self):
        """Test authentication security under concurrent load"""
        # Test concurrent authentication attempts
        auth_results = []
        auth_lock = threading.Lock()

        def concurrent_auth_worker(worker_id: int, credentials: Dict[str, Any]):
            """Worker for concurrent authentication testing"""
            try:
                # Simulate authentication delay
                time.sleep(0.01)

                # Mock authentication
                if (
                    "api_key" in credentials
                    and credentials["api_key"] in self.auth_system.api_keys
                ):
                    success = True
                    reason = "Valid credentials"
                else:
                    success = False
                    reason = "Invalid credentials"

                with auth_lock:
                    auth_results.append(
                        {
                            "worker_id": worker_id,
                            "success": success,
                            "reason": reason,
                            "timestamp": time.time(),
                        }
                    )

            except Exception as e:
                with auth_lock:
                    auth_results.append(
                        {
                            "worker_id": worker_id,
                            "success": False,
                            "reason": f"Error: {str(e)}",
                            "timestamp": time.time(),
                        }
                    )

        # Create concurrent authentication threads
        threads = []
        test_credentials = [
            (1, {"api_key": "valid_key_2024"}),
            (2, {"api_key": "invalid_key_1"}),
            (3, {"api_key": "admin_key_2024"}),
            (4, {"api_key": "invalid_key_2"}),
            (5, {"api_key": "limited_key_2024"}),
            (6, {"api_key": "invalid_key_3"}),
            (7, {"api_key": "nonexistent"}),
            (8, {"username": "admin", "password": "wrong"}),
        ]

        # Start concurrent authentication attempts
        for worker_id, credentials in test_credentials:
            thread = threading.Thread(
                target=concurrent_auth_worker, args=(worker_id, credentials)
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=10)

        # Verify results
        assert len(auth_results) == 8

        # Count successful and failed attempts
        successful_auths = [r for r in auth_results if r["success"]]
        failed_auths = [r for r in auth_results if not r["success"]]

        # Should have 3 valid API keys from our test data
        assert len(successful_auths) == 3
        assert len(failed_auths) == 5

        # Verify successful authentications
        successful_keys = [r["worker_id"] for r in successful_auths]
        assert 1 in successful_keys  # valid_key_2024
        assert 3 in successful_keys  # admin_key_2024
        assert 5 in successful_keys  # limited_key_2024

    def test_csrf_token_security(self):
        """Test CSRF token generation and validation"""

        # Mock CSRF token system
        class CSRFProtection:
            def __init__(self):
                self.tokens = {}
                self.token_length = 32

            def generate_token(self, session_id: str) -> str:
                """Generate CSRF token for session"""
                token = secrets.token_urlsafe(self.token_length)
                self.tokens[session_id] = {
                    "token": token,
                    "created_at": time.time(),
                    "used": False,
                }
                return token

            def validate_token(
                self, session_id: str, provided_token: str
            ) -> Dict[str, Any]:
                """Validate CSRF token"""
                if session_id not in self.tokens:
                    return {"valid": False, "reason": "No token for session"}

                token_data = self.tokens[session_id]

                # Check if token matches
                if not hmac.compare_digest(token_data["token"], provided_token):
                    return {"valid": False, "reason": "Token mismatch"}

                # Check if token already used (prevent replay)
                if token_data["used"]:
                    return {"valid": False, "reason": "Token already used"}

                # Mark token as used
                token_data["used"] = True

                return {"valid": True}

            def cleanup_expired_tokens(self, max_age_seconds: int = 3600):
                """Clean up expired tokens"""
                current_time = time.time()
                expired_sessions = []

                for session_id, token_data in self.tokens.items():
                    if current_time - token_data["created_at"] > max_age_seconds:
                        expired_sessions.append(session_id)

                for session_id in expired_sessions:
                    del self.tokens[session_id]

                return len(expired_sessions)

        csrf_protection = CSRFProtection()

        # Test token generation
        session_id = "test_session_123"
        token = csrf_protection.generate_token(session_id)

        assert isinstance(token, str)
        assert len(token) >= csrf_protection.token_length
        assert session_id in csrf_protection.tokens

        # Test token validation
        result = csrf_protection.validate_token(session_id, token)
        assert result["valid"] is True

        # Test token reuse prevention
        result = csrf_protection.validate_token(session_id, token)
        assert result["valid"] is False
        assert "already used" in result["reason"]

        # Test invalid token
        result = csrf_protection.validate_token(session_id, "invalid_token")
        assert result["valid"] is False
        assert "mismatch" in result["reason"]

        # Test token for non-existent session
        result = csrf_protection.validate_token("nonexistent_session", token)
        assert result["valid"] is False
        assert "No token for session" in result["reason"]

        # Test multiple tokens for different sessions
        sessions = ["session1", "session2", "session3"]
        tokens = {}

        for session in sessions:
            tokens[session] = csrf_protection.generate_token(session)

        # Each token should be valid for its session only
        for session in sessions:
            # Valid for correct session
            result = csrf_protection.validate_token(session, tokens[session])
            assert result["valid"] is True

            # Invalid for other sessions
            for other_session in sessions:
                if other_session != session:
                    result = csrf_protection.validate_token(
                        other_session, tokens[session]
                    )
                    assert result["valid"] is False
