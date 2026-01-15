# Phase 1 API Tests - Integral Philosophy Publishing System

## Overview

This directory contains comprehensive Phase 1 API tests for the Integral Philosophy Publishing System. The test suite provides thorough coverage of authentication, job management, API endpoints, and file operations with extensive security and performance testing.

## Test Structure

### 📁 Test Files

1. **`test_authentication.py`** - Authentication and authorization tests
2. **`test_job_management.py`** - Job creation, tracking, and management tests  
3. **`test_endpoints.py`** - HTTP methods and API endpoint tests
4. **`test_file_operations.py`** - File upload, download, and security tests

### 🏗️ Test Infrastructure

- **Base Class**: All tests inherit from `APITestCase` in `../utils/base_test_classes.py`
- **Fixtures**: Uses pytest fixtures from `../conftest.py` for test setup
- **Mocking**: Comprehensive mocking of external services and dependencies
- **Configuration**: Test automatically detects component availability and skips if needed

## Test Coverage

### 🔐 Authentication Tests (`test_authentication.py`)

- **API Key Validation**: Valid, invalid, missing, expired, and format validation
- **Security Testing**: Authentication bypass attempts, CSRF protection, scope validation
- **Rate Limiting**: Request rate limiting, IP-based limiting, concurrent request handling
- **Session Management**: Session creation, tracking, and cleanup
- **Performance**: Authentication performance under load (under 50ms per request)

**Total Tests**: 15 comprehensive test methods

### 🔄 Job Management Tests (`test_job_management.py`)

- **Job Creation**: All pipeline types (convert, scrape, TEI, UML, pipeline)
- **Status Tracking**: Real-time job status monitoring and lifecycle management
- **Concurrent Handling**: Multiple simultaneous job creation and tracking
- **Error Scenarios**: Invalid parameters, timeout handling, error recovery
- **Performance**: Job creation performance under load (20 concurrent jobs in <5s)

**Total Tests**: 17 comprehensive test methods

### 🔌 API Endpoint Tests (`test_endpoints.py`)

- **HTTP Methods**: GET, POST, DELETE operations on all endpoints
- **Request/Response**: JSON validation, content negotiation, error handling
- **API Features**: Versioning support, CORS headers, webhooks, batch operations
- **Security**: Parameter encoding, request ID tracking, method override protection
- **Performance**: Response time testing (<1s for most endpoints)

**Total Tests**: 24 comprehensive test methods

### 📁 File Operations Tests (`test_file_operations.py`)

- **File Upload**: Supported formats, size limits, malicious file detection
- **File Download**: Access control, invalid ID handling, path traversal prevention
- **Security**: Filename sanitization, content scanning, threat detection
- **Operations**: Temporary file cleanup, metadata extraction, quota management
- **Performance**: File processing performance for various sizes

**Total Tests**: 23 comprehensive test methods

## Test Features

### 🛡️ Security Testing

- **Input Validation**: All inputs validated for injection attacks
- **File Security**: Malicious file detection and path traversal prevention
- **Authentication**: Comprehensive auth testing including bypass attempts
- **Rate Limiting**: Protection against abuse and DoS attacks
- **Headers**: Security headers validation (CORS, CSP, XSS protection)

### ⚡ Performance Testing

- **Response Times**: All endpoints tested for performance benchmarks
- **Concurrent Operations**: Multi-threaded testing for scalability
- **Load Testing**: System behavior under sustained load
- **Resource Management**: Memory and CPU usage monitoring

### 🔧 Error Handling

- **Invalid Requests**: Proper HTTP status codes for all error scenarios
- **Missing Data**: Handling of incomplete or malformed requests
- **Service Failures**: Graceful degradation when dependencies fail
- **Timeout Scenarios**: Job and request timeout handling

### 🧪 Mocking Strategy

- **External Services**: All external dependencies mocked for isolation
- **File System**: Temp files and directory operations mocked
- **Network**: HTTP requests and responses mocked with realistic data
- **Time Operations**: Time-based functions mocked for predictable testing

## Running the Tests

### Prerequisites

The tests require the actual API components to be available. Tests will be automatically skipped if components are not found.

### Quick Start

```bash
# Run all API tests
python -m pytest tests/api/ -v

# Run specific test file
python -m pytest tests/api/test_authentication.py -v

# Run with performance markers
python -m pytest tests/api/ -m "slow" -v

# Run the test validation script
python run_api_tests.py
```

### Test Categories

```bash
# Authentication tests only
python -m pytest tests/api/test_authentication.py::TestAuthentication -v

# Job management tests only  
python -m pytest tests/api/test_job_management.py::TestJobManagement -v

# Security-focused tests
python -m pytest tests/api/ -k "security" -v

# Performance tests
python -m pytest tests/api/ -m "slow" -v
```

## Test Configuration

### Environment Setup

- **Test Directory**: `/tests/api/`
- **Configuration**: Uses `tests/conftest.py` for shared fixtures
- **Base Classes**: `tests/utils/base_test_classes.py` for common functionality

### Component Detection

The test suite automatically detects component availability:
- API components (`api_server.py`)
- Web interface (`web_interface.py`) 
- Validators (`validators/`)
- Processing scripts (`format_converter.py`, etc.)

Tests are skipped if required components are not available, ensuring clean test runs.

## Integration with CI/CD

### GitHub Actions Integration

```yaml
- name: Run API Tests
  run: |
    python -m pytest tests/api/ -v --tb=short --junitxml=api-test-results.xml
```

### Test Results

- **Exit Codes**: 0 (success), 1 (failure), 2 (interrupt)
- **Reports**: JUnit XML format available for CI integration
- **Coverage**: Can be combined with coverage tools for code metrics

## Future Enhancements

### Phase 2 Plans

- **Load Testing**: Extended performance testing with realistic user loads
- **Integration Testing**: End-to-end workflow testing with real components
- **Contract Testing**: API contract validation and backward compatibility
- **Chaos Testing**: Failure injection and resilience testing

### Monitoring Integration

- **Metrics Collection**: Integration with monitoring systems
- **Alerting**: Performance threshold alerts
- **Trend Analysis**: Historical performance tracking

## Documentation

### API Documentation

Tests serve as living documentation for:
- Expected request/response formats
- Error handling patterns  
- Security requirements
- Performance expectations

### Test Examples

Each test provides examples of:
- Proper API usage patterns
- Error handling scenarios
- Security best practices
- Performance benchmarks

## Summary

The Phase 1 API test suite provides:

✅ **79 comprehensive test methods** across 4 test files
✅ **Complete API coverage** including security and performance
✅ **Robust error handling** and edge case testing
✅ **Scalable architecture** for future enhancements
✅ **Production-ready testing** with CI/CD integration

The test suite ensures the Integral Philosophy Publishing System API is secure, performant, and reliable for production use.