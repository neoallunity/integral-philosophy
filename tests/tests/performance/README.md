# Phase 2 Performance Tests

Comprehensive performance testing suite for the Integral Philosophy Publishing System. This test suite validates system performance under load, benchmarks component performance, stress-tests system limits, and profiles memory usage.

## 🎯 Test Categories

### 1. Load Testing (`tests/performance/load/`)
Tests API performance under concurrent load conditions (10-100 concurrent requests).

**Key Features:**
- Concurrent user simulation (10-100 users)
- Sustained load testing (24-hour operation compressed for CI)
- API endpoint performance validation
- Memory usage monitoring during load
- Response time validation with P95/P99 metrics

**Test Files:**
- `test_api_load.py` - Comprehensive API load testing

### 2. Benchmark Testing (`tests/performance/benchmark/`)
Tests component performance metrics and speed benchmarks.

**Key Features:**
- Format conversion speed benchmarks
- Content processing throughput measurement  
- Web scraping performance metrics
- TEI XML generation speed
- Memory usage profiling per component

**Test Files:**
- `test_component_benchmarks.py` - Component performance benchmarks

### 3. Stress Testing (`tests/performance/stress/`)
Tests system limits and resource exhaustion scenarios.

**Key Features:**
- Maximum concurrent user simulation
- Large file processing limits (up to 500MB)
- Resource exhaustion scenarios (CPU, memory, files, threads)
- System recovery under stress
- Memory leak detection under load

**Test Files:**
- `test_system_limits.py` - System stress and limit testing

### 4. Memory Profiling (`tests/performance/memory/`)
Tests memory leak detection and component memory usage.

**Key Features:**
- Memory leak detection in long-running processes
- Garbage collection efficiency testing
- Component memory footprint analysis
- Large document processing memory usage
- Memory growth trend analysis

**Test Files:**
- `test_memory_profiling.py` - Memory profiling and leak detection

## 🚀 Quick Start

### Prerequisites

```bash
# Install performance testing dependencies
pip install -r tests/performance/requirements.txt

# Ensure base system dependencies are available
# Ubuntu/Debian:
sudo apt-get install python3-dev gcc

# CentOS/RHEL:
sudo yum install python3-devel gcc

# macOS:
xcode-select --install
```

### Running Tests

#### Run All Performance Tests
```bash
# Run all performance test categories
python tests/performance/run_performance_tests.py

# Run with specific output directory
python tests/performance/run_performance_tests.py --test-dir /path/to/tests
```

#### Run Specific Categories
```bash
# Run only load testing
python tests/performance/run_performance_tests.py --categories load

# Run multiple specific categories
python tests/performance/run_performance_tests.py --categories load benchmark memory

# Run individual test files
python -m pytest tests/performance/load/test_api_load.py -v
python -m pytest tests/performance/benchmark/test_component_benchmarks.py -v
python -m pytest tests/performance/stress/test_system_limits.py -v
python -m pytest tests/performance/memory/test_memory_profiling.py -v
```

#### Run with Pytest Directly
```bash
# Run with custom configuration
python -m pytest tests/performance/ -v --tb=short

# Run with performance monitoring
PERFORMANCE_TEST=1 python -m pytest tests/performance/ -v

# Run in parallel for faster execution
python -m pytest tests/performance/ -v -n auto
```

## 📊 Performance Thresholds

### Load Testing Thresholds
- **Max Response Time (P95):** 5.0 seconds
- **Max Response Time (P99):** 10.0 seconds  
- **Min Throughput:** 10 requests/second
- **Max Error Rate:** 5%
- **Max CPU Usage:** 80%
- **Max Memory Increase:** 200MB

### Benchmark Testing Thresholds
- **Format Conversion:** ≤0.01s per KB
- **Content Processing:** ≤0.005s per KB
- **Web Scraping:** ≤5.0s per page
- **TEI Generation:** ≤0.02s per KB

### Stress Testing Thresholds
- **Max Memory Usage:** 2GB
- **Max CPU Usage:** 95%
- **Min Success Rate:** 70%
- **Max Timeout Rate:** 20%
- **Recovery Time:** ≤60 seconds

### Memory Profiling Thresholds
- **Max Memory Leak:** 50MB
- **Max Memory Growth:** 100MB
- **Max GC Pause:** 100ms
- **Min GC Efficiency:** 70%
- **Max Component Memory:** 200MB

## 📈 Reports and Analysis

### Automatic Report Generation
Performance tests automatically generate comprehensive reports in two formats:

1. **JSON Report:** Detailed machine-readable results
2. **Markdown Report:** Human-readable summary

Reports include:
- Executive summary with pass/fail status
- Category-specific performance metrics
- Performance trends and bottlenecks
- Optimization recommendations
- System configuration information

### Report Location
Reports are saved to: `test_results/performance/`

### Example Report Structure
```
test_results/performance/
├── performance_report_20240115_143022.json
├── performance_report_20240115_143022.md
└── individual_test_results/
    ├── test_api_load_results.json
    ├── test_component_benchmarks_results.json
    └── ...
```

## 🔧 Configuration

### Environment Variables
- `PERFORMANCE_TEST=1` - Enable performance monitoring mode
- `CI=1` - Enable CI-optimized settings (reduced test durations)

### Test Configuration Files
Each test category includes configurable thresholds in the test files:

```python
@pytest.fixture
def stress_config(self):
    return {
        "max_concurrent_users": 200,
        "stress_thresholds": {
            "max_memory_mb": 2048,
            "max_cpu_percent": 95,
            # ... more thresholds
        }
    }
```

## 🛠️ Test Infrastructure

### Base Test Classes
All performance tests inherit from `PerformanceTestCase` which provides:

- **Performance Monitoring:** Automatic memory and CPU tracking
- **Metric Collection:** Detailed performance metrics
- **Assertion Helpers:** Performance-based assertions
- **Resource Cleanup:** Automatic cleanup after tests

### Mock Services
Tests include comprehensive mocking for:

- API endpoints with configurable response times
- File processing with size-based complexity
- Database operations with memory simulation
- Network requests with latency simulation

### Data Generation
Dynamic test data generation based on:

- File size specifications
- Content complexity parameters
- Mathematical expression density
- Document structure variations

## 🐛 Troubleshooting

### Common Issues

#### 1. Import Errors
```bash
# Ensure you're in the project root
cd /path/to/Magazine/Magazine
python tests/performance/run_performance_tests.py
```

#### 2. Permission Errors
```bash
# Ensure write permissions for results directory
chmod -R 755 test_results/
```

#### 3. Memory Issues
```bash
# Run with reduced test scope for memory-constrained systems
python tests/performance/run_performance_tests.py --categories benchmark memory
```

#### 4. Test Timeouts
```bash
# Increase timeout or run individual test files
python -m pytest tests/performance/load/test_api_load.py -v --timeout=1800
```

### Debug Mode
Enable detailed logging with:
```bash
PERFORMANCE_TEST=1 python -m pytest tests/performance/ -v -s --tb=long
```

## 📋 CI/CD Integration

### GitHub Actions Example
```yaml
- name: Run Performance Tests
  run: |
    pip install -r tests/performance/requirements.txt
    python tests/performance/run_performance_tests.py --categories load benchmark
```

### Performance Regression Detection
The test suite automatically detects performance regressions by:

- Comparing against configurable thresholds
- Tracking memory growth trends
- Monitoring response time degradation
- Validating recovery performance

## 📚 API Reference

### Test Runner Class
```python
from tests.performance.run_performance_tests import PerformanceTestRunner

runner = PerformanceTestRunner()
results = runner.run_performance_tests(categories=["load", "benchmark"])
```

### Performance Monitor
```python
from tests.utils.base_test_classes import PerformanceTestCase

class MyPerformanceTest(PerformanceTestCase):
    def test_something(self):
        self.start_performance_monitoring()
        # ... perform operations
        metrics = self.stop_performance_monitoring()
        self.assert_performance_within(metrics, max_duration=5.0)
```

## 🤝 Contributing

### Adding New Performance Tests
1. Inherit from `PerformanceTestCase`
2. Use appropriate fixtures for configuration
3. Include comprehensive metric collection
4. Add performance-based assertions
5. Update documentation

### Performance Best Practices
- Always clean up resources in tests
- Use configurable thresholds
- Include both positive and negative test cases
- Monitor memory usage throughout test lifecycle
- Provide meaningful performance metrics

## 📄 License

This performance test suite is part of the Integral Philosophy Publishing System and follows the same licensing terms.

---

## 📞 Support

For questions or issues with the performance test suite:

1. Check the troubleshooting section above
2. Review test logs for detailed error information  
3. Consult the generated performance reports
4. Open an issue with relevant test files and logs