# REST API Documentation

## 🔌 Integral Philosophy Publishing System API

A comprehensive REST API providing programmatic access to all content processing pipeline components.

### 🚀 Quick Start

```bash
# Start API server
export INTEGRAL_API_KEY=your-secret-key
python3 api_server.py

# API will be available at http://localhost:5001
```

### 🔐 Authentication

All API endpoints require authentication via API key:

```bash
# Set API key in environment
export INTEGRAL_API_KEY=your-secure-api-key

# Or pass in header
curl -H "X-API-Key: your-secure-api-key" http://localhost:5001/api/v1/health
```

### 📋 API Endpoints

#### **Health & Info**

| Endpoint | Method | Description |
|----------|---------|-------------|
| `/api/v1/health` | GET | Check API health |
| `/api/v1/info` | GET | API information and capabilities |

#### **Content Processing**

| Endpoint | Method | Description |
|----------|---------|-------------|
| `/api/v1/convert` | POST | Convert between formats |
| `/api/v1/scrape` | POST | Scrape website content |
| `/api/v1/tei` | POST | Generate TEI XML |
| `/api/v1/uml` | POST | Generate UML diagrams |
| `/api/v1/pipeline` | POST | Run full pipeline |

#### **Job Management**

| Endpoint | Method | Description |
|----------|---------|-------------|
| `/api/v1/jobs` | GET | List all jobs |
| `/api/v1/jobs/<job_id>` | GET | Get job status |
| `/api/v1/jobs/<job_id>` | DELETE | Delete job |

#### **Plugin System**

| Endpoint | Method | Description |
|----------|---------|-------------|
| `/api/v1/plugins` | GET | List available plugins |

### 🔧 API Usage Examples

#### **Format Conversion**

```bash
curl -X POST http://localhost:5001/api/v1/convert \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "input_content": "# Hello World\n\nThis is a test document with math: $E = mc^2$",
    "input_format": "markdown",
    "output_format": "html"
  }'
```

**Response:**
```json
{
  "job_id": "12345678-1234-1234-1234-123456789012",
  "status": "submitted",
  "message": "Conversion job submitted successfully"
}
```

#### **Web Scraping**

```bash
curl -X POST http://localhost:5001/api/v1/scrape \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "url": "https://philosophynow.org",
    "max_pages": 10,
    "depth": 2
  }'
```

#### **TEI Generation**

```bash
curl -X POST http://localhost:5001/api/v1/tei \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "input_content": "# Academic Paper\n\nContent with citations and math formulas.",
    "metadata": {
      "title": "Example Paper",
      "author": "John Doe",
      "language": "en",
      "date": "2025-01-15"
    }
  }'
```

#### **UML Generation**

```bash
curl -X POST http://localhost:5001/api/v1/uml \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "input_content": "# Main Document\n\n## Section 1\nContent here.\n\n## Section 2\nMore content.",
    "format": "plantuml"
  }'
```

#### **Full Pipeline**

```bash
curl -X POST http://localhost:5001/api/v1/pipeline \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "source": "https://plato.stanford.edu/entries/descartes-epistemology/",
    "options": {
      "scraping": {"depth": 2, "js_render": true},
      "processing": {"generate_tei": true, "generate_uml": true},
      "output": {"formats": ["html", "tei", "pdf"]}
    }
  }'
```

### 📊 Job Status Monitoring

#### **Check Job Status**

```bash
curl -H "X-API-Key: your-api-key" \
  http://localhost:5001/api/v1/jobs/12345678-1234-1234-1234-123456789012
```

**Response:**
```json
{
  "job_id": "12345678-1234-1234-1234-123456789012",
  "job_type": "convert",
  "status": "completed",
  "progress": 100,
  "start_time": "2025-01-15T10:30:00",
  "end_time": "2025-01-15T10:30:45",
  "result": {
    "output_content": "<h1>Hello World</h1>\n\n<p>This is a test document with math: <em>E = mc²</em></p>",
    "output_format": "html",
    "file_size": 1024
  }
}
```

#### **List All Jobs**

```bash
curl -H "X-API-Key: your-api-key" \
  "http://localhost:5001/api/v1/jobs?limit=10&status=completed"
```

### 🔄 Supported Formats

#### **Input Formats**
- `markdown` - Markdown with LaTeX support
- `html` - HTML5 semantic markup
- `latex` - LaTeX documents
- `org` - Org Mode files
- `asciidoc` - AsciiDoc documents
- `rst` - reStructuredText
- `typst` - Typst documents

#### **Output Formats**
- `html` - Responsive HTML5
- `latex` - LaTeX source
- `org` - Org Mode
- `asciidoc` - AsciiDoc
- `rst` - reStructuredText
- `typst` - Typst
- `tei` - TEI XML
- `pdf` - PDF via LaTeX
- `epub` - E-book format
- `docx` - Microsoft Word

#### **UML Formats**
- `plantuml` - PlantUML syntax
- `mermaid` - Mermaid.js format
- `graphviz` - DOT format for Graphviz

### ⚡ Rate Limiting

- **Default Limit**: 100 requests per hour per API key
- **Burst Limit**: 10 requests per minute
- **Headers**: Rate limit info in response headers

```bash
# Check rate limits
curl -I -H "X-API-Key: your-api-key" http://localhost:5001/api/v1/health

# Response headers:
# X-RateLimit-Limit: 100
# X-RateLimit-Remaining: 95
# X-RateLimit-Reset: 1642245600
```

### 🚨 Error Handling

#### **HTTP Status Codes**
- `200` - Success
- `201` - Created
- `202` - Accepted (job submitted)
- `400` - Bad Request
- `401` - Unauthorized
- `404` - Not Found
- `429` - Too Many Requests
- `500` - Internal Server Error

#### **Error Response Format**
```json
{
  "error": "Detailed error message",
  "error_code": "VALIDATION_ERROR",
  "timestamp": "2025-01-15T10:30:00Z",
  "request_id": "req-123456789"
}
```

### 🔌 Plugin Development

#### **Plugin Structure**
```python
class CustomPlugin:
    def __init__(self):
        self.name = "CustomPlugin"
        self.version = "1.0.0"
        self.description = "Custom processing plugin"
    
    def process(self, content, options):
        # Custom processing logic
        return processed_content
    
    def validate(self, content):
        # Content validation
        return True
```

#### **Register Plugin**
```bash
# Add to plugins directory
mkdir plugins
cp custom_plugin.py plugins/

# Restart API server
python3 api_server.py
```

### 🔧 Configuration

#### **Environment Variables**
```bash
export INTEGRAL_API_KEY="your-secure-api-key"
export FLASK_ENV="production"
export LOG_LEVEL="INFO"
export MAX_CONCURRENT_JOBS="10"
export DEFAULT_TIMEOUT="300"
```

#### **Configuration File**
```json
{
  "api": {
    "version": "v1",
    "rate_limiting": {
      "requests_per_hour": 100,
      "burst_limit": 10
    },
    "job_limits": {
      "max_file_size": "100MB",
      "max_processing_time": "30min"
    }
  },
  "processing": {
    "default_formats": ["html", "tei"],
    "enable_uml_generation": true,
    "enable_bibliography": true
  }
}
```

### 🔒 Security Best Practices

#### **API Key Management**
```bash
# Generate secure API key
openssl rand -hex 32

# Set in environment (not in code)
export INTEGRAL_API_KEY="your-generated-key"

# Rotate keys regularly
# Set expiration dates
# Use different keys for different environments
```

#### **Input Validation**
- All inputs validated and sanitized
- File size limits enforced
- Format restrictions applied
- SQL injection protection

#### **HTTPS Configuration**
```nginx
# SSL/TLS setup
server {
    listen 443 ssl;
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
}
```

### 📊 Monitoring & Analytics

#### **API Metrics**
```bash
# Get API metrics
curl -H "X-API-Key: your-api-key" \
  http://localhost:5001/api/v1/metrics

# Response includes:
# - Request count by endpoint
# - Average response times
# - Error rates
# - Active job counts
```

#### **Webhook Integration**
```bash
# Configure webhooks for job completion
curl -X POST http://localhost:5001/api/v1/webhooks \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "url": "https://your-service.com/webhook",
    "events": ["job_completed", "job_failed"],
    "secret": "webhook-secret"
  }'
```

### 🧪 Testing

#### **API Testing with curl**
```bash
# Health check
curl -H "X-API-Key: your-key" http://localhost:5001/api/v1/health

# Test conversion
curl -X POST http://localhost:5001/api/v1/convert \
  -H "Content-Type: application/json" \
  -H "X-API-Key: your-key" \
  -d '{"input_content": "# Test", "output_format": "html"}'
```

#### **Python Client Example**
```python
import requests

class IntegralAPIClient:
    def __init__(self, api_key, base_url="http://localhost:5001/api/v1"):
        self.api_key = api_key
        self.base_url = base_url
        self.headers = {
            "Content-Type": "application/json",
            "X-API-Key": api_key
        }
    
    def convert(self, content, output_format):
        response = requests.post(
            f"{self.base_url}/convert",
            headers=self.headers,
            json={
                "input_content": content,
                "output_format": output_format
            }
        )
        return response.json()
    
    def get_job_status(self, job_id):
        response = requests.get(
            f"{self.base_url}/jobs/{job_id}",
            headers=self.headers
        )
        return response.json()

# Usage
client = IntegralAPIClient("your-api-key")
job = client.convert("# Hello World", "html")
status = client.get_job_status(job["job_id"])
```

### 📚 SDK & Libraries

#### **Official Python SDK**
```bash
pip install integral-philosophy-sdk
```

```python
from integral_philosophy import IntegralClient

client = IntegralClient(api_key="your-key")
result = client.convert("# Hello", "html")
```

#### **JavaScript SDK**
```bash
npm install integral-philosophy-js
```

```javascript
import { IntegralClient } from 'integral-philosophy-js';

const client = new IntegralClient({ apiKey: 'your-key' });
client.convert('# Hello', 'html').then(console.log);
```

---

**🔌 The REST API provides comprehensive programmatic access to the Integral Philosophy Publishing System, enabling integration with external tools, workflows, and applications!**