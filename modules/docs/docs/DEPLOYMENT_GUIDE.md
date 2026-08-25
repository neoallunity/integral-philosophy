# Production Deployment Guide

## 🐳 Docker Production Setup for Integral Philosophy Publishing System

### 🚀 Quick Deployment

```bash
# Clone and setup
git clone <repository-url>
cd Integral-Philosophy-Publishing-System

# Deploy with Docker
./deploy_docker.sh
```

The system will be available at:
- **Main Application**: http://localhost
- **Web Interface**: http://localhost:5000
- **Grafana Monitoring**: http://localhost:3000 (admin/admin123)
- **Prometheus Metrics**: http://localhost:9090

### 📋 Prerequisites

#### System Requirements
- **Operating System**: Linux (Ubuntu 20.04+, CentOS 8+, Debian 11+)
- **Memory**: Minimum 4GB RAM, 8GB+ recommended
- **Storage**: Minimum 20GB free space, 50GB+ recommended
- **CPU**: Minimum 2 cores, 4+ cores recommended

#### Software Requirements
- **Docker**: Version 20.10 or later
- **Docker Compose**: Version 2.0 or later
- **Git**: For source code management
- **Ports**: 80, 443, 5000, 3000, 9090 available

#### Network Requirements
- **Internet Connection**: For web scraping and updates
- **Bandwidth**: 10Mbps+ for optimal performance
- **Firewall**: Open ports for web interface and monitoring

### 🏗️ Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Production Environment                     │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │
│  │    Nginx   │  │   Redis     │  │ Prometheus  │  │
│  │  (Proxy)    │  │  (Cache)    │  │ (Metrics)   │  │
│  └─────────────┘  └─────────────┘  └─────────────┘  │
│         │               │               │             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │        Integral Philosophy App                   │  │
│  │    (Flask + Processing Pipeline)              │  │
│  └─────────────────────────────────────────────────────┘  │
│                       │                              │
│              ┌─────────────┐                        │
│              │   Grafana   │                        │
│              │(Dashboard)  │                        │
│              └─────────────┘                        │
└─────────────────────────────────────────────────────────────┘
```

### 🔧 Configuration Files

#### **Dockerfile**
- **Base Image**: Python 3.11-slim
- **System Dependencies**: Pandoc, LaTeX, Node.js, Chrome
- **Application**: Flask web interface
- **Health Check**: HTTP endpoint monitoring

#### **docker-compose.yml**
- **Services**: Application, Redis, Nginx, Prometheus, Grafana
- **Networking**: Docker bridge network
- **Volumes**: Persistent storage for logs and data
- **Restart Policies**: Automatic recovery

#### **Nginx Configuration**
```nginx
# Reverse proxy for application
upstream integral_app {
    server integral-philosophy:5000;
}

server {
    listen 80;
    server_name localhost;
    
    location / {
        proxy_pass http://integral_app;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### 🚀 Deployment Steps

#### 1. **Environment Setup**
```bash
# Create deployment directory
mkdir -p /opt/integral-philosophy
cd /opt/integral-philosophy

# Clone repository
git clone <repository-url> .

# Set permissions
chown -R $USER:$USER .
chmod +x *.sh
```

#### 2. **Docker Deployment**
```bash
# Build and start services
./deploy_docker.sh

# Or manually:
docker-compose build
docker-compose up -d
```

#### 3. **Service Verification**
```bash
# Check service status
docker-compose ps

# Check application health
curl -f http://localhost:5000/status

# View logs
docker-compose logs -f
```

### 📊 Monitoring Setup

#### **Prometheus Metrics**
- **Application Metrics**: Job processing, performance, errors
- **System Metrics**: CPU, memory, disk usage
- **Custom Metrics**: Conversion success rates, processing times
- **Endpoint**: http://localhost:9090

#### **Grafana Dashboard**
- **URL**: http://localhost:3000
- **Credentials**: admin/admin123
- **Dashboards**: Pre-configured for system monitoring
- **Alerts**: Configurable for system health

#### **Log Management**
- **Application Logs**: `/var/log/integral-philosophy/`
- **Access Logs**: Nginx access logs
- **Error Logs**: Application error tracking
- **Docker Logs**: Container log aggregation

### 🔒 Security Configuration

#### **Network Security**
```bash
# Configure firewall
ufw allow 80/tcp
ufw allow 443/tcp
ufw allow 5000/tcp  # Optional, for direct access
ufw enable

# SSL/TLS Setup (Production)
# 1. Obtain SSL certificate
certbot --nginx -d yourdomain.com

# 2. Update nginx configuration
# 3. Restart services
docker-compose restart nginx
```

#### **Application Security**
```yaml
# docker-compose.yml security settings
services:
  integral-philosophy:
    user: "1000:1000"  # Non-root user
    read_only: true      # Read-only filesystem
    tmpfs:
      - /tmp           # Temporary filesystem
    security_opt:
      - no-new-privileges:true
```

#### **Environment Variables**
```bash
# Production environment
export FLASK_ENV=production
export FLASK_DEBUG=0
export SECRET_KEY=your-secret-key-here
export REDIS_URL=redis://redis:6379/0
```

### 📈 Performance Optimization

#### **Resource Scaling**
```yaml
# docker-compose.yml scaling
services:
  integral-philosophy:
    deploy:
      replicas: 3  # Horizontal scaling
      resources:
        limits:
          cpus: '1.0'
          memory: 2G
        reservations:
          cpus: '0.5'
          memory: 1G
```

#### **Caching Strategy**
- **Redis Caching**: Job status, conversion results
- **File Caching**: Generated documents, intermediate files
- **Browser Caching**: Static assets optimization
- **CDN Integration**: For static asset delivery

#### **Database Optimization**
```python
# Connection pooling
DATABASE_CONFIG = {
    'pool_size': 20,
    'max_overflow': 30,
    'pool_timeout': 30,
    'pool_recycle': 3600
}

# Query optimization
INDEXES = [
    'jobs(job_id, status)',
    'users(email)',
    'conversions(job_id, format)'
]
```

### 🔧 Maintenance Operations

#### **Backup Strategy**
```bash
# Daily backup script
#!/bin/bash
DATE=$(date +%Y%m%d)
BACKUP_DIR="/backups/integral-philosophy/$DATE"

# Create backup
mkdir -p $BACKUP_DIR

# Backup data
docker run --rm -v integral_phosophy_redis_data:/data \
    -v $BACKUP_DIR:/backup redis:latest \
    tar czf /backup/redis_$DATE.tar.gz /data

# Backup application data
tar czf $BACKUP_DIR/app_data_$DATE.tar.gz \
    web_jobs/ logs/ config/

# Cleanup old backups (7 days)
find /backups -name "*.tar.gz" -mtime +7 -delete
```

#### **Updates and Upgrades**
```bash
# Update application
git pull origin main
docker-compose build
docker-compose up -d

# Update dependencies
docker-compose pull
docker-compose up -d

# Zero-downtime deployment
docker-compose up -d --no-deps integral-philosophy
```

#### **Health Monitoring**
```bash
# Health check script
#!/bin/bash
APP_STATUS=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:5000/status)
REDIS_STATUS=$(docker-compose exec -T redis redis-cli ping)

if [ "$APP_STATUS" != "200" ] || [ "$REDIS_STATUS" != "PONG" ]; then
    echo "Service health check failed"
    # Send alert
    curl -X POST "https://alerts.example.com/webhook" \
         -d "service=integral-philosophy&status=unhealthy"
fi
```

### 🚨 Troubleshooting

#### **Common Issues**

1. **Container Won't Start**
```bash
# Check logs
docker-compose logs integral-philosophy

# Check resources
docker stats

# Check port conflicts
netstat -tulpn | grep :5000
```

2. **Performance Issues**
```bash
# Monitor resources
docker stats --no-stream

# Check disk space
df -h

# Monitor database
docker-compose exec redis redis-cli INFO
```

3. **Web Scraping Issues**
```bash
# Check Chrome/ChromeDriver
docker-compose exec integral-philosophy google-chrome --version

# Check network connectivity
docker-compose exec integral-philosophy ping google.com

# Verify DNS
docker-compose exec integral-philosophy nslookup example.com
```

#### **Debug Mode**
```bash
# Enable debug logging
docker-compose -f docker-compose.yml -f docker-compose.debug.yml up

# Access container shell
docker-compose exec integral-philosophy bash

# Monitor real-time logs
docker-compose logs -f integral-philosophy
```

### 📚 Advanced Configuration

#### **Kubernetes Deployment**
```yaml
# k8s-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: integral-philosophy
spec:
  replicas: 3
  selector:
    matchLabels:
      app: integral-philosophy
  template:
    metadata:
      labels:
        app: integral-philosophy
    spec:
      containers:
      - name: app
        image: integral-philosophy:latest
        ports:
        - containerPort: 5000
        env:
        - name: FLASK_ENV
          value: "production"
```

#### **Load Balancing**
```nginx
# nginx.conf with load balancing
upstream integral_backend {
    least_conn;
    server integral-1:5000;
    server integral-2:5000;
    server integral-3:5000;
}
```

#### **CI/CD Integration**
```yaml
# .github/workflows/deploy.yml
name: Deploy to Production
on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Deploy to Docker
      run: |
        docker-compose build
        docker-compose push
        docker-compose up -d
```

### 🔔 Alert Configuration

#### **Grafana Alerts**
```json
{
  "alerting": {
    "rules": [
      {
        "name": "HighErrorRate",
        "condition": "error_rate > 0.1",
        "duration": "5m",
        "action": "webhook"
      },
      {
        "name": "HighMemoryUsage",
        "condition": "memory_usage > 0.8",
        "duration": "10m",
        "action": "email"
      }
    ]
  }
}
```

#### **Slack Integration**
```python
# Alert webhook
import requests

def send_alert(message):
    webhook_url = "https://hooks.slack.com/services/..."
    payload = {"text": message}
    requests.post(webhook_url, json=payload)
```

---

**🐳 This Docker deployment provides a production-ready, scalable, and monitored Integral Philosophy Publishing System suitable for enterprise use!**