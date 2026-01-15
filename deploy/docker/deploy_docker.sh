#!/bin/bash
# Docker Deployment Script for Integral Philosophy Publishing System

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
	local color=$1
	local message=$2
	echo -e "${color}${message}${NC}"
}

print_status $BLUE "🐳 Docker Deployment - Integral Philosophy Publishing System"
echo "=========================================================="

# Check if Docker is installed
if ! command -v docker &>/dev/null; then
	print_status $RED "❌ Docker is not installed. Please install Docker first."
	exit 1
fi

if ! command -v docker-compose &>/dev/null; then
	print_status $RED "❌ Docker Compose is not installed. Please install Docker Compose first."
	exit 1
fi

print_status $GREEN "✅ Docker and Docker Compose found"

# Create necessary directories
print_status $BLUE "📁 Creating directories..."
mkdir -p logs config ssl grafana/provisioning

# Create configuration files if they don't exist
if [ ! -f "nginx.conf" ]; then
	print_status $YELLOW "📝 Creating Nginx configuration..."
	cat >nginx.conf <<'EOF'
events {
    worker_connections 1024;
}

http {
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

        location /static/ {
            alias /var/www/uploads/;
            expires 1y;
            add_header Cache-Control "public, immutable";
        }
    }
}
EOF
fi

if [ ! -f "prometheus.yml" ]; then
	print_status $YELLOW "📝 Creating Prometheus configuration..."
	cat >prometheus.yml <<'EOF'
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'integral-philosophy'
    static_configs:
      - targets: ['integral-philosophy:5000']
    metrics_path: '/metrics'
    scrape_interval: 5s
EOF
fi

# Build and start containers
print_status $BLUE "🔨 Building Docker image..."
docker-compose build

print_status $BLUE "🚀 Starting containers..."
docker-compose up -d

# Wait for services to be ready
print_status $BLUE "⏳ Waiting for services to start..."
sleep 30

# Check service status
print_status $BLUE "🔍 Checking service status..."

# Check main application
if curl -f http://localhost:5000/status &>/dev/null; then
	print_status $GREEN "✅ Main application is running"
else
	print_status $RED "❌ Main application is not responding"
fi

# Check monitoring services
if curl -f http://localhost:3000 &>/dev/null; then
	print_status $GREEN "✅ Grafana is running at http://localhost:3000"
	print_status $YELLOW "💡 Grafana credentials: admin/admin123"
else
	print_status $YELLOW "⚠️  Grafana is still starting"
fi

if curl -f http://localhost:9090 &>/dev/null; then
	print_status $GREEN "✅ Prometheus is running at http://localhost:9090"
else
	print_status $YELLOW "⚠️  Prometheus is still starting"
fi

# Show logs
print_status $BLUE "📊 Showing application logs..."
echo "=========================================================="
docker-compose logs -f integral-philosophy
