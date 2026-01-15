# Integral Philosophy Publishing System - Docker Production Setup
FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    pandoc \
    texlive-full \
    nodejs \
    npm \
    graphviz \
    tidy \
    curl \
    wget \
    git \
    chromium \
    chromium-driver \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js tools for UML generation
RUN npm install -g mermaid-cli plantuml

# Copy requirements and install Python dependencies
COPY requirements.txt web_requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt
RUN pip install --no-cache-dir -r web_requirements.txt

# Copy application code
COPY . .

# Create necessary directories
RUN mkdir -p /app/web_jobs /app/web_templates /app/logs

# Set up permissions
RUN chmod +x /app/*.sh

# Configure environment variables
ENV PYTHONPATH=/app/scripts
ENV FLASK_APP=/app/web_interface.py
ENV FLASK_ENV=production
ENV CHROME_BIN=/usr/bin/chromium
ENV CHROME_DRIVER=/usr/bin/chromedriver

# Expose port
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:5000/status || exit 1

# Run the application
CMD ["python3", "web_interface.py"]