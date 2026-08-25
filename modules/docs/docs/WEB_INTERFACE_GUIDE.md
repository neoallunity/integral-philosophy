# Web Interface Documentation

## 🌐 Integral Philosophy Publishing System - Web UI

A modern, responsive web interface for the Integral Philosophy Publishing System that provides user-friendly access to all pipeline components.

### 🚀 Quick Start

```bash
# Start the web interface
./start_web_interface.sh

# Or manually:
source venv/bin/activate
pip install -r web_requirements.txt
python3 web_interface.py
```

The web interface will be available at **http://localhost:5000**

### 📋 Features

#### 🏠 **Main Dashboard**
- **Overview**: System status and recent jobs
- **Quick Access**: Direct links to all components
- **Job Monitoring**: Real-time status updates
- **Modern Design**: Responsive, gradient-based UI

#### 📝 **Format Converter**
- **File Upload**: Drag-and-drop file upload
- **Format Selection**: 11+ input/output formats
- **Progress Tracking**: Real-time conversion progress
- **Download Options**: Direct download or view in browser

#### 🕷️ **Web Scraper**
- **URL Input**: Simple URL submission
- **Configuration**: Adjustable depth and page limits
- **JavaScript Support**: Modern website handling
- **Progress Monitoring**: Real-time scraping updates

#### 📚 **TEI Generator**
- **Academic Output**: TEI XML generation
- **Metadata Support**: Title, author, language
- **Validation**: XML structure validation
- **Download Options**: Standard TEI format

#### 🎨 **UML Generator**
- **Multiple Formats**: PlantUML, Mermaid, Graphviz
- **Structure Analysis**: Automatic content hierarchy
- **Visual Output**: Downloadable diagram files
- **Format Selection**: Choose preferred UML format

#### 🔄 **Full Pipeline**
- **End-to-End**: Complete processing workflow
- **Multi-Source**: Websites or local files
- **Batch Output**: Multiple format generation
- **Comprehensive**: All-in-one processing

### 💡 Usage Examples

#### Format Conversion via Web Interface
1. Navigate to **http://localhost:5000/convert**
2. Upload your document (Markdown, HTML, LaTeX, etc.)
3. Select output format (HTML, PDF, EPUB, etc.)
4. Click "Convert Document"
5. Monitor progress in real-time
6. Download or view results

#### Web Scraping via Web Interface
1. Navigate to **http://localhost:5000/scrape**
2. Enter website URL
3. Configure options (max pages, depth)
4. Start scraping
5. Monitor progress
6. Download scraped content

#### TEI Generation via Web Interface
1. Navigate to **http://localhost:5000/tei**
2. Upload source document
3. Enter metadata (title, author)
4. Generate TEI XML
5. Validate and download

### 🔧 Technical Architecture

#### **Backend Technology**
- **Flask**: Lightweight Python web framework
- **Threading**: Background job processing
- **File Management**: Secure temporary file handling
- **Status Tracking**: Real-time job monitoring

#### **Frontend Technology**
- **HTML5**: Modern semantic markup
- **CSS3**: Responsive design with gradients
- **JavaScript**: Dynamic updates and AJAX
- **Progress Indicators**: Real-time status updates

#### **Job Processing**
- **Queue System**: Background job management
- **Status Tracking**: Job state monitoring
- **File Handling**: Secure upload/download
- **Error Handling**: Graceful failure recovery

### 📊 Job Status System

#### **Job States**
- **Pending**: Waiting to start
- **Running**: Currently processing
- **Completed**: Successfully finished
- **Failed**: Error encountered

#### **Progress Tracking**
- **Percentage**: 0-100% progress indicator
- **Real-time Updates**: Automatic status refresh
- **Duration Tracking**: Processing time measurement
- **Error Reporting**: Detailed error messages

#### **File Management**
- **Upload**: Secure file upload with validation
- **Processing**: Temporary file management
- **Download**: Direct file access
- **Cleanup**: Automatic file cleanup

### 🎨 UI/UX Features

#### **Responsive Design**
- **Mobile Friendly**: Works on all devices
- **Touch Interface**: Optimized for mobile use
- **Adaptive Layout**: Flexible grid system
- **Progressive Enhancement**: Graceful degradation

#### **Visual Design**
- **Modern Gradients**: Professional color schemes
- **Card-Based Layout**: Component organization
- **Smooth Animations**: CSS transitions
- **Interactive Elements**: Hover effects and feedback

#### **User Experience**
- **Intuitive Navigation**: Clear menu structure
- **Progress Feedback**: Real-time status updates
- **Error Messages**: Clear error reporting
- **Success Confirmation**: Completion notifications

### 🔄 API Endpoints

#### **Main Endpoints**
```
GET  /                    # Main dashboard
GET  /convert            # Format converter page
POST /convert            # Process conversion
GET  /scrape             # Web scraper page  
POST /scrape             # Start scraping
GET  /tei                # TEI generator page
POST /tei                # Generate TEI
GET  /uml                # UML generator page
POST /uml                # Generate UML
GET  /pipeline           # Full pipeline page
POST /pipeline           # Run full pipeline
```

#### **Status Endpoints**
```
GET  /status/<job_id>    # Get job status
GET  /download/<job_id>/<filename>  # Download file
GET  /view/<job_id>/<filename>       # View file
```

#### **Job Management**
```
POST /api/jobs          # Create new job
GET  /api/jobs          # List all jobs
GET  /api/jobs/<id>     # Get specific job
DELETE /api/jobs/<id>   # Delete job
```

### 🔒 Security Features

#### **File Upload Security**
- **File Type Validation**: Allowed file types only
- **Size Limits**: Maximum file size restrictions
- **Path Validation**: Secure file path handling
- **Sanitization**: File name sanitization

#### **Job Security**
- **Isolated Processing**: Separate job directories
- **Timeout Protection**: Job execution limits
- **Resource Limits**: Memory and CPU constraints
- **Cleanup Automation**: Automatic file cleanup

#### **Web Security**
- **CSRF Protection**: Cross-site request forgery prevention
- **XSS Prevention**: Cross-site scripting protection
- **Secure Headers**: Security header configuration
- **Input Validation**: Request parameter validation

### 🚀 Deployment Options

#### **Development Mode**
```bash
# Local development
python3 web_interface.py

# With debug mode
python3 web_interface.py --debug
```

#### **Production Mode**
```bash
# Production deployment
export FLASK_ENV=production
export FLASK_DEBUG=0
python3 web_interface.py

# With Gunicorn
pip install gunicorn
gunicorn -w 4 -b 0.0.0.0:5000 web_interface:app
```

#### **Docker Deployment**
```dockerfile
FROM python:3.9-slim
COPY . /app
WORKDIR /app
RUN pip install -r web_requirements.txt
EXPOSE 5000
CMD ["python3", "web_interface.py"]
```

### 📈 Performance Considerations

#### **Optimization Features**
- **Lazy Loading**: On-demand resource loading
- **Caching**: Static file caching
- **Compression**: Response compression
- **Minification**: CSS/JS optimization

#### **Scalability**
- **Horizontal Scaling**: Multiple worker processes
- **Load Balancing**: Distribution of requests
- **Database Integration**: Persistent job storage
- **CDN Support**: Static asset delivery

### 🐛 Troubleshooting

#### **Common Issues**
1. **Flask Installation**: Ensure web requirements installed
2. **Port Conflicts**: Check port 5000 availability
3. **File Permissions**: Verify write permissions for web_jobs
4. **Component Import**: Check script imports

#### **Debug Mode**
```bash
# Enable debug mode
export FLASK_DEBUG=1
python3 web_interface.py

# Check logs
tail -f web_jobs/*.log
```

#### **Performance Issues**
- **Memory**: Monitor memory usage with large files
- **Timeout**: Adjust job timeout settings
- **Concurrency**: Increase worker processes
- **Storage**: Ensure sufficient disk space

### 🔄 Integration with CLI Tools

The web interface provides the same functionality as the CLI tools:

| Web Feature | CLI Equivalent |
|-------------|-----------------|
| Format Converter | `./convert.sh` |
| Web Scraper | `./scrape.sh` |
| TEI Generator | `./tei.sh` |
| UML Generator | `./uml.sh` |
| Full Pipeline | `./pipeline.sh` |

### 📚 Advanced Usage

#### **Custom Templates**
- **Template Directory**: `web_templates/`
- **Jinja2 Templates**: Dynamic content rendering
- **CSS Customization**: Modify web_templates/styles.css
- **JavaScript Extensions**: Add custom functionality

#### **Plugin System**
- **Component Plugins**: Extend with new processors
- **Format Plugins**: Add new format support
- **Theme System**: Custom UI themes
- **API Integration**: External service connections

---

**🌐 The web interface provides a user-friendly gateway to the powerful Integral Philosophy Publishing System, making academic content processing accessible to users of all technical levels!**