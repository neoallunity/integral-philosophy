# Комплексная Система Обработки Контента

## Архитектура

Система обеспечивает полную трансформацию контента от веб-скрапинга до множественных форматов вывода:

```
Готовый сайт (HTML)
        |
        v
   HTML Parser
        |
        v
   Site AST
        |
        v
      UML
        |
        v
     TEI XML   ← канон
        |
    +--> XSLT → HTML + SCSS + JS
    +--> XSLT → LaTeX → PDF
    +--> XSLT → EPUB
    +--> XSLT → DOCX
```

## Компоненты Системы

### 1. Веб-скрапер (`web_scraper.py`)
**Рекурсивный скрапер с поддержкой JavaScript**

- **Функционал**: Полное скачивание сайтов с обработкой JS
- **Технологии**: Selenium WebDriver
- **Возможности**:
  - Рекурсивное обход сайтов
  - Обработка динамического контента
  - Сохранение в MarkdownTeX формате
  - Генерация AST сайта
  - Извлечение метаданных

**Использование**:
```bash
python3 scripts/web_scraper.py https://example.com -o scraped_content -m 100
```

### 2. MarkdownTex Парсер (`markdowntex_parser.py`)
**Парсер CommonMark с расширениями LaTeX**

- **Функционал**: Преобразование Markdown+TeX в AST
- **Возможности**:
  - Полная поддержка CommonMark
  - Встроенный LaTeX (формулы, теоремы)
  - Сохранение структуры и ссылок
  - Генерация JSON AST
  - Извлечение метаданных

**Использование**:
```bash
python3 scripts/markdowntex_parser.py document.md -o document.ast.json
```

### 3. UML Трансформер (`ast_to_uml.py`)
**Визуализация структуры сайта**

- **Функционал**: Преобразование AST в UML диаграммы
- **Форматы**: PlantUML, Mermaid, Graphviz
- **Возможности**:
  - Классификация страниц по типам
  - Визуализация связей между страницами
  - Генерация иерархической структуры
  - Статистика сайта

**Использование**:
```bash
python3 scripts/ast_to_uml.py site_ast.json -o uml_output -f all
```

### 4. TEI Генератор (`tei_generator.py`)
**Каноническое хранение в TEI XML**

- **Функционал**: Преобразование в академический стандарт TEI
- **Возможности**:
  - Структурированное хранение контента
  - Поддержка научных публикаций
  - Метаданные и библиография
  - Stand-off разметка для связей

**Использование**:
```bash
python3 scripts/tei_generator.py site_ast.json -o site_document.xml
```

### 5. XSLT Трансформер (`xslt_transformer.py`)
**Множественные форматы вывода**

- **Функционал**: Преобразование TEI XML в различные форматы
- **Поддерживаемые форматы**: HTML, LaTeX, PDF, EPUB, DOCX
- **Технологии**: XSLT, Pandoc, LuaLaTeX

**Использование**:
```bash
python3 scripts/xslt_transformer.py site_document.xml -o output -f all
```

### 6. HTML↔TEI Конвертер (`html_tei_converter.py`)
**Двунаправленное преобразование с тестированием изоморфизма**

- **Функционал**: Конвертация HTML ↔ TEI
- **Валидация**: Тестирование сохранения структуры
- **Инструменты**: Tidy, Saxon, xmllint
- **Тесты**: Изоморфизм HTML → TEI → HTML

**Использование**:
```bash
python3 scripts/html_tei_converter.py document.html --batch
```

### 7. Мультиформатный Конвертер (`format_converter.py`)
**Преобразование между множественными форматами разметки**

- **Поддерживаемые форматы**:
  - Markdown
  - Org-mode
  - AsciiDoc
  - reStructuredText
  - Typst
  - HTML
  - LaTeX
  - TEI XML

**Использование**:
```bash
# Конвертация одного файла
python3 scripts/format_converter.py document.md -f html -o document.html

# Цепочка конвертаций
python3 scripts/format_converter.py document.md --chain org asciidoc rst

# Генерация матрицы конвертаций
python3 scripts/format_converter.py document.md --matrix
```

### 8. Master Pipeline (`content_pipeline.py`)
**Оркестрация полного процесса**

- **Функционал**: Полный пайплин от URL до готовых форматов
- **Стадии**:
  1. Веб-скрапинг
  2. Парсинг контента
  3. Генерация UML
  4. Создание TEI
  5. Трансформация форматов
  6. Валидация

**Использование**:
```bash
python3 scripts/content_pipeline.py https://example.com -o pipeline_output -p 50
```

## Зависимости

### Системные инструменты:
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y python3-pip selenium chromium-browser tidy saxon-xslt libxml2-utils
sudo apt-get install -y pandoc lualatex texlive-full
sudo apt-get install -y graphviz

# macOS
brew install selenium-server-standalone chromium tidy-html5 saxon libxml2
brew install pandoc lualatex graphviz
```

### Python пакеты:
```bash
pip install selenium lxml pyyaml
pip install beautifulsoup4 html2text
pip install pillow
```

## Рабочий Процесс

### 1. Подготовка
```bash
# Создание рабочей директории
mkdir content_pipeline && cd content_pipeline

# Клонирование репозитория
git clone https://github.com/your-repo/integral-philosophy-publishing .
```

### 2. Базовая конвертация сайта
```bash
# Запуск полного пайплайна
python3 scripts/content_pipeline.py https://philosophy-site.com -o output -p 100
```

### 3. Индивидуальное использование компонентов

#### Скрапинг сайта
```bash
python3 scripts/web_scraper.py https://example.com -o scraped_site
```

#### Создание UML диаграмм
```bash
python3 scripts/ast_to_uml.py scraped_site/site_ast.json -o uml_diagrams
```

#### Генерация TEI
```bash
python3 scripts/tei_generator.py scraped_site/site_ast.json -o canonical_tei.xml
```

#### Трансформация в форматы
```bash
python3 scripts/xslt_transformer.py canonical_tei.xml -o published_formats -f all
```

### 4. Валидация и тестирование

#### Тест изоморфизма
```bash
python3 scripts/html_tei_converter.py published_formats/document.html --batch
```

#### Сравнение форматов
```bash
python3 scripts/format_converter.py document.md --compare document.html document.tex
```

## Структура Выходных Данных

```
content_pipeline/
├── 01_scraped/           # Результаты скрапинга
│   ├── pages/           # Страницы в MarkdownTeX
│   ├── meta/            # Метаданные
│   └── site_ast.json    # AST сайта
├── 02_parsed/           # Распарсенный контент
├── 03_uml/             # UML диаграммы
│   ├── site_structure.puml
│   ├── site_structure.mmd
│   └── site_structure.dot
├── 04_tei/             # TEI XML документы
│   └── site_document.xml
├── 05_transformed/      # Финальные форматы
│   ├── document.html
│   ├── document.tex
│   ├── document.pdf
│   ├── document.epub
│   └── document.docx
├── 06_validation/       # Результаты валидации
└── reports/            # Отчеты пайплайна
```

## Конфигурация

### Настройка скрапера
```python
scraper_config = {
    'max_pages': 100,
    'delay': 1.0,
    'user_agent': 'CustomBot/1.0',
    'respect_robots_txt': True
}
```

### Настройка TEI
```python
tei_config = {
    'language': 'ru',
    'encoding': 'UTF-8',
    'include_bibliography': True,
    'preserve_formatting': True
}
```

### Настройка форматов вывода
```python
output_config = {
    'html': {'css_style': 'academic', 'responsive': True},
    'latex': {'engine': 'lualatex', 'font': 'Times New Roman'},
    'pdf': {'page_size': 'A4', 'margin': '2cm'},
    'epub': {'toc': True, 'embed_fonts': True}
}
```

## Интеграция с Инструментами

### CI/CD Pipeline
```yaml
# .github/workflows/publish.yml
name: Publish Content
on: [push]
jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Python
        uses: actions/setup-python@v2
        with:
          python-version: '3.9'
      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y pandoc lualatex tidy saxon-xslt
          pip install -r requirements.txt
      - name: Run pipeline
        run: |
          python3 scripts/content_pipeline.py $SITE_URL -o output
      - name: Upload artifacts
        uses: actions/upload-artifact@v2
        with:
          name: published-formats
          path: output/05_transformed/
```

### Docker контейнер
```dockerfile
FROM python:3.9-slim

# Системные зависимости
RUN apt-get update && apt-get install -y \
    pandoc lualatex tidy saxon-xslt libxml2-utils \
    chromium-driver && rm -rf /var/lib/apt/lists/*

# Python зависимости
COPY requirements.txt .
RUN pip install -r requirements.txt

# Код приложения
COPY . /app
WORKDIR /app

CMD ["python3", "scripts/content_pipeline.py"]
```

## Расширенные Возможности

### 1. Кастомные XSLT трансформации
```xml
<!-- custom_tei_to_html.xslt -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="tei:div[@type='chapter']">
    <section class="chapter">
      <xsl:apply-templates/>
    </section>
  </xsl:template>
</xsl:stylesheet>
```

### 2. Плагины для парсера
```python
class CustomMarkdownParser(MarkdownTeXParser):
    def parse_custom_element(self, text):
        # Кастомная логика парсинга
        pass
```

### 3. Расширение форматов
```python
# Добавление нового формата
SUPPORTED_FORMATS['custom'] = ['cust']
```

## Мониторинг и Отладка

### Логирование
```python
import logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('pipeline.log'),
        logging.StreamHandler()
    ]
)
```

### Метрики производительности
```python
# Отслеживание времени выполнения
import time
start_time = time.time()
# ... операция ...
duration = time.time() - start_time
logger.info(f"Operation completed in {duration:.2f} seconds")
```

## FAQ

### Q: Как добавить поддержку нового формата?
A: Расширьте `SUPPORTED_FORMATS` в `format_converter.py` и создайте необходимые XSLT трансформации.

### Q: Как настроить обработку специфических сайтов?
A: Измените параметры `WebScraper` класса: `max_pages`, `delay`, `user_agent`.

### Q: Как интегрировать с существующей CMS?
A: Используйте `tei_generator.py` для создания TEI XML, затем импортируйте в CMS через API.

### Q: Как обеспечить высокую производительность?
A: Используйте асинхронную обработку, кэширование и оптимальные настройки пайплайна.

## Контакты и Поддержка

- **Документация**: `docs/`
- **Примеры**: `examples/`
- **Тесты**: `tests/`
- **Issues**: GitHub Issues

## Лицензирование

Система распространяется под MIT лицензией. См. `LICENSE` файл для деталей.

---

**Система разработана для Integral Philosophy Publishing System**  
*Версия 1.0.0 | 2024*