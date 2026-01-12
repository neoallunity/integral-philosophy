# ============================================
# Makefile для журнала "Интегральная философия"
# ============================================

# --------------------------------------------
# КОНФИГУРАЦИЯ
# --------------------------------------------

# Основной файл
MAIN = main

# Движок компиляции
LATEXMK = latexmk
BIBER = biber
MAKEGLOSSARIES = makeglossaries
MAKEINDEX = makeindex

# Директории
CHAPTERS_DIR = chapters
CFG_DIR = cfg
OUTPUT_DIR = .
BUILD_DIR = tmp

# Исходные файлы
MAIN_TEX = $(MAIN).tex
PREAMBLE = preamble.tex
BIB_FILE = references.bib

# Конфигурационные модули
CFG_FILES = $(wildcard $(CFG_DIR)/*.tex)

# Главы
CHAPTER_FILES = $(wildcard $(CHAPTERS_DIR)/*.tex)

# Финальный PDF
PDF = $(BUILD_DIR)/$(MAIN).pdf

# Временные файлы для очистки
TEMP_EXTS = aux log toc out fdb_latexmk fls synctex.gz bbl bcf blg run.xml \
            glo gls glg acn acr alg slg idx ind ilg syi syg lof lot xdv \
            nav snm vrb

# Цвета для вывода (опционально)
NO_COLOR = \033[0m
GREEN = \033[0;32m
YELLOW = \033[0;33m
RED = \033[0;31m
BLUE = \033[0;34m

# --------------------------------------------
# ОСНОВНЫЕ ЦЕЛИ
# --------------------------------------------

.PHONY: all build clean distclean watch help install-deps validate

# Цель по умолчанию
all: build

# Сборка PDF
build: $(PDF)
	@echo -e "$(GREEN)✓ Сборка завершена: $(PDF)$(NO_COLOR)"

# Синоним для build
pdf: build
	@echo -e "$(GREEN)✓ PDF создан: $(PDF)$(NO_COLOR)"

# Полная пересборка
rebuild: clean build

# Непрерывная компиляция (watch mode)
watch:
	@echo -e "$(BLUE)▶ Запуск непрерывной компиляции с Latexmk...$(NO_COLOR)"
	@$(LATEXMK) -pvc $(MAIN_TEX)

# --------------------------------------------
# ПРАВИЛА СБОРКИ
# --------------------------------------------

# Основное правило: PDF зависит от всех исходников
$(PDF): $(MAIN_TEX) $(PREAMBLE) $(CFG_FILES) $(CHAPTER_FILES) $(BIB_FILE)
	@echo -e "$(BLUE)▶ Компиляция $(MAIN_TEX) с использованием Latexmk...$(NO_COLOR)"
	@$(LATEXMK) $(MAIN_TEX)

# Быстрая компиляция (без полной сборки)
quick:
	@echo -e "$(YELLOW)⚡ Быстрая компиляция (без библиографии/индексов) с LuaLaTeX...$(NO_COLOR)"
	@$(LATEX) $(MAIN_TEX)

# --------------------------------------------
# ОЧИСТКА
# --------------------------------------------

# Стандартная очистка (сохраняет PDF)
clean:
	@echo -e "$(YELLOW)🧹 Очистка временных файлов с помощью latexmk...$(NO_COLOR)"
	@$(LATEXMK) -c
	@echo -e "$(GREEN)✓ Очистка завершена$(NO_COLOR)"

# Полная очистка (удаляет PDF)
distclean:
	@echo -e "$(RED)🗑️  Полная очистка (включая PDF) с помощью latexmk...$(NO_COLOR)"
	@$(LATEXMK) -C
	@echo -e "$(GREEN)✓ Полная очистка завершена$(NO_COLOR)"

# Очистка кэша latexmk
clean-cache:
	@echo -e "$(YELLOW)🗑️  Очистка кэша latexmk...$(NO_COLOR)"
	@rm -f *.fdb_latexmk *.fls

# --------------------------------------------
# ВАЛИДАЦИЯ И ПРОВЕРКИ
# --------------------------------------------

# Проверка синтаксиса LaTeX
validate:
	@echo -e "$(BLUE)🔍 Проверка синтаксиса с LuaLaTeX...$(NO_COLOR)"
	@$(LATEX) $(MAIN_TEX) > /dev/null
	@echo -e "$(GREEN)✓ Синтаксис корректен$(NO_COLOR)"

# Проверка наличия undefined references
check-refs: build
	@echo -e "$(BLUE)🔍 Проверка ссылок...$(NO_COLOR)"
	@if grep -q "LaTeX Warning: Reference" $(MAIN).log; then \
		echo -e "$(RED)✗ Найдены неопределённые ссылки:$(NO_COLOR)"; \
		grep "LaTeX Warning: Reference" $(MAIN).log; \
		exit 1; \
	else \
		echo -e "$(GREEN)✓ Все ссылки определены$(NO_COLOR)"; \
	fi

# Проверка наличия undefined citations
check-cites: build
	@echo -e "$(BLUE)🔍 Проверка цитирований...$(NO_COLOR)"
	@if grep -q "LaTeX Warning: Citation" $(MAIN).log; then \
		echo -e "$(RED)✗ Найдены неопределённые цитирования:$(NO_COLOR)"; \
		grep "LaTeX Warning: Citation" $(MAIN).log; \
		exit 1; \
	else \
		echo -e "$(GREEN)✓ Все цитирования определены$(NO_COLOR)"; \
	fi

# Полная проверка
check: check-refs check-cites
	@echo -e "$(GREEN)✓ Все проверки пройдены$(NO_COLOR)"

# --------------------------------------------
# СТАТИСТИКА И ИНФОРМАЦИЯ
# --------------------------------------------

# Подсчёт страниц
count-pages: build
	@echo -e "$(BLUE)📄 Количество страниц:$(NO_COLOR)"
	@pdfinfo $(PDF) 2>/dev/null | grep "Pages:" || echo -e "Не удалось определить"

# Подсчёт слов (приблизительно)
count-words:
	@echo -e "$(BLUE)📝 Подсчёт слов...$(NO_COLOR)"
	@detex $(CHAPTER_FILES) 2>/dev/null | wc -w || echo -e "Установите detex для подсчёта слов"

# Статистика проекта
stats: count-pages count-words
	@echo -e "$(BLUE)📊 Статистика проекта:$(NO_COLOR)"
	@echo -e "  Глав:        $$(ls -1 $(CHAPTERS_DIR)/*.tex 2>/dev/null | wc -l)"
	@echo -e "  Конфигураций: $$(ls -1 $(CFG_DIR)/*.tex 2>/dev/null | wc -l)"
	@echo -e "  Размер PDF:  $$(du -h $(PDF) 2>/dev/null | cut -f1 || echo -e 'N/A')"

# Информация о последней сборке
info:
	@echo -e "$(BLUE)ℹ️  Информация о проекте:$(NO_COLOR)"
	@echo -e "  Основной файл: $(MAIN_TEX)"
	@echo -e "  Движок:        $(LATEX)"
	@echo -e "  PDF:           $(PDF)"
	@echo -e "  Последняя сборка: $$(stat -f '%Sm' $(PDF) 2>/dev/null || stat -c '%y' $(PDF) 2>/dev/null || echo -e 'N/A')"

# --------------------------------------------
# ПРОСМОТР И ОТКРЫТИЕ
# --------------------------------------------

# Открыть PDF в системном просмотрщике
view: build
	@echo -e "$(BLUE)👁️  Открытие $(PDF)...$(NO_COLOR)"
	@if [ "$(shell uname)" = "Darwin" ]; then \
		open $(PDF); \
	elif [ "$(shell uname)" = "Linux" ]; then \
		xdg-open $(PDF) 2>/dev/null || evince $(PDF) 2>/dev/null || okular $(PDF) 2>/dev/null; \
	else \
		start $(PDF); \
	fi

# Открыть в конкретной программе
view-okular: build
	@okular $(PDF) &

view-evince: build
	@evince $(PDF) &

view-zathura: build
	@zathura $(PDF) &

# --------------------------------------------
# УСТАНОВКА ЗАВИСИМОСТЕЙ
# --------------------------------------------

# Проверка наличия необходимых инструментов
check-deps:
	@echo -e "$(BLUE)🔍 Проверка зависимостей...$(NO_COLOR)"
	@command -v /opt/texlive/2025/bin/x86_64-linux/lualatex >/dev/null 2>&1 || { echo -e "$(RED)✗ lualatex не установлен по указанному пути (/opt/texlive/2025/bin/x86_64-linux/lualatex)$(NO_COLOR)"; exit 1; }
	@command -v /opt/texlive/2025/bin/x86_64-linux/latexmk >/dev/null 2>&1 || { echo -e "$(RED)✗ latexmk не установлен по указанному пути (/opt/texlive/2025/bin/x86_64-linux/latexmk)$(NO_COLOR)"; exit 1; }
	@command -v biber >/dev/null 2>&1 || { echo -e "$(RED)✗ biber не установлен (или не в PATH)$(NO_COLOR)"; exit 1; }
	@echo -e "$(GREEN)✓ Все зависимости установлены (TeX Live 2025)$(NO_COLOR)"

# Инструкции по установке (для Linux/Ubuntu)
install-deps-ubuntu:
	@echo -e "$(BLUE)📦 Установка зависимостей для Ubuntu/Debian...$(NO_COLOR)"
	sudo apt-get update
	sudo apt-get install -y texlive-full texlive-luatex latexmk biber

# Инструкции по установке (для macOS)
install-deps-macos:
	@echo -e "$(BLUE)📦 Установка зависимостей для macOS...$(NO_COLOR)"
	@echo -e "Установите MacTeX: https://www.tug.org/mactex/"
	@echo -e "Или через Homebrew:"
	@echo -e "  brew install --cask mactex"

# --------------------------------------------
# АРХИВИРОВАНИЕ И РЕЗЕРВНОЕ КОПИРОВАНИЕ
# --------------------------------------------

# Создать архив проекта
archive:
	@echo -e "$(BLUE)📦 Создание архива проекта...$(NO_COLOR)"
	@tar -czf $(MAIN)_$(shell date +%Y%m%d_%H%M%S).tar.gz \
		$(MAIN_TEX) $(PREAMBLE) $(BIB_FILE) \
		$(CFG_DIR) $(CHAPTERS_DIR) \
		Makefile latexmkrc README.md 2>/dev/null || true
	@echo -e "$(GREEN)✓ Архив создан$(NO_COLOR)"

# Создать архив с PDF
archive-with-pdf: build
	@echo -e "$(BLUE)📦 Создание архива с PDF...$(NO_COLOR)"
	@tar -czf $(MAIN)_with_pdf_$(shell date +%Y%m%d_%H%M%S).tar.gz \
		$(MAIN_TEX) $(PREAMBLE) $(BIB_FILE) $(PDF) \
		$(CFG_DIR) $(CHAPTERS_DIR) \
		Makefile latexmkrc README.md 2>/dev/null || true
	@echo -e "$(GREEN)✓ Архив с PDF создан$(NO_COLOR)"

# Резервная копия
backup: archive

# --------------------------------------------
# ВЕРСИОНИРОВАНИЕ (GIT)
# --------------------------------------------

# Коммит изменений
commit:
	@echo -e "$(BLUE)📝 Коммит изменений...$(NO_COLOR)"
	@git add -A
	@git status
	@read -p "Сообщение коммита: " msg; \
	git commit -m "$$msg"

# Тег версии
tag:
	@echo -e "$(BLUE)🏷️  Создание тега версии...$(NO_COLOR)"
	@read -p "Номер версии (например, v1.0): " ver; \
	git tag -a $$ver -m "Release $$ver"
	@echo -e "$(GREEN)✓ Тег создан. Не забудьте: git push origin --tags$(NO_COLOR)"

# --------------------------------------------
# РАЗРАБОТКА И ОТЛАДКА
# --------------------------------------------

# Показать warnings из лога
warnings: build
	@echo -e "$(YELLOW)⚠️  Предупреждения из лога:$(NO_COLOR)"
	@grep -i "warning" $(BUILD_DIR)/$(MAIN).log || echo -e "Предупреждений не найдено"

# Показать errors из лога
errors: build
	@echo -e "$(RED)❌ Ошибки из лога:$(NO_COLOR)"
	@grep -i "error" $(BUILD_DIR)/$(MAIN).log || echo -e "Ошибок не найдено"

# Показать overfull/underfull boxes
boxes: build
	@echo -e "$(YELLOW)📦 Проблемы с боксами:$(NO_COLOR)"
	@grep -E "(Overfull|Underfull)" $(BUILD_DIR)/$(MAIN).log || echo -e "Проблем не найдено"

# Полный отчёт о проблемах
report: warnings errors boxes

# Режим черновика (быстрая компиляция)
draft:
	@echo -e "$(YELLOW)📄 Компиляция в режиме черновика ...$(NO_COLOR)"
	@$(LATEX) --chatter=minimal --only-cached $(MAIN_TEX)

# --------------------------------------------
# СПЕЦИФИЧНЫЕ ДЛЯ ЖУРНАЛА
# --------------------------------------------

# Обновить метаданные выпуска
update-metadata:
	@echo -e "$(BLUE)📝 Обновление метаданных выпуска...$(NO_COLOR)"
	@read -p "Номер выпуска: " issue; \
	read -p "Год: " year; \
	sed -i.bak "s/\\\\newcommand{\\\\journalissue}{[0-9]*}/\\\\newcommand{\\\\journalissue}{$$issue}/" $(CFG_DIR)/cfg-metadata.tex; \
	sed -i.bak "s/\\\\newcommand{\\\\journalyear}{[0-9]*}/\\\\newcommand{\\\\journalyear}{$$year}/" $(CFG_DIR)/cfg-metadata.tex; \
	rm -f $(CFG_DIR)/cfg-metadata.tex.bak
	@echo -e "$(GREEN)✓ Метаданные обновлены$(NO_COLOR)"

# Создать новую статью из шаблона
new-article:
	@echo -e "$(BLUE)📄 Создание новой статьи...$(NO_COLOR)"
	@read -p "Фамилия автора (латиница): " author; \
	cp templates/article-template.tex $(CHAPTERS_DIR)/article-$$author.tex
	@echo -e "$(GREEN)✓ Создан файл: $(CHAPTERS_DIR)/article-$$author.tex$(NO_COLOR)"

# --------------------------------------------
# ТЕСТИРОВАНИЕ
# --------------------------------------------

# Тест компиляции без сохранения
test:
	@echo -e "$(BLUE)🧪 Тестовая компиляция...$(NO_COLOR)"
	@$(LATEX) -interaction=nonstopmode -draftmode $(MAIN_TEX) > /dev/null 2>&1 && \
		echo -e "$(GREEN)✓ Тест пройден$(NO_COLOR)" || \
		{ echo -e "$(RED)✗ Тест не пройден$(NO_COLOR)"; exit 1; }

# Continuous Integration check
ci: check-deps test check
	@echo -e "$(GREEN)✓ CI проверка завершена успешно$(NO_COLOR)"

# --------------------------------------------
# СПРАВКА
# --------------------------------------------

# Показать помощь
help:
	@echo -e "$(BLUE)╔════════════════════════════════════════════════════════════╗$(NO_COLOR)"
	@echo -e "$(BLUE)║  Makefile для журнала «Интегральная философия»             ║$(NO_COLOR)"
	@echo -e "$(BLUE)╚════════════════════════════════════════════════════════════╝$(NO_COLOR)"
	@echo -e ""
	@echo -e "$(GREEN)ОСНОВНЫЕ КОМАНДЫ:$(NO_COLOR)"
	@echo -e "  make              - Собрать PDF (по умолчанию)"
	@echo -e "  make build        - Собрать PDF"
	@echo -e "  make rebuild      - Полная пересборка"
	@echo -e "  make clean        - Очистить временные файлы"
	@echo -e "  make distclean    - Полная очистка (включая PDF)"
	@echo -e "  make watch        - Непрерывная компиляция"
	@echo -e ""
	@echo -e "$(GREEN)ПРОСМОТР:$(NO_COLOR)"
	@echo -e "  make view         - Открыть PDF в системном просмотрщике"
	@echo -e "  make view-okular  - Открыть в Okular"
	@echo -e "  make view-evince  - Открыть в Evince"
	@echo -e ""
	@echo -e "$(GREEN)ПРОВЕРКА:$(NO_COLOR)"
	@echo -e "  make check        - Проверить ссылки и цитирования"
	@echo -e "  make check-refs   - Проверить ссылки"
	@echo -e "  make check-cites  - Проверить цитирования"
	@echo -e "  make validate     - Проверить синтаксис"
	@echo -e ""
	@echo -e "$(GREEN)СТАТИСТИКА:$(NO_COLOR)"
	@echo -e "  make stats        - Показать статистику проекта"
	@echo -e "  make count-pages  - Подсчитать страницы"
	@echo -e "  make count-words  - Подсчитать слова"
	@echo -e "  make info         - Информация о проекте"
	@echo -e ""
	@echo -e "$(GREEN)ОТЛАДКА:$(NO_COLOR)"
	@echo -e "  make warnings     - Показать предупреждения"
	@echo -e "  make errors       - Показать ошибки"
	@echo -e "  make boxes        - Показать проблемы с боксами"
	@echo -e "  make report       - Полный отчёт о проблемах"
	@echo -e ""
	@echo -e "$(GREEN)АРХИВИРОВАНИЕ:$(NO_COLOR)"
	@echo -e "  make archive      - Создать архив проекта"
	@echo -e "  make backup       - Резервная копия"
	@echo -e ""
	@echo -e "$(GREEN)ЗАВИСИМОСТИ:$(NO_COLOR)"
	@echo -e "  make check-deps   - Проверить зависимости"
	@echo -e ""
	@echo -e "$(GREEN)ЖУРНАЛ:$(NO_COLOR)"
	@echo -e "  make update-metadata - Обновить номер выпуска/год"
	@echo -e "  make new-article     - Создать новую статью из шаблона"
	@echo -e ""
	@echo -e "$(GREEN)МНОГОФОРМАТНАЯ ПУБЛИКАЦИЯ:$(NO_COLOR)"
	@echo -e "  make tei             - Конвертировать Markdown в TEI XML"
	@echo -e "  make validate        - Валидировать TEI XML"
	@echo -e "  make html            - Создать HTML версию"
	@echo -e "  make epub            - Создать EPUB версию"
	@echo -e "  make docx            - Создать DOCX версию"
	@echo -e "  make all-formats     - Создать все форматы"
	@echo -e "  make view-html       - Открыть HTML в браузере"
	@echo -e "  make tei-stats       - Статистика многоформатной публикации"
	@echo -e ""
	@echo -e "$(YELLOW)Документация: README.md$(NO_COLOR)"
	@echo -e "$(YELLOW)Поддержка: http://allunity.ru$(NO_COLOR)"

# Алиас для help
h: help

# --------------------------------------------
# .PHONY TARGETS
# --------------------------------------------

.PHONY: all build rebuild clean distclean clean-cache watch \
        validate check check-refs check-cites \
        count-pages count-words stats info \
        view view-okular view-evince view-zathura \
        check-deps install-deps-ubuntu install-deps-macos \
        archive archive-with-pdf backup \
        commit tag \
        warnings errors boxes report draft \
        update-metadata new-article \
        test ci help h quick

# ============================================
# МНОГОФОРМАТНАЯ ПУБЛИКАЦИЯ (TEI)
# ============================================

# Инструменты для TEI и многоформатной публикации
PANDOC = pandoc
SAXON = saxon-he
XMLLINT = xmllint

# Исходные файлы и директории
SRC_MD := $(wildcard src/*.md)
TEI_XML := tei/document.xml
BUILD_DIR := build

# Директории для различных форматов
HTML_DIR := $(BUILD_DIR)/html
LATEX_DIR := $(BUILD_DIR)/latex
EPUB_DIR := $(BUILD_DIR)/epub
DOCX_DIR := $(BUILD_DIR)/docx

# XSLT стилевые таблицы
XSLT_DIR := xslt
TEI2HTML_XSL := $(XSLT_DIR)/tei2html.xsl
TEI2LATEX_XSL := $(XSLT_DIR)/tei2latex.xsl
TEI2EPUB_XSL := $(XSLT_DIR)/tei2epub.xsl

# RelaxNG схема для валидации
TEI_SCHEMA := schema/tei-site.rng

# --------------------------------------------
# ЦЕЛИ МНОГОФОРМАТНОЙ ПУБЛИКАЦИИ
# --------------------------------------------

# Все форматы
all-formats: html pdf-from-tei epub docx
	@echo -e "$(GREEN)✓ Все форматы созданы успешно$(NO_COLOR)"

# Markdown + TeX -> TEI
tei: $(TEI_XML)

$(TEI_XML): $(SRC_MD) | tei-dir check-tei-deps
	@echo -e "$(BLUE)📝 Конвертация Markdown в TEI XML...$(NO_COLOR)"
	@if [ -z "$(SRC_MD)" ]; then \
		echo -e "$(RED)✗ Не найдены исходные Markdown файлы в src/$(NO_COLOR)"; \
		exit 1; \
	fi
	@$(PANDOC) $(SRC_MD) \
		--from=markdown+tex_math_dollars \
		--to=tei \
		--output=$(TEI_XML) \
		|| { echo -e "$(RED)✗ Ошибка конвертации Markdown в TEI XML$(NO_COLOR)"; exit 1; }
	@if [ ! -f "$(TEI_XML)" ]; then \
		echo -e "$(RED)✗ TEI XML файл не был создан$(NO_COLOR)"; \
		exit 1; \
	fi
	@if [ ! -s "$(TEI_XML)" ]; then \
		echo -e "$(RED)✗ TEI XML файл пуст$(NO_COLOR)"; \
		exit 1; \
	fi
	@echo -e "$(GREEN)✓ TEI XML создан: $(TEI_XML)$(NO_COLOR)"

# Валидация TEI
validate-tei: $(TEI_XML)
	@echo -e "$(BLUE)🔍 Валидация TEI XML против схемы...$(NO_COLOR)"
	@if [ ! -f "$(TEI_SCHEMA)" ]; then \
		echo -e "$(YELLOW)⚠️  Схема валидации $(TEI_SCHEMA) не найдена, пропускаю валидацию$(NO_COLOR)"; \
	else \
		$(XMLLINT) --noout --relaxng $(TEI_SCHEMA) $(TEI_XML) \
			|| { echo -e "$(RED)✗ TEI XML не прошел валидацию против схемы$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ TEI XML валиден$(NO_COLOR)"; \
	fi

# TEI -> HTML
html: validate-tei | html-dir
	@echo -e "$(BLUE)🌐 Генерация HTML из TEI...$(NO_COLOR)"
	@if [ ! -f "$(TEI2HTML_XSL)" ]; then \
		echo -e "$(RED)✗ XSLT таблица $(TEI2HTML_XSL) не найдена$(NO_COLOR)"; \
		exit 1; \
	fi
	@$(SAXON) -s:$(TEI_XML) \
		-xsl:$(TEI2HTML_XSL) \
		-o:$(HTML_DIR)/index.html \
		|| { echo -e "$(RED)✗ Ошибка генерации HTML из TEI$(NO_COLOR)"; exit 1; }
	@if [ ! -f "$(HTML_DIR)/index.html" ]; then \
		echo -e "$(RED)✗ HTML файл не был создан$(NO_COLOR)"; \
		exit 1; \
	fi
	@cp build/html/journal.css $(HTML_DIR)/ 2>/dev/null || echo -e "$(YELLOW)⚠️  CSS файл не найден$(NO_COLOR)"
	@cp build/html/journal.js $(HTML_DIR)/ 2>/dev/null || echo -e "$(YELLOW)⚠️  JS файл не найден$(NO_COLOR)"
	@echo -e "$(GREEN)✓ HTML создан: $(HTML_DIR)/index.html$(NO_COLOR)"

# TEI -> LaTeX
latex-from-tei: validate-tei | latex-dir
	@echo -e "$(BLUE)📄 Генерация LaTeX из TEI...$(NO_COLOR)"
	@$(SAXON) -s:$(TEI_XML) \
		-xsl:$(TEI2LATEX_XSL) \
		-o:$(LATEX_DIR)/document.tex
	@echo -e "$(GREEN)✓ LaTeX создан: $(LATEX_DIR)/document.tex$(NO_COLOR)"

# LaTeX -> PDF (из TEI)
pdf-from-tei: latex-from-tei
	@echo -e "$(BLUE)📑 Компиляция PDF из TEI-LaTeX...$(NO_COLOR)"
	@cd $(LATEX_DIR) && $(LUALATEX) document.tex
	@cd $(LATEX_DIR) && $(LUALATEX) document.tex  # Повторная компиляция для ссылок
	@echo -e "$(GREEN)✓ PDF создан: $(LATEX_DIR)/document.pdf$(NO_COLOR)"

# TEI -> EPUB
epub: validate-tei | epub-dir
	@echo -e "$(BLUE)📚 Генерация EPUB из TEI...$(NO_COLOR)"
	@$(SAXON) -s:$(TEI_XML) \
		-xsl:$(TEI2EPUB_XSL) \
		-o:$(EPUB_DIR)/book.xhtml
	@cp build/epub/styles.css $(EPUB_DIR)/
	@echo -e "$(BLUE)📦 Создание EPUB архива...$(NO_COLOR)"
	@cd $(EPUB_DIR) && echo "application/epub+zip" > mimetype
	@cd $(EPUB_DIR) && mkdir -p META-INF
	@cd $(EPUB_DIR) && echo '<?xml version="1.0"?><container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container"><rootfiles><rootfile full-path="book.xhtml" media-type="application/xhtml+xml"/></rootfiles></container>' > META-INF/container.xml
	@cd $(EPUB_DIR) && zip -r book.epub mimetype META-INF book.xhtml styles.css
	@echo -e "$(GREEN)✓ EPUB создан: $(EPUB_DIR)/book.epub$(NO_COLOR)"

# TEI -> DOCX (через Pandoc)
docx: validate-tei | docx-dir
	@echo -e "$(BLUE)📄 Генерация DOCX из TEI...$(NO_COLOR)"
	@$(PANDOC) $(TEI_XML) -o $(DOCX_DIR)/book.docx
	@echo -e "$(GREEN)✓ DOCX создан: $(DOCX_DIR)/book.docx$(NO_COLOR)"

# --------------------------------------------
# СОЗДАНИЕ ДИРЕКТОРИЙ
# --------------------------------------------

tei-dir:
	@if [ ! -d "tei" ]; then \
		echo -e "$(BLUE)📁 Создание директории tei...$(NO_COLOR)"; \
		mkdir -p tei 2>/dev/null || { echo -e "$(RED)✗ Не удалось создать директорию tei$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ Директория tei создана$(NO_COLOR)"; \
	fi

html-dir:
	@if [ ! -d "$(HTML_DIR)" ]; then \
		echo -e "$(BLUE)📁 Создание директории $(HTML_DIR)...$(NO_COLOR)"; \
		mkdir -p $(HTML_DIR) 2>/dev/null || { echo -e "$(RED)✗ Не удалось создать директорию $(HTML_DIR)$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ Директория $(HTML_DIR) создана$(NO_COLOR)"; \
	fi

latex-dir:
	@if [ ! -d "$(LATEX_DIR)" ]; then \
		echo -e "$(BLUE)📁 Создание директории $(LATEX_DIR)...$(NO_COLOR)"; \
		mkdir -p $(LATEX_DIR) 2>/dev/null || { echo -e "$(RED)✗ Не удалось создать директорию $(LATEX_DIR)$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ Директория $(LATEX_DIR) создана$(NO_COLOR)"; \
	fi

epub-dir:
	@if [ ! -d "$(EPUB_DIR)" ]; then \
		echo -e "$(BLUE)📁 Создание директории $(EPUB_DIR)...$(NO_COLOR)"; \
		mkdir -p $(EPUB_DIR) 2>/dev/null || { echo -e "$(RED)✗ Не удалось создать директорию $(EPUB_DIR)$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ Директория $(EPUB_DIR) создана$(NO_COLOR)"; \
	fi

docx-dir:
	@if [ ! -d "$(DOCX_DIR)" ]; then \
		echo -e "$(BLUE)📁 Создание директории $(DOCX_DIR)...$(NO_COLOR)"; \
		mkdir -p $(DOCX_DIR) 2>/dev/null || { echo -e "$(RED)✗ Не удалось создать директорию $(DOCX_DIR)$(NO_COLOR)"; exit 1; }; \
		echo -e "$(GREEN)✓ Директория $(DOCX_DIR) создана$(NO_COLOR)"; \
	fi

build-dirs: tei-dir html-dir latex-dir epub-dir docx-dir

# --------------------------------------------
# ОЧИСТКА МНОГОФОРМАТНЫХ ФАЙЛОВ
# --------------------------------------------

clean-tei:
	@echo -e "$(YELLOW)🧹 Очистка TEI файлов...$(NO_COLOR)"
	@rm -rf tei/*.xml
	@echo -e "$(GREEN)✓ TEI файлы очищены$(NO_COLOR)"

clean-formats:
	@echo -e "$(YELLOW)🧹 Очистка всех форматов...$(NO_COLOR)"
	@rm -rf $(BUILD_DIR)
	@echo -e "$(GREEN)✓ Все форматы очищены$(NO_COLOR)"

clean-all: clean clean-tei clean-formats

# --------------------------------------------
# ПРОВЕРКА ЗАВИСИМОСТЕЙ ДЛЯ TEI
# --------------------------------------------

check-tei-deps:
	@echo -e "$(BLUE)🔍 Проверка зависимостей для TEI...$(NO_COLOR)"
	@command -v $(PANDOC) >/dev/null 2>&1 || { echo -e "$(RED)✗ pandoc не установлен$(NO_COLOR)"; echo -e "$(YELLOW)Установите: sudo apt-get install pandoc$(NO_COLOR)"; exit 1; }
	@command -v $(SAXON) >/dev/null 2>&1 || { echo -e "$(RED)✗ saxon-he не установлен$(NO_COLOR)"; echo -e "$(YELLOW)Установите: sudo apt-get install saxon-he$(NO_COLOR)"; exit 1; }
	@command -v $(XMLLINT) >/dev/null 2>&1 || { echo -e "$(RED)✗ xmllint не установлен$(NO_COLOR)"; echo -e "$(YELLOW)Установите: sudo apt-get install libxml2-utils$(NO_COLOR)"; exit 1; }
	@echo -e "$(BLUE)📋 Проверка версий...$(NO_COLOR)"
	@PANDOC_VERSION=$$($(PANDOC) --version | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+'); \
	if [ -z "$$PANDOC_VERSION" ]; then \
		echo -e "$(YELLOW)⚠️  Не удалось определить версию pandoc$(NO_COLOR)"; \
	else \
		echo -e "$(GREEN)✓ pandoc: $$PANDOC_VERSION$(NO_COLOR)"; \
	fi
	@SAXON_VERSION=$$($(SAXON) -version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+' | head -1); \
	if [ -z "$$SAXON_VERSION" ]; then \
		echo -e "$(YELLOW)⚠️  Не удалось определить версию saxon-he$(NO_COLOR)"; \
	else \
		echo -e "$(GREEN)✓ saxon-he: $$SAXON_VERSION$(NO_COLOR)"; \
	fi
	@XMLLINT_VERSION=$$($(XMLLINT) --version | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+'); \
	if [ -z "$$XMLLINT_VERSION" ]; then \
		echo -e "$(YELLOW)⚠️  Не удалось определить версию xmllint$(NO_COLOR)"; \
	else \
		echo -e "$(GREEN)✓ xmllint: $$XMLLINT_VERSION$(NO_COLOR)"; \
	fi
	@echo -e "$(GREEN)✓ Все зависимости для TEI установлены$(NO_COLOR)"

# --------------------------------------------
# ПРОСМОТР РЕЗУЛЬТАТОВ
# --------------------------------------------

view-html: html
	@echo -e "$(BLUE)👁️  Открытие HTML версии...$(NO_COLOR)"
	@if [ "$(shell uname)" = "Darwin" ]; then \
		open $(HTML_DIR)/index.html; \
	elif [ "$(shell uname)" = "Linux" ]; then \
		xdg-open $(HTML_DIR)/index.html 2>/dev/null || firefox $(HTML_DIR)/index.html 2>/dev/null; \
	fi

view-epub: epub
	@echo -e "$(BLUE)👁️  EPUB создан: $(EPUB_DIR)/book.epub$(NO_COLOR)"
	@echo -e "$(YELLOW)Используйте программу для чтения EPUB для просмотра$(NO_COLOR)"

# --------------------------------------------
# СТАТИСТИКА МНОГОФОРМАТНОЙ ПУБЛИКАЦИИ
# --------------------------------------------

tei-stats:
	@echo -e "$(BLUE)📊 Статистика TEI публикации:$(NO_COLOR)"
	@echo -e "  Исходных MD файлов: $(words $(SRC_MD))"
	@if [ -f "$(TEI_XML)" ]; then \
		echo -e "  Размер TEI XML: $(shell du -h $(TEI_XML) 2>/dev/null | cut -f1)"; \
	fi
	@if [ -f "$(HTML_DIR)/index.html" ]; then \
		echo -e "  Размер HTML: $(shell du -h $(HTML_DIR)/index.html 2>/dev/null | cut -f1)"; \
	fi
	@if [ -f "$(EPUB_DIR)/book.epub" ]; then \
		echo -e "  Размер EPUB: $(shell du -h $(EPUB_DIR)/book.epub 2>/dev/null | cut -f1)"; \
	fi
	@if [ -f "$(DOCX_DIR)/book.docx" ]; then \
		echo -e "  Размер DOCX: $(shell du -h $(DOCX_DIR)/book.docx 2>/dev/null | cut -f1)"; \
	fi

# --------------------------------------------
# ОБНОВЛЕНИЕ СПРАВКИ
# --------------------------------------------

.PHONY: tei validate-tei html latex-from-tei pdf-from-tei epub docx \
        all-formats clean-tei clean-formats clean-all \
        check-tei-deps view-html view-epub tei-stats \
        tei-dir html-dir latex-dir epub-dir docx-dir build-dirs