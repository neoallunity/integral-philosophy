
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MAIN_TEX="$SCRIPT_DIR/main.tex"
MAIN_PDF="$SCRIPT_DIR/main.pdf"

help() {
echo "Скрипт сборки журнала 'Интегральная философия'"
echo ""
echo "Использование: $0 [команда]"
echo ""
echo "Команды:"
echo " (без команды) Полная сборка PDF"
echo " clean Очистка временных файлов"
echo " fast Быстрая сборка (без библиографии)"
echo " view Открыть PDF в просмотрщике"
echo " help Показать эту справку"
echo ""
}

build_full() {
echo "🔨 Начало полной сборки..."
cd "$SCRIPT_DIR"

echo "1️⃣  Первая компиляция LaTeX..."
pdflatex -interaction=nonstopmode "$MAIN_TEX" >/dev/null 2>&1 || {
    echo "❌ Ошибка компиляции LaTeX"
    exit 1
}

echo "2️⃣  Генерация библиографии..."
bibtex "$(basename "$MAIN_TEX" .tex)" >/dev/null 2>&1 || {
    echo "⚠️  Предупреждение: возможны проблемы с библиографией"
}

echo "3️⃣  Вторая компиляция..."
pdflatex -interaction=nonstopmode "$MAIN_TEX" >/dev/null 2>&1

echo "4️⃣  Третья компиляция (для корректных ссылок)..."
pdflatex -interaction=nonstopmode "$MAIN_TEX" >/dev/null 2>&1

if [ -f "$MAIN_PDF" ]; then
    echo "✅ Сборка завершена: $MAIN_PDF"
    echo "📄 Размер файла: $(du -h "$MAIN_PDF" | cut -f1)"
else
    echo "❌ Ошибка: PDF не создан"
    exit 1
fi

}

build_fast() {
echo "⚡ Быстрая сборка (без библиографии)..."
cd "$SCRIPT_DIR"
pdflatex -interaction=nonstopmode "$MAIN_TEX"

if [ -f "$MAIN_PDF" ]; then
    echo "✅ Быстрая сборка завершена: $MAIN_PDF"
fi

}

clean() {
  echo "🧹 Очистка временных файлов..."
  cd "$SCRIPT_DIR"
  rm -f *.aux *.bbl *.blg *.log .out .toc .lof .lot
  rm -f frontmatter/.aux articles/.aux backmatter/.aux
  rm -f _minted- *.pyg
  echo "✅ Очистка завершена"
}

view_pdf() {
if [ ! -f "$MAIN_PDF" ]; then
  echo "❌ PDF не найден. Сначала выполните сборку."
  exit 1
fi

echo "👁️  Открытие PDF..."
if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$MAIN_PDF"
elif command -v open >/dev/null 2>&1; then
    open "$MAIN_PDF"
else
    echo "Не найден просмотрщик PDF. Файл: $MAIN_PDF"
fi

}

case "$1" in
 "clean")
    clean
  ;;
  "fast")
    build_fast
  ;;
  "view")
    view_pdf
  ;;
  "help"|"-h"|"--help")
    help
  ;;
  "")
    build_full
  ;;
  *)
  echo "❌ Неизвестная команда: $1"
  echo ""
  help
  exit 1
  ;;
esac
