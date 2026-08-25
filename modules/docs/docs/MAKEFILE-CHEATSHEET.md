# Makefile: Шпаргалка

## Основные команды (90% использования)

```bash
make              # Собрать PDF
make clean        # Очистить мусор
make watch        # Автокомпиляция
make view         # Открыть PDF
make help         # Все команды
```

---

## Сборка

```bash
make              # = make build
make rebuild      # Полная пересборка
make quick        # Быстро (без библиографии)
make draft        # Черновик
```

---

## Очистка

```bash
make clean        # Временные файлы (оставить PDF)
make distclean    # Всё включая PDF
make clean-cache  # Только кэш latexmk
```

---

## Проверка

```bash
make check        # Ссылки + цитирования
make check-refs   # Только ссылки
make check-cites  # Только цитирования
make validate     # Синтаксис
```

---

## Отладка

```bash
make report       # Полный отчёт
make warnings     # Предупреждения
make errors       # Ошибки
make boxes        # Переполнения строк
```

---

## Статистика

```bash
make stats        # Страницы + слова + размер
make count-pages  # Количество страниц
make count-words  # Количество слов
make info         # Информация о проекте
```

---

## Архивирование

```bash
make archive      # .tar.gz без PDF
make backup       # = make archive
```

---

## Журнал

```bash
make update-metadata  # Изменить выпуск/год
make new-article      # Новая статья из шаблона
```

---

## Типичные сценарии

### Ежедневная работа
```bash
make watch  # Запустить в отдельном терминале
# Редактировать файлы → автосохранение → авто-пересборка
```

### Перед публикацией
```bash
make rebuild && make check && make stats
```

### Устранение ошибок
```bash
make report  # Посмотреть все проблемы
# Исправить
make rebuild
```

### Новый выпуск
```bash
make update-metadata  # Номер + год
make distclean        # Очистить старое
make                  # Собрать новое
```

---

## Горячие клавиши (добавить в shell)

```bash
# ~/.bashrc или ~/.zshrc:
alias mb='make build'
alias mc='make clean'
alias mw='make watch'
alias mv='make view'
alias mr='make rebuild'
```

---

## Флаги Make

```bash
make -j4          # Параллельная сборка (4 процесса)
make -n           # Dry run (показать команды без выполнения)
make -s           # Тихий режим (без вывода команд)
make -k           # Продолжить при ошибках
```

---

## Проверка зависимостей

```bash
make check-deps  # Проверить наличие всех инструментов
```

Если что-то не установлено:
```bash
# Ubuntu/Debian:
sudo apt-get install texlive-full latexmk biber

# macOS:
brew install --cask mactex
```

---

## Интеграция с Git

```bash
# .gitignore уже содержит временные файлы LaTeX
# После работы:
make clean       # Очистить
git add -A       # Добавить изменения
git commit -m "Выпуск №15"
git tag v15.0    # Тег версии
```

---

## Если что-то сломалось

1. **Полная очистка и пересборка:**
   ```bash
   make distclean && make
   ```

2. **Проверить лог:**
   ```bash
   make report
   ```

3. **Вручную запустить latexmk:**
   ```bash
   latexmk -lualatex -interaction=nonstopmode main.tex
   ```

4. **Проверить зависимости:**
   ```bash
   make check-deps
   ```

---

## Настройка под себя

**Изменить движок:** Makefile, строка 15
```makefile
LATEX = lualatex  # → pdflatex, xelatex
```

**Добавить расширение для очистки:** Makefile, строка 33
```makefile
TEMP_EXTS = aux log ... myext
```

**Отключить цвета:** Makefile, строки 37-41
```makefile
NO_COLOR =
GREEN =
```

---

## Полезные ссылки

- `make help` — все команды
- `MAKEFILE-USAGE.md` — подробное руководство
- GNU Make Manual: https://www.gnu.org/software/make/manual/

---

**[Проверено]** Эта шпаргалка покрывает 95% типичных задач при работе с журналом.
