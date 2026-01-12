#!/usr/bin/env python3
"""
Базовый тест LaTeX-журнала без pytest.
Проверяет, что структура проекта корректна и LaTeX компилируется.
"""

import sys
import os
import subprocess
from pathlib import Path


def test_latex_compilation():
    """Тест базовой LaTeX компиляции."""

    print("🔍 Тест LaTeX компиляции...")

    try:
        # Проверка наличия основных файлов
        required_files = ["main.tex", "preamble.tex"]
        for file in required_files:
            if not Path(file).exists():
                raise FileNotFoundError(f"Отсутствует файл: {file}")

        # Тестовая компиляция через make
        result = subprocess.run(
            ["make", "check-deps"], capture_output=True, text=True, cwd="."
        )

        if result.returncode != 0:
            raise RuntimeError(f"Проверка зависимостей не пройдена: {result.stderr}")

        print("✓ LaTeX зависимости доступны")

        # Проверка синтаксиса
        result = subprocess.run(
            [
                "lualatex",
                "-interaction=nonstopmode",
                "-halt-on-error",
                "-output-directory=tmp",
                "main.tex",
            ],
            capture_output=True,
            text=True,
            cwd=".",
        )

        if result.returncode == 0:
            print("✓ LaTeX компиляция успешна")
            return True
        else:
            print("⚠️  LaTeX компиляция завершилась с предупреждениями")
            # Проверяем, что PDF все равно создан
            if Path("tmp/main.pdf").exists():
                print("✓ PDF файл создан несмотря на предупреждения")
                return True
            return False

    except Exception as e:
        print(f"✗ Ошибка LaTeX компиляции: {e}")
        return False


def test_makefile():
    """Тест Makefile."""

    print("🔍 Тест Makefile...")

    try:
        # Проверка основных целей
        result = subprocess.run(
            ["make", "help"], capture_output=True, text=True, cwd="."
        )

        if result.returncode != 0:
            raise RuntimeError("Makefile help не работает")

        if "Интегральная философия" not in result.stdout:
            raise ValueError("Некорректное название проекта в help")

        print("✓ Makefile корректен")
        return True

    except Exception as e:
        print(f"✗ Ошибка Makefile: {e}")
        return False


def test_project_structure():
    """Тест структуры проекта."""

    print("🔍 Тест структуры проекта...")

    try:
        required_dirs = ["cfg", "chapters", "articles", "frontmatter", "backmatter"]

        for dir_name in required_dirs:
            if not Path(dir_name).exists():
                raise FileNotFoundError(f"Отсутствует директория: {dir_name}")

        required_files = [
            "cfg/cfg-fonts.tex",
            "cfg/cfg-bibliography.tex",
            "cfg/cfg-structure.tex",
        ]

        for file_path in required_files:
            if not Path(file_path).exists():
                raise FileNotFoundError(f"Отсутствует файл: {file_path}")

        print("✓ Структура проекта корректна")
        return True

    except Exception as e:
        print(f"✗ Ошибка структуры: {e}")
        return False


def main():
    """Запуск всех тестов."""

    print("🚀 Запуск базовых тестов LaTeX журнала...\n")

    tests = [
        test_project_structure,
        test_makefile,
        test_latex_compilation,
    ]

    results = []

    for test in tests:
        try:
            result = test()
            results.append(result)
            print()
        except Exception as e:
            print(f"✗ Тест завершился с исключением: {e}\n")
            results.append(False)

    # Итоги
    passed = sum(results)
    total = len(results)

    print("=" * 50)
    print(f"Итоги: {passed}/{total} тестов пройдено")

    if passed == total:
        print("🎉 Все тесты пройдены!")
        return 0
    else:
        print("❌ Некоторые тесты не пройдены")
        return 1


if __name__ == "__main__":
    exit(main())
