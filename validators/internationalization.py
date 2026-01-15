#!/usr/bin/env python3
"""
Internationalization (i18n) Support for Integral Philosophy publishing system.
Implements comprehensive multi-language validation and localization support.
"""

import json
import re
from pathlib import Path
from typing import Dict, List, Any, Optional, Set, Union
from dataclasses import dataclass, field
import logging
from datetime import datetime
import locale
import gettext

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)

# Language configurations
SUPPORTED_LANGUAGES = {
    "en": {
        "name": "English",
        "code": "en",
        "direction": "ltr",
        "date_format": "%Y-%m-%d",
        "decimal_separator": ".",
        "thousands_separator": ",",
        "encoding": "utf-8",
    },
    "es": {
        "name": "Español",
        "code": "es",
        "direction": "ltr",
        "date_format": "%d/%m/%Y",
        "decimal_separator": ",",
        "thousands_separator": ".",
        "encoding": "utf-8",
    },
    "fr": {
        "name": "Français",
        "code": "fr",
        "direction": "ltr",
        "date_format": "%d/%m/%Y",
        "decimal_separator": ",",
        "thousands_separator": " ",
        "encoding": "utf-8",
    },
    "de": {
        "name": "Deutsch",
        "code": "de",
        "direction": "ltr",
        "date_format": "%d.%m.%Y",
        "decimal_separator": ",",
        "thousands_separator": ".",
        "encoding": "utf-8",
    },
    "ru": {
        "name": "Русский",
        "code": "ru",
        "direction": "ltr",
        "date_format": "%d.%m.%Y",
        "decimal_separator": ",",
        "thousands_separator": " ",
        "encoding": "utf-8",
    },
    "zh": {
        "name": "中文",
        "code": "zh",
        "direction": "ltr",
        "date_format": "%Y年%m月%d日",
        "decimal_separator": ".",
        "thousands_separator": ",",
        "encoding": "utf-8",
    },
    "ar": {
        "name": "العربية",
        "code": "ar",
        "direction": "rtl",
        "date_format": "%Y/%m/%d",
        "decimal_separator": ".",
        "thousands_separator": ",",
        "encoding": "utf-8",
    },
}

# Validation message translations
VALIDATION_MESSAGES = {
    "en": {
        "missing_title": "Missing title in document",
        "missing_author": "Missing author in document",
        "invalid_structure": "Invalid document structure",
        "accessibility_issue": "Accessibility compliance issue",
        "security_vulnerability": "Security vulnerability detected",
        "performance_warning": "Performance optimization recommended",
        "content_integrity": "Content integrity verification",
        "metadata_incomplete": "Document metadata is incomplete",
    },
    "es": {
        "missing_title": "Falta el título en el documento",
        "missing_author": "Falta el autor en el documento",
        "invalid_structure": "Estructura de documento inválida",
        "accessibility_issue": "Problema de cumplimiento de accesibilidad",
        "security_vulnerability": "Vulnerabilidad de seguridad detectada",
        "performance_warning": "Se recomienda optimización de rendimiento",
        "content_integrity": "Verificación de integridad del contenido",
        "metadata_incomplete": "Los metadatos del documento están incompletos",
    },
    "fr": {
        "missing_title": "Titre manquant dans le document",
        "missing_author": "Auteur manquant dans le document",
        "invalid_structure": "Structure de document invalide",
        "accessibility_issue": "Problème de conformité d'accessibilité",
        "security_vulnerability": "Vulnérabilité de sécurité détectée",
        "performance_warning": "Optimisation des performances recommandée",
        "content_integrity": "Vérification de l'intégrité du contenu",
        "metadata_incomplete": "Les métadonnées du document sont incomplètes",
    },
    "de": {
        "missing_title": "Titel im Dokument fehlt",
        "missing_author": "Autor im Dokument fehlt",
        "invalid_structure": "Ungültige Dokumentstruktur",
        "accessibility_issue": "Barrierefreiheitskonformitätsproblem",
        "security_vulnerability": "Sicherheitsanfälligkeit erkannt",
        "performance_warning": "Leistungsoptimierung empfohlen",
        "content_integrity": "Überprüfung der Inhaltintegrität",
        "metadata_incomplete": "Dokumentmetadaten sind unvollständig",
    },
    "ru": {
        "missing_title": "Отсутствует заголовок в документе",
        "missing_author": "Отсутствует автор в документе",
        "invalid_structure": "Неверная структура документа",
        "accessibility_issue": "Проблема соответствия доступности",
        "security_vulnerability": "Обнаружена уязвимость безопасности",
        "performance_warning": "Рекомендуется оптимизация производительности",
        "content_integrity": "Проверка целостности содержимого",
        "metadata_incomplete": "Метаданные документа неполные",
    },
    "zh": {
        "missing_title": "文档中缺少标题",
        "missing_author": "文档中缺少作者",
        "invalid_structure": "文档结构无效",
        "accessibility_issue": "可访问性合规问题",
        "security_vulnerability": "检测到安全漏洞",
        "performance_warning": "建议性能优化",
        "content_integrity": "内容完整性验证",
        "metadata_incomplete": "文档元数据不完整",
    },
    "ar": {
        "missing_title": "العنوان مفقود في المستند",
        "missing_author": "المؤلف مفقود في المستند",
        "invalid_structure": "بنية المستند غير صالحة",
        "accessibility_issue": "مشكلة توافق الوصول",
        "security_vulnerability": "تم الكشف عن ثغرة أمنية",
        "performance_warning": "يوصى بتحسين الأداء",
        "content_integrity": "التحقق من سلامة المحتوى",
        "metadata_incomplete": "بيانات الوصفية للمستند غير مكتملة",
    },
}


@dataclass
class LanguageConfig:
    """Language configuration settings."""

    code: str
    name: str
    direction: str  # ltr or rtl
    date_format: str
    decimal_separator: str
    thousands_separator: str
    encoding: str
    locale_name: Optional[str] = None


@dataclass
class I18nValidationResult:
    """Internationalized validation result."""

    base_result: ValidationResult
    language_code: str
    translated_messages: List[Dict[str, str]]
    cultural_adaptations: List[str]
    locale_specific_issues: List[str]


class InternationalizationValidator(BaseValidator):
    """Comprehensive internationalization and multi-language validation."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.default_language = config.get("default_language", "en") if config else "en"
        self.supported_languages = SUPPORTED_LANGUAGES
        self.validation_messages = VALIDATION_MESSAGES

        # Initialize translation catalogs
        self._init_translations()

        # Language-specific validation rules
        self.language_rules = {
            "ar": {
                "text_direction_checks": True,
                "rtl_layout_validation": True,
                "arabic_script_support": True,
            },
            "zh": {
                "cjk_character_validation": True,
                "vertical_text_support": True,
                "ime_compatibility": True,
            },
            "ru": {
                "cyrillic_script_validation": True,
                "case_sensitivity_checks": True,
                "locale_specific_formatting": True,
            },
        }

    def _init_translations(self):
        """Initialize translation catalogs."""
        self.translations = {}

        for lang_code, messages in VALIDATION_MESSAGES.items():
            self.translations[lang_code] = messages

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate file with internationalization support."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Detect document language
            detected_languages = self._detect_document_languages(file_path)
            stats["detected_languages"] = detected_languages

            # Validate language support
            language_errors = self._validate_language_support(
                detected_languages, file_path
            )
            errors.extend(language_errors)
            stats["language_checks"] = len(language_errors)

            # Validate cultural adaptations
            cultural_errors = self._validate_cultural_adaptations(
                detected_languages, file_path
            )
            errors.extend(cultural_errors)
            stats["cultural_checks"] = len(cultural_errors)

            # Validate locale-specific issues
            locale_errors = self._validate_locale_specific_issues(
                detected_languages, file_path
            )
            errors.extend(locale_errors)
            stats["locale_checks"] = len(locale_errors)

            # Validate text direction and layout
            layout_errors = self._validate_text_layout(detected_languages, file_path)
            errors.extend(layout_errors)
            stats["layout_checks"] = len(layout_errors)

            # Translate error messages
            translated_errors = self._translate_error_messages(
                errors, detected_languages
            )

            stats["total_checks"] = sum(
                [
                    stats["language_checks"],
                    stats["cultural_checks"],
                    stats["locale_checks"],
                    stats["layout_checks"],
                ]
            )

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"Internationalization validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="i18n-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=translated_errors,
            stats=stats,
        )

    def _detect_document_languages(self, file_path: Path) -> List[str]:
        """Detect languages used in the document."""
        detected_languages = []

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            # Language detection based on content analysis
            language_indicators = {
                "ar": re.findall(r"[\u0600-\u06FF]+", content),
                "zh": re.findall(r"[\u4E00-\u9FFF]+", content),
                "ru": re.findall(r"[\u0400-\u04FF]+", content),
                "de": re.findall(r"ß|ä|ö|ü|Ä|Ö|Ü", content),
                "fr": re.findall(r"à|â|é|è|ê|ë|î|ï|ô|û|ù|ç|œ", content),
                "es": re.findall(r"ñ|á|é|í|ó|ú|ü|¿|¡", content),
            }

            # Check for language declarations
            lang_declarations = re.findall(
                r'xml:lang="([^"]+)"|lang="([^"]+)"', content
            )
            for match in lang_declarations:
                lang_code = match if match else None
                if lang_code and lang_code in self.supported_languages:
                    detected_languages.append(lang_code)

            # Check character patterns
            for lang_code, characters in language_indicators.items():
                if characters and lang_code not in detected_languages:
                    # Minimum threshold for language detection
                    if len(characters) > 10:
                        detected_languages.append(lang_code)

            # Add English as default if no languages detected
            if not detected_languages:
                detected_languages.append("en")

        except Exception as e:
            logger.warning(f"Language detection failed: {e}")
            detected_languages.append("en")

        return list(set(detected_languages))

    def _validate_language_support(
        self, languages: List[str], file_path: Path
    ) -> List[ValidationError]:
        """Validate language-specific support requirements."""
        errors = []

        for lang in languages:
            if lang not in self.supported_languages:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Language {lang} not fully supported for validation",
                        file_path=str(file_path),
                        rule_id="i18n-unsupported-language",
                    )
                )
                continue

            lang_config = self.supported_languages[lang]

            # Check encoding compatibility
            try:
                content = file_path.read_text(
                    encoding=lang_config["encoding"], errors="ignore"
                )
            except UnicodeDecodeError:
                errors.append(
                    ValidationError(
                        severity="error",
                        message=f"File cannot be decoded with {lang_config['encoding']} encoding",
                        file_path=str(file_path),
                        rule_id="i18n-encoding-mismatch",
                    )
                )

            # Check language-specific rules
            if lang in self.language_rules:
                errors.extend(self._validate_language_specific_rules(lang, file_path))

        return errors

    def _validate_language_specific_rules(
        self, language: str, file_path: Path
    ) -> List[ValidationError]:
        """Validate language-specific validation rules."""
        errors = []

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")
            rules = self.language_rules.get(language, {})

            if language == "ar" and rules.get("rtl_layout_validation"):
                # RTL layout validation
                if 'dir="rtl"' not in content and "direction: rtl" not in content:
                    errors.append(
                        ValidationError(
                            severity="warning",
                            message="Arabic content missing RTL direction specification",
                            file_path=str(file_path),
                            rule_id="i18n-rtl-direction-missing",
                        )
                    )

            elif language == "zh" and rules.get("cjk_character_validation"):
                # CJK character validation
                cjk_chars = re.findall(r"[\u4E00-\u9FFF]", content)
                if len(cjk_chars) > 0:
                    # Check for proper CJK support declarations
                    if "cjk" not in content.lower():
                        errors.append(
                            ValidationError(
                                severity="info",
                                message="CJK characters detected but CJK support not declared",
                                file_path=str(file_path),
                                rule_id="i18n-cjk-support-missing",
                            )
                        )

            elif language == "ru" and rules.get("cyrillic_script_validation"):
                # Cyrillic script validation
                cyrillic_chars = re.findall(r"[\u0400-\u04FF]", content)
                if len(cyrillic_chars) > 0:
                    # Check for proper Cyrillic support
                    if "cyrillic" not in content.lower():
                        errors.append(
                            ValidationError(
                                severity="info",
                                message="Cyrillic characters detected but Cyrillic support not declared",
                                file_path=str(file_path),
                                rule_id="i18n-cyrillic-support-missing",
                            )
                        )

        except Exception as e:
            logger.warning(f"Language-specific validation failed: {e}")

        return errors

    def _validate_cultural_adaptations(
        self, languages: List[str], file_path: Path
    ) -> List[ValidationError]:
        """Validate cultural adaptations for different regions."""
        errors = []

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            for lang in languages:
                lang_config = self.supported_languages.get(lang, {})

                # Date format validation
                if lang == "en":
                    # Check for mm/dd/yyyy vs dd/mm/yyyy consistency
                    date_patterns = re.findall(
                        r"\d{1,2}[/-]\d{1,2}[/-]\d{2,4}", content
                    )
                    for pattern in date_patterns:
                        if (
                            "/" in pattern and len(pattern.split("/")[0]) == 4
                        ):  # YYYY/MM/DD format in English
                            errors.append(
                                ValidationError(
                                    severity="info",
                                    message="Non-standard date format detected for English locale",
                                    file_path=str(file_path),
                                    rule_id="i18n-date-format-cultural",
                                )
                            )

                elif lang == "de":
                    # Check for German-specific formatting
                    if ",." not in content and ".," not in content:
                        errors.append(
                            ValidationError(
                                severity="info",
                                message="German number formatting (1.234,56) not detected",
                                file_path=str(file_path),
                                rule_id="i18n-number-format-cultural",
                            )
                        )

                # Currency format validation
                if "$" in content and lang == "en":
                    if "€" in content or "£" in content:
                        errors.append(
                            ValidationError(
                                severity="warning",
                                message="Mixed currency symbols in English content",
                                file_path=str(file_path),
                                rule_id="i18n-currency-mix",
                            )
                        )

        except Exception as e:
            logger.warning(f"Cultural adaptation validation failed: {e}")

        return errors

    def _validate_locale_specific_issues(
        self, languages: List[str], file_path: Path
    ) -> List[ValidationError]:
        """Validate locale-specific formatting and conventions."""
        errors = []

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            for lang in languages:
                lang_config = self.supported_languages.get(lang, {})

                # Number formatting validation
                decimal_sep = lang_config.get("decimal_separator", ".")
                thousands_sep = lang_config.get("thousands_separator", ",")

                # Look for numbers with wrong separators
                number_patterns = re.findall(r"\d{1,3}(?:[.,])\d{2}", content)
                for number in number_patterns:
                    separator = re.search(r"([.,])", number).group(1)
                    if separator != decimal_sep:
                        errors.append(
                            ValidationError(
                                severity="info",
                                message=f"Wrong decimal separator for {lang}: found '{separator}', expected '{decimal_sep}'",
                                file_path=str(file_path),
                                rule_id="i18n-decimal-separator",
                            )
                        )

                # Text direction validation
                direction = lang_config.get("direction", "ltr")
                if direction == "rtl":
                    if 'dir="rtl"' not in content and "direction: rtl" not in content:
                        errors.append(
                            ValidationError(
                                severity="warning",
                                message=f"RTL text direction not specified for {lang}",
                                file_path=str(file_path),
                                rule_id="i18n-text-direction-missing",
                            )
                        )

        except Exception as e:
            logger.warning(f"Locale-specific validation failed: {e}")

        return errors

    def _validate_text_layout(
        self, languages: List[str], file_path: Path
    ) -> List[ValidationError]:
        """Validate text layout and bidirectional text handling."""
        errors = []

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            # Check for mixed LTR/RTL text
            ltr_chars = re.findall(r"[A-Za-z0-9\p{L}]+", content)
            rtl_chars = re.findall(r"[\u0600-\u06FF\p{Arabic}]+", content)

            if ltr_chars and rtl_chars:
                # Check for proper Unicode bidirectional handling
                if "bidi-override" not in content and "unicode-bidi" not in content:
                    errors.append(
                        ValidationError(
                            severity="info",
                            message="Mixed LTR/RTL text detected but bidirectional handling not specified",
                            file_path=str(file_path),
                            rule_id="i18n-bidi-text",
                        )
                    )

            # Check for font support declarations
            for lang in languages:
                if lang == "zh":
                    if "cjk" not in content.lower() and "font-family" not in content:
                        errors.append(
                            ValidationError(
                                severity="warning",
                                message="CJK content detected but no CJK font support declared",
                                file_path=str(file_path),
                                rule_id="i18n-font-support-missing",
                            )
                        )

            # Check for character encoding declaration
            if "charset=" not in content and "encoding=" not in content:
                errors.append(
                    ValidationError(
                        severity="info",
                        message="Character encoding not explicitly declared",
                        file_path=str(file_path),
                        rule_id="i18n-encoding-declaration-missing",
                    )
                )

        except Exception as e:
            logger.warning(f"Text layout validation failed: {e}")

        return errors

    def _translate_error_messages(
        self, errors: List[ValidationError], languages: List[str]
    ) -> List[ValidationError]:
        """Translate error messages to detected languages."""
        translated_errors = []

        # Primary language for translation
        primary_lang = languages[0] if languages else self.default_language

        for error in errors:
            # Get translation for primary language
            if primary_lang in self.translations:
                messages = self.translations[primary_lang]

                # Try to translate the error message
                translated_message = self._translate_message(error.message, messages)

                # Create translated error
                translated_error = ValidationError(
                    severity=error.severity,
                    message=translated_message,
                    file_path=error.file_path,
                    rule_id=error.rule_id,
                    line_number=error.line_number,
                    column_number=error.column_number,
                )
                translated_errors.append(translated_error)
            else:
                # Keep original error if translation not available
                translated_errors.append(error)

        return translated_errors

    def _translate_message(self, message: str, translation_dict: Dict[str, str]) -> str:
        """Translate a message using the translation dictionary."""
        # Simple pattern matching for translation
        for key, translation in translation_dict.items():
            # Check if message contains the English key pattern
            english_key = VALIDATION_MESSAGES["en"].get(key, "")
            if english_key and english_key.lower() in message.lower():
                return translation

        # Return original message if no translation found
        return message

    def get_language_config(self, language_code: str) -> Optional[LanguageConfig]:
        """Get language configuration for a specific language."""
        if language_code not in self.supported_languages:
            return None

        lang_data = self.supported_languages[language_code]
        return LanguageConfig(
            code=lang_data["code"],
            name=lang_data["name"],
            direction=lang_data["direction"],
            date_format=lang_data["date_format"],
            decimal_separator=lang_data["decimal_separator"],
            thousands_separator=lang_data["thousands_separator"],
            encoding=lang_data["encoding"],
            locale_name=f"{lang_data['code']}_{'UTF-8' if lang_data['encoding'] == 'utf-8' else lang_data['encoding'].upper()}",
        )

    def generate_i18n_report(self, result: ValidationResult) -> Dict[str, Any]:
        """Generate comprehensive internationalization report."""

        report = {
            "summary": {
                "languages_detected": result.stats.get("detected_languages", []),
                "supported_languages": list(self.supported_languages.keys()),
                "total_i18n_issues": len(
                    [e for e in result.errors if "i18n" in e.rule_id]
                ),
                "cultural_adaptations": result.stats.get("cultural_checks", 0),
                "locale_specific_issues": result.stats.get("locale_checks", 0),
            },
            "language_analysis": {},
            "translation_coverage": {},
            "accessibility_by_language": {},
            "recommendations": [],
        }

        # Analyze each detected language
        detected_languages = result.stats.get("detected_languages", [])
        for lang in detected_languages:
            if lang in self.supported_languages:
                lang_config = self.supported_languages[lang]
                report["language_analysis"][lang] = {
                    "name": lang_config["name"],
                    "direction": lang_config["direction"],
                    "encoding": lang_config["encoding"],
                    "support_level": "full",
                }
            else:
                report["language_analysis"][lang] = {
                    "name": lang,
                    "support_level": "partial",
                }

        # Count issues by language
        issues_by_language = {}
        for error in result.errors:
            if "i18n" in error.rule_id:
                lang = (
                    error.rule_id.split("-")[-1] if "-" in error.rule_id else "unknown"
                )
                if lang not in issues_by_language:
                    issues_by_language[lang] = []
                issues_by_language[lang].append(error.message)

        report["translation_coverage"] = issues_by_language

        # Generate recommendations
        if detected_languages:
            report["recommendations"].append(
                "Declare language attributes (lang, xml:lang) in HTML/XML"
            )
            report["recommendations"].append(
                "Use appropriate character encoding declarations"
            )
            report["recommendations"].append(
                "Implement proper text direction for RTL languages"
            )
            report["recommendations"].append("Consider cultural formatting differences")

        return report

    def set_translation_domain(self, domain: str, locale_path: Optional[Path] = None):
        """Set up gettext translation domain."""
        try:
            # Initialize gettext for the domain
            if locale_path:
                gettext.bindtextdomain(domain)
                gettext.bindtextdomain(domain, str(locale_path))

            logger.info(f"Translation domain '{domain}' set up successfully")
        except Exception as e:
            logger.warning(f"Failed to set up translation domain: {e}")

    def get_supported_languages(self) -> Dict[str, LanguageConfig]:
        """Get all supported language configurations."""
        supported = {}
        for lang_code in self.supported_languages:
            config = self.get_language_config(lang_code)
            if config:
                supported[lang_code] = config
        return supported
