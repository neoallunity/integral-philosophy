#!/usr/bin/env python3
"""
Machine Learning-based Quality Prediction for Integral Philosophy publishing system.
Implements predictive analytics and quality scoring using ML algorithms.
"""

import json
import time
import pickle
import hashlib
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import logging
from collections import defaultdict
import statistics
import re

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)


@dataclass
class QualityFeatures:
    """Feature set for ML quality prediction."""

    # File characteristics
    file_size_kb: float
    file_extension: str
    content_length: int
    word_count: int
    paragraph_count: int

    # Validation metrics
    error_count: int
    warning_count: int
    info_count: int
    security_score: int
    accessibility_score: int

    # Content characteristics
    has_images: int
    has_tables: int
    has_links: int
    has_headers: int
    unique_words: int
    avg_sentence_length: float

    # Linguistic features
    readability_score: float
    complexity_score: float
    keyword_density: Dict[str, float]
    language_detected: str


@dataclass
class QualityPrediction:
    """ML-based quality prediction result."""

    file_path: str
    predicted_score: float  # 0-100
    confidence: float  # 0-1
    quality_tier: str  # "excellent", "good", "fair", "poor", "critical"
    key_factors: List[str]
    recommendations: List[str]
    similar_files: List[str]
    processing_time: float
    model_version: str


class QualityPredictor:
    """Machine learning-based quality prediction system."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.model_path = Path(
            self.config.get("model_path", "models/quality_predictor.pkl")
        )
        self.feature_cache = {}
        self.prediction_history = []

        # ML model parameters
        self.model_params = {
            "n_estimators": 100,
            "max_depth": 10,
            "min_samples_split": 5,
            "random_state": 42,
        }

        # Quality thresholds
        self.quality_thresholds = {
            "excellent": 90,
            "good": 75,
            "fair": 60,
            "poor": 40,
            "critical": 0,
        }

        # Initialize model
        self.model = None
        self._load_or_create_model()

        # Feature extractors
        self.feature_extractors = {
            "basic": self._extract_basic_features,
            "content": self._extract_content_features,
            "linguistic": self._extract_linguistic_features,
            "structural": self._extract_structural_features,
            "historical": self._extract_historical_features,
        }

    def _load_or_create_model(self):
        """Load existing model or create new one."""
        try:
            if self.model_path.exists():
                with open(self.model_path, "rb") as f:
                    self.model = pickle.load(f)
                logger.info(f"Loaded ML model from {self.model_path}")
            else:
                self._create_initial_model()
        except Exception as e:
            logger.warning(f"Failed to load ML model: {e}, creating new model")
            self._create_initial_model()

    def _create_initial_model(self):
        """Create initial ML model with synthetic data."""
        logger.info("Creating initial ML model...")

        # Create simple rule-based model initially
        self.model = {
            "type": "rule_based",
            "rules": {
                "error_weight": -2.0,
                "warning_weight": -0.5,
                "security_weight": -1.5,
                "accessibility_weight": -1.0,
                "size_weight": -0.1,
                "base_score": 100.0,
            },
            "feature_weights": {
                "file_size": 0.1,
                "error_count": -2.0,
                "warning_count": -0.5,
                "content_length": 0.05,
                "complexity": -0.3,
                "readability": 0.2,
            },
        }

    def predict_quality(
        self, file_path: Path, validation_result: Optional[ValidationResult] = None
    ) -> QualityPrediction:
        """Predict quality score using ML model."""

        start_time = time.time()

        try:
            # Extract features
            features = self._extract_features(file_path, validation_result)

            # Generate prediction
            prediction = self._make_prediction(features)

            # Calculate processing time
            processing_time = time.time() - start_time

            # Create quality prediction
            result = QualityPrediction(
                file_path=str(file_path),
                predicted_score=prediction["score"],
                confidence=prediction["confidence"],
                quality_tier=prediction["tier"],
                key_factors=prediction["key_factors"],
                recommendations=prediction["recommendations"],
                similar_files=prediction["similar_files"],
                processing_time=processing_time,
                model_version=self.model.get("version", "1.0"),
            )

            # Store prediction for future learning
            self.prediction_history.append(
                {
                    "features": features,
                    "prediction": prediction,
                    "timestamp": datetime.now(),
                    "file_path": str(file_path),
                }
            )

            return result

        except Exception as e:
            logger.error(f"Quality prediction failed: {e}")
            # Return fallback prediction
            return QualityPrediction(
                file_path=str(file_path),
                predicted_score=50.0,
                confidence=0.1,
                quality_tier="fair",
                key_factors=["prediction_error"],
                recommendations=[f"Error during prediction: {str(e)}"],
                similar_files=[],
                processing_time=time.time() - start_time,
                model_version="error",
            )

    def _extract_features(
        self, file_path: Path, validation_result: Optional[ValidationResult]
    ) -> QualityFeatures:
        """Extract comprehensive feature set from file and validation results."""

        # Check cache first
        file_hash = self._get_file_hash(file_path)
        if file_hash in self.feature_cache:
            return self.feature_cache[file_hash]

        # Extract basic features
        basic_features = self.feature_extractors["basic"](file_path)

        # Extract content features
        content_features = self.feature_extractors["content"](file_path)

        # Extract linguistic features
        linguistic_features = self.feature_extractors["linguistic"](file_path)

        # Extract structural features
        structural_features = self.feature_extractors["structural"](file_path)

        # Extract historical features
        historical_features = self.feature_extractors["historical"](file_path)

        # Extract validation features
        validation_features = self._extract_validation_features(validation_result)

        # Combine all features
        features = QualityFeatures(
            file_size_kb=basic_features["file_size_kb"],
            file_extension=basic_features["file_extension"],
            content_length=content_features["content_length"],
            word_count=content_features["word_count"],
            paragraph_count=content_features["paragraph_count"],
            error_count=validation_features["error_count"],
            warning_count=validation_features["warning_count"],
            info_count=validation_features["info_count"],
            security_score=validation_features["security_score"],
            accessibility_score=validation_features["accessibility_score"],
            has_images=structural_features["has_images"],
            has_tables=structural_features["has_tables"],
            has_links=structural_features["has_links"],
            has_headers=structural_features["has_headers"],
            unique_words=linguistic_features["unique_words"],
            avg_sentence_length=linguistic_features["avg_sentence_length"],
            readability_score=linguistic_features["readability_score"],
            complexity_score=linguistic_features["complexity_score"],
            keyword_density=linguistic_features["keyword_density"],
            language_detected=linguistic_features["language_detected"],
        )

        # Cache features
        self.feature_cache[file_hash] = features

        return features

    def _extract_basic_features(self, file_path: Path) -> Dict[str, Any]:
        """Extract basic file features."""

        file_stats = file_path.stat()
        return {
            "file_size_kb": file_stats.st_size / 1024,
            "file_extension": file_path.suffix.lower(),
            "creation_time": file_stats.st_ctime,
            "modification_time": file_stats.st_mtime,
        }

    def _extract_content_features(self, file_path: Path) -> Dict[str, Any]:
        """Extract content-based features."""

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            # Count words and paragraphs
            words = re.findall(r"\b\w+\b", content)
            paragraphs = re.split(r"\n\s*\n", content)

            return {
                "content_length": len(content),
                "word_count": len(words),
                "paragraph_count": len([p for p in paragraphs if p.strip()]),
                "char_count": len(content),
                "line_count": len(content.split("\n")),
            }
        except Exception:
            return {
                "content_length": 0,
                "word_count": 0,
                "paragraph_count": 0,
                "char_count": 0,
                "line_count": 0,
            }

    def _extract_linguistic_features(self, file_path: Path) -> Dict[str, Any]:
        """Extract linguistic analysis features."""

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            # Calculate readability (simplified)
            sentences = re.split(r"[.!?]+", content)
            avg_sentence_len = statistics.mean(
                [len(s.split()) for s in sentences if s.strip()]
            )

            # Calculate vocabulary complexity
            words = re.findall(r"\b\w+\b", content.lower())
            unique_words = len(set(words))
            total_words = len(words)

            # Flesch Reading Ease (simplified)
            avg_words_per_sentence = total_words / len(sentences) if sentences else 0
            avg_syllables_per_word = 1.5  # Simplified estimate
            readability_score = (
                206.835 - 1.015 * avg_words_per_sentence - 84.6 * avg_syllables_per_word
            )

            # Language detection (simplified)
            language_detected = "en"  # Default to English

            return {
                "unique_words": unique_words,
                "avg_sentence_length": avg_sentence_len,
                "readability_score": max(0, min(100, readability_score)),
                "complexity_score": (unique_words / total_words * 100)
                if total_words > 0
                else 0,
                "language_detected": language_detected,
                "keyword_density": self._extract_keywords(content),
            }
        except Exception:
            return {
                "unique_words": 0,
                "avg_sentence_length": 0,
                "readability_score": 50,
                "complexity_score": 0,
                "language_detected": "unknown",
                "keyword_density": {},
            }

    def _extract_keywords(self, content: str) -> Dict[str, float]:
        """Extract keyword density from content."""

        # Academic keywords
        academic_keywords = [
            "philosophy",
            "theory",
            "analysis",
            "research",
            "method",
            "concept",
            "framework",
            "paradigm",
            "critique",
            "discourse",
        ]

        content_lower = content.lower()
        word_count = len(re.findall(r"\b\w+\b", content_lower))

        keyword_density = {}
        if word_count > 0:
            for keyword in academic_keywords:
                occurrences = content_lower.count(keyword)
                density = (occurrences / word_count) * 100
                if density > 0:
                    keyword_density[keyword] = density

        return keyword_density

    def _extract_structural_features(self, file_path: Path) -> Dict[str, Any]:
        """Extract structural features from file."""

        try:
            content = file_path.read_text(encoding="utf-8", errors="ignore")

            # Count structural elements
            has_images = len(re.findall(r"<img[^>]*>", content)) > 0
            has_tables = len(re.findall(r"<table[^>]*>", content)) > 0
            has_links = len(re.findall(r"<a[^>]*href", content)) > 0
            has_headers = len(re.findall(r"<h[1-6][^>]*>", content)) > 0

            return {
                "has_images": int(has_images),
                "has_tables": int(has_tables),
                "has_links": int(has_links),
                "has_headers": int(has_headers),
            }
        except Exception:
            return {"has_images": 0, "has_tables": 0, "has_links": 0, "has_headers": 0}

    def _extract_historical_features(self, file_path: Path) -> Dict[str, Any]:
        """Extract historical features from prediction history."""

        # Look at similar files in history
        file_name = file_path.stem
        similar_predictions = [
            p
            for p in self.prediction_history
            if file_name in p["file_path"]
            and (datetime.now() - p["timestamp"]).days < 30
        ]

        if similar_predictions:
            avg_score = statistics.mean(
                [p["prediction"]["score"] for p in similar_predictions]
            )
            return {
                "historical_avg_score": avg_score,
                "similar_file_count": len(similar_predictions),
                "score_trend": "stable",
            }
        else:
            return {
                "historical_avg_score": 75.0,  # Default good score
                "similar_file_count": 0,
                "score_trend": "new",
            }

    def _extract_validation_features(
        self, validation_result: Optional[ValidationResult]
    ) -> Dict[str, Any]:
        """Extract features from validation result."""

        if not validation_result:
            return {
                "error_count": 0,
                "warning_count": 0,
                "info_count": 0,
                "security_score": 100,
                "accessibility_score": 100,
            }

        error_count = len(
            [e for e in validation_result.errors if e.severity == "error"]
        )
        warning_count = len(
            [e for e in validation_result.errors if e.severity == "warning"]
        )
        info_count = len([e for e in validation_result.errors if e.severity == "info"])

        # Extract security and accessibility scores from stats
        stats = validation_result.stats or {}
        security_score = max(
            0,
            100 - stats.get("critical_vulns", 0) * 10 - stats.get("high_vulns", 0) * 5,
        )
        accessibility_score = max(0, 100 - stats.get("aa_compliance_issues", 0) * 3)

        return {
            "error_count": error_count,
            "warning_count": warning_count,
            "info_count": info_count,
            "security_score": security_score,
            "accessibility_score": accessibility_score,
        }

    def _make_prediction(self, features: QualityFeatures) -> Dict[str, Any]:
        """Make quality prediction using ML model."""

        if self.model["type"] == "rule_based":
            return self._rule_based_prediction(features)
        else:
            return self._ml_based_prediction(features)

    def _rule_based_prediction(self, features: QualityFeatures) -> Dict[str, Any]:
        """Make rule-based quality prediction."""

        rules = self.model["rules"]
        base_score = rules["base_score"]

        # Apply feature weights
        score_adjustments = 0

        # Size penalty
        if features.file_size_kb > 1000:  # > 1MB
            score_adjustments += rules["size_weight"] * (features.file_size_kb / 1000)

        # Error penalties
        score_adjustments += rules["error_weight"] * features.error_count
        score_adjustments += rules["warning_weight"] * features.warning_count

        # Security and accessibility scores
        score_adjustments += (features.security_score - 100) * 0.5
        score_adjustments += (features.accessibility_score - 100) * 0.3

        # Content quality bonuses
        if features.content_length > 1000:
            score_adjustments += rules["content_length"] * (
                features.content_length / 1000
            )

        # Add readability bonus if rule exists
        readability_bonus = rules.get("readability", 0)
        complexity_penalty = rules.get("complexity", 0)

        if readability_bonus > 0 and features.readability_score > 70:
            score_adjustments += readability_bonus * 2

        if complexity_penalty < 0 and features.complexity_score < 30:  # Not too complex
            score_adjustments += abs(complexity_penalty) * 2

        final_score = max(0, min(100, base_score + score_adjustments))

        # Determine key factors
        key_factors = []
        if features.error_count > 0:
            key_factors.append(f"{features.error_count} errors found")
        if features.security_score < 80:
            key_factors.append("Security issues detected")
        if features.accessibility_score < 70:
            key_factors.append("Accessibility improvements needed")
        if features.file_size_kb > 5000:
            key_factors.append("Large file size impacts score")

        # Determine quality tier
        quality_tier = self._determine_quality_tier(final_score)

        # Generate recommendations
        recommendations = self._generate_rule_recommendations(features, final_score)

        # Find similar files (simplified)
        similar_files = self._find_similar_files(features)

        return {
            "score": final_score,
            "confidence": 0.8,  # High confidence for rule-based
            "tier": quality_tier,
            "key_factors": key_factors,
            "recommendations": recommendations,
            "similar_files": similar_files,
        }

    def _ml_based_prediction(self, features: QualityFeatures) -> Dict[str, Any]:
        """Make ML-based quality prediction."""

        # For now, fallback to rule-based
        # In production, this would use trained ML model
        return self._rule_based_prediction(features)

    def _determine_quality_tier(self, score: float) -> str:
        """Determine quality tier from score."""

        for tier, threshold in sorted(
            self.quality_thresholds.items(), key=lambda x: x[1], reverse=True
        ):
            if score >= threshold:
                return tier
        return "critical"

    def _generate_rule_recommendations(
        self, features: QualityFeatures, score: float
    ) -> List[str]:
        """Generate recommendations based on features and score."""

        recommendations = []

        # Error-based recommendations
        if features.error_count > 0:
            recommendations.append("Fix validation errors to improve quality score")

        # Size-based recommendations
        if features.file_size_kb > 10000:  # > 10MB
            recommendations.append("Consider file compression for better performance")

        # Content-based recommendations
        if features.content_length < 500:
            recommendations.append("Add more content to improve completeness")

        # Readability recommendations
        if features.readability_score < 50:
            recommendations.append("Improve text readability with simpler language")

        # Accessibility recommendations
        if features.accessibility_score < 60:
            recommendations.append("Enhance accessibility features for compliance")

        # Security recommendations
        if features.security_score < 70:
            recommendations.append("Address security vulnerabilities for safety")

        return recommendations

    def _find_similar_files(self, features: QualityFeatures) -> List[str]:
        """Find similar files based on features."""

        similar_files = []

        # Look for files with similar characteristics in history
        for prediction in self.prediction_history[-50:]:  # Last 50 predictions
            historical_features = prediction.get("features")
            if not historical_features:
                continue

            # Calculate similarity score (simplified)
            similarity = 0

            # Same file extension
            if historical_features.file_extension == features.file_extension:
                similarity += 20

            # Similar file size
            size_diff = abs(historical_features.file_size_kb - features.file_size_kb)
            if size_diff < 100:  # Within 100KB
                similarity += 15

            # Similar word count
            if abs(historical_features.word_count - features.word_count) < 50:
                similarity += 10

            if similarity > 30:
                similar_files.append(prediction["file_path"])

        return similar_files[:5]  # Top 5 similar

    def _get_file_hash(self, file_path: Path) -> str:
        """Get file hash for caching."""

        try:
            file_stats = file_path.stat()
            hash_data = f"{file_stats.st_size}_{file_stats.st_mtime}"
            return hashlib.md5(hash_data.encode()).hexdigest()
        except Exception:
            return hashlib.md5(str(file_path).encode()).hexdigest()

    def train_model(self, training_data: List[Dict[str, Any]]):
        """Train ML model with historical data."""

        logger.info(f"Training ML model with {len(training_data)} samples...")

        # For now, update rule-based model weights
        if len(training_data) > 10:
            # Calculate feature importance from data
            self._update_model_weights(training_data)

        logger.info("ML model training completed")

    def _update_model_weights(self, training_data: List[Dict[str, Any]]):
        """Update model weights based on training data."""

        # Simple weight optimization based on feature importance
        rules = self.model["rules"]

        # Calculate correlation between features and actual quality scores
        feature_importance = self._calculate_feature_importance(training_data)

        # Update weights based on importance
        if "error_count" in feature_importance:
            rules["error_weight"] = -2.0 * feature_importance["error_count"]

        if "security_score" in feature_importance:
            rules["security_weight"] = -1.0 * feature_importance["security_score"]

        self.model["version"] = "2.0"

    def _calculate_feature_importance(
        self, training_data: List[Dict[str, Any]]
    ) -> Dict[str, float]:
        """Calculate feature importance from training data."""

        # Simplified importance calculation
        importance = {}

        error_correlation = []
        security_correlation = []
        size_correlation = []

        for item in training_data:
            if "features" in item and "actual_quality" in item:
                features = item["features"]
                actual_quality = item["actual_quality"]

                error_correlation.append(
                    abs(features.error_count - (100 - actual_quality))
                )
                security_correlation.append(
                    abs(features.security_score - actual_quality)
                )
                size_correlation.append(
                    abs(features.file_size_kb / 1000) if actual_quality > 50 else 0
                )

        if error_correlation:
            importance["error_count"] = statistics.mean(error_correlation) / max(
                error_correlation
            )

        if security_correlation:
            importance["security_score"] = statistics.mean(security_correlation) / max(
                security_correlation
            )

        return importance

    def save_model(self, path: Optional[Path] = None):
        """Save trained model to disk."""

        save_path = path or self.model_path

        try:
            with open(save_path, "wb") as f:
                pickle.dump(self.model, f)
            logger.info(f"Model saved to {save_path}")
        except Exception as e:
            logger.error(f"Failed to save model: {e}")

    def get_prediction_accuracy(
        self, test_data: List[Dict[str, Any]]
    ) -> Dict[str, float]:
        """Calculate model prediction accuracy."""

        if not test_data:
            return {"accuracy": 0.0, "mae": 0.0}

        predictions = []
        actuals = []

        for item in test_data:
            if "features" in item and "actual_quality" in item:
                # Make prediction (simplified)
                prediction = self._rule_based_prediction(item["features"])
                predictions.append(prediction["score"])
                actuals.append(item["actual_quality"])

        if not predictions:
            return {"accuracy": 0.0, "mae": 0.0}

        # Calculate metrics
        correct_predictions = sum(
            1 for p, a in zip(predictions, actuals) if abs(p - a) < 10
        )
        accuracy = correct_predictions / len(predictions)

        mae = statistics.mean([abs(p - a) for p, a in zip(predictions, actuals)])

        return {"accuracy": accuracy, "mae": mae, "samples": len(predictions)}

    def generate_quality_report(
        self, predictions: List[QualityPrediction]
    ) -> Dict[str, Any]:
        """Generate comprehensive quality prediction report."""

        if not predictions:
            return {
                "summary": {"total_predictions": 0, "average_score": 0},
                "distribution": {},
                "recommendations": [],
            }

        # Calculate statistics
        scores = [p.predicted_score for p in predictions]
        confidences = [p.confidence for p in predictions]

        # Quality distribution
        distribution = defaultdict(int)
        for prediction in predictions:
            distribution[prediction.quality_tier] += 1

        # Calculate summary statistics
        summary = {
            "total_predictions": len(predictions),
            "average_score": statistics.mean(scores),
            "average_confidence": statistics.mean(confidences),
            "score_std": statistics.stdev(scores) if len(scores) > 1 else 0,
            "distribution": dict(distribution),
        }

        # Generate recommendations
        recommendations = []

        if summary["average_score"] < 70:
            recommendations.append(
                "Overall quality below target - implement quality improvement program"
            )

        low_confidence_count = len([c for c in confidences if c < 0.5])
        if low_confidence_count > len(predictions) * 0.3:
            recommendations.append(
                "Many low confidence predictions - gather more training data"
            )

        # Model-specific recommendations
        if self.model.get("version", "1.0") == "1.0":
            recommendations.append(
                "Consider training ML model with more data for better accuracy"
            )

        return {
            "summary": summary,
            "predictions": [
                {
                    "file_path": p.file_path,
                    "score": p.predicted_score,
                    "confidence": p.confidence,
                    "tier": p.quality_tier,
                    "key_factors": p.key_factors,
                    "recommendations": p.recommendations,
                }
                for p in predictions
            ],
            "model_info": {
                "version": self.model.get("version", "1.0"),
                "type": self.model.get("type", "rule_based"),
                "training_samples": len(self.prediction_history),
            },
            "recommendations": recommendations,
        }
