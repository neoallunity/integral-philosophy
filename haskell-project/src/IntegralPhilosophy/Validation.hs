{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core validation framework
module IntegralPhilosophy.Validation
  ( -- * Validation interface
    Validator(..)
  , Validation(..)
    
    -- * Core validation functions  
  , validateDocument
  , validateMultiple
  , combineResults
  , calculateQualityScore
    
    -- * Error handling
  , ValidationError(..)
  , ValidationSeverity(..)
  , ValidationResult(..)
  , makeError
  , makeWarning
  , makeInfo
  ) where

import IntegralPhilosophy.Types
import qualified IntegralPhilosophy.Utils.Time as TimeUtils
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.DeepSeq (NFData, rnf)

-- | Validation interface
class Validator a where
  validate :: MonadIO m => a -> Document -> m ValidationResult
  validatorName :: a -> Text
  validatorType :: a -> ContentType

-- | Validation context
data Validation = Validation
  { valConfig :: !ValidationConfig
  , valContext :: !ProcessingContext
  , valStartTime :: !UTCTime
  } deriving (Show, Eq)

-- | Validate a single document with a specific validator
validateDocument :: (MonadIO m, NFData a, Validator a) => a -> Document -> m ValidationResult
validateDocument validator doc = do
  startTime <- liftIO getCurrentTime
  result <- validate validator doc
  endTime <- liftIO getCurrentTime
  pure $ result
    { vrProcessingTime = calculateTime startTime endTime
    }

-- | Validate multiple documents with multiple validators
validateMultiple :: (MonadIO m, NFData a, Validator a) => [a] -> [Document] -> m [ValidationResult]
validateMultiple validators documents = do
  results <- mapM (\doc -> mapM (`validateDocument` doc) validators) documents
  pure $ concat results

-- | Combine multiple validation results
combineResults :: [ValidationResult] -> ValidationResult
combineResults [] = ValidationResult [] [] (QualityScore 100.0) 0.0
combineResults results = ValidationResult
  { vrErrors = concatMap vrErrors results
  , vrWarnings = concatMap vrWarnings results
  , vrScore = calculateQualityScore results
  , vrProcessingTime = sum $ map vrProcessingTime results
  }

-- | Calculate quality score from validation results
calculateQualityScore :: [ValidationResult] -> QualityScore
calculateQualityScore [] = QualityScore 100.0
calculateQualityScore results = QualityScore $ max 0 $ 100.0 - errorPenalty - warningPenalty
  where
    totalErrors = sum $ length . vrErrors <$> results
    totalWarnings = sum $ length . vrWarnings <$> results
    errorPenalty = fromIntegral totalErrors * 10.0
    warningPenalty = fromIntegral totalWarnings * 2.0

-- | Calculate time difference (local version to avoid conflicts)
calculateTime :: UTCTime -> UTCTime -> Double
calculateTime start end = realToFrac $ diffUTCTime end start

-- | Error constructors
makeError :: ValidationSeverity -> Text -> Text -> FilePath -> Int -> Int -> ValidationError
makeError = ValidationError

makeWarning :: Text -> Text -> FilePath -> Int -> Int -> ValidationError
makeWarning = ValidationError Warning

makeInfo :: Text -> Text -> FilePath -> Int -> Int -> ValidationError  
makeInfo = ValidationError Info