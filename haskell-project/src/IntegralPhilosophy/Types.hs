{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Core types and utilities for the Integral Philosophy Publishing System
module IntegralPhilosophy.Types
  ( -- * Basic Types
    FilePath
  , Text
  , ValidationSeverity(..)
  , ValidationError(..)
  , ValidationResult(..)
  , QualityScore(..)
  , ProcessingContext(..)
    
    -- * Content Types
  , ContentType(..)
  , Document(..)
  , Section(..)
  , MathExpression(..)
  , Citation(..)
  , Metadata(..)
    
    -- * Configuration
  , Config(..)
  , ValidationConfig(..)
  , OutputConfig(..)
    
    -- * Results
  , ProcessingResult(..)
  , BatchResult(..)
  , QualityReport(..)
  ) where

import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map
import System.FilePath (FilePath)

type HashMap k v = Map.Map k v

-- | Text type alias
type Text = T.Text

-- | Severity of validation errors
data ValidationSeverity
  = Info
  | Warning
  | Error
  | Critical
  deriving (Show, Eq, Ord)

-- | Individual validation error
data ValidationError = ValidationError
  { veSeverity :: !ValidationSeverity
  , veCode :: !Text
  , veMessage :: !Text
  , veFilePath :: !FilePath
  , veLineNumber :: !Int
  , veColumnNumber :: !Int
  } deriving (Show, Eq)

-- | Result of validation
data ValidationResult = ValidationResult
  { vrErrors :: ![ValidationError]
  , vrWarnings :: ![ValidationError]
  , vrScore :: !QualityScore
  , vrProcessingTime :: !Double
  } deriving (Show, Eq)

-- | Quality score (0-100)
newtype QualityScore = QualityScore 
  { getQualityScore :: Double
  } deriving (Show, Eq, Ord, Num)

-- | Processing context
data ProcessingContext = ProcessingContext
  { pcConfig :: !Config
  , pcStartTime :: !UTCTime
  , pcWorkingDirectory :: !FilePath
  } deriving (Show, Eq)

-- | Content type enumeration
data ContentType
  = LaTeX
  | HTML5
  | EPUB3
  | PDF
  | DOCX
  | Markdown
  | TEI
  deriving (Show, Eq, Ord)

-- | Document representation
data Document = Document
  { docTitle :: !Text
  , docAuthor :: !Text
  , docAbstract :: !Text
  , docKeywords :: ![Text]
  , docSections :: ![Section]
  , docMathExpressions :: ![MathExpression]
  , docCitations :: ![Citation]
  , docMetadata :: !Metadata
  , docContentType :: !ContentType
  } deriving (Show, Eq)

-- | Document section
data Section = Section
  { secLevel :: !Int
  , secTitle :: !Text
  , secContent :: !Text
  , secSubsections :: ![Section]
  } deriving (Show, Eq)

-- | Mathematical expression
data MathExpression
  = InlineMath !Text
  | DisplayMath !Text
  | Equation !Text ![Text] -- equation body, labels
  deriving (Show, Eq)

-- | Citation reference
data Citation = Citation
  { citKey :: !Text
  , citPages :: !(Maybe Text)
  , citNote :: !(Maybe Text)
  } deriving (Show, Eq)

-- | Document metadata
data Metadata = Metadata
  { metaLanguage :: !Text
  , metaCreationDate :: !(Maybe UTCTime)
  , metaModificationDate :: !(Maybe UTCTime)
  , metaWordCount :: !Int
  , metaCharCount :: !Int
  , metaCustomFields :: !(HashMap Text Text)
  } deriving (Show, Eq)

-- | Main configuration
data Config = Config
  { cfgValidation :: !ValidationConfig
  , cfgOutput :: !OutputConfig
  , cfgVerbose :: !Bool
  , cfgParallel :: !Bool
  , cfgMaxThreads :: !Int
  } deriving (Show, Eq)

-- | Validation configuration
data ValidationConfig = ValidationConfig
  { vcEnableHTML5 :: !Bool
  , vcEnableEPUB3 :: !Bool
  , vcEnablePDF :: !Bool
  , vcEnableDOCX :: !Bool
  , vcEnableAccessibility :: !Bool
  , vcEnableSecurity :: !Bool
  , vcStrictMode :: !Bool
  , vcTimeoutSeconds :: !Int
  } deriving (Show, Eq)

-- | Output configuration
data OutputConfig = OutputConfig
  { ocOutputDirectory :: !FilePath
  , ocFormat :: ![ContentType]
  , ocGenerateReports :: !Bool
  , ocReportFormat :: ![Text] -- "json", "html", "markdown"
  , ocPreserveOriginal :: !Bool
  } deriving (Show, Eq)

-- | Single processing result
data ProcessingResult a = ProcessingResult
  { prInput :: !a
  , prOutput :: !(Either Text a)
  , prValidation :: !ValidationResult
  , prTimestamp :: !UTCTime
  } deriving (Show, Eq)

-- | Batch processing result
data BatchResult a = BatchResult
  { brResults :: ![ProcessingResult a]
  , brTotalCount :: !Int
  , brSuccessCount :: !Int
  , brFailureCount :: !Int
  , brTotalTime :: !Double
  , brQualityScore :: !QualityScore
  } deriving (Show, Eq)

-- | Quality report
data QualityReport = QualityReport
  { qrDocument :: !Document
  , qrOverallScore :: !QualityScore
  , qrFormatScores :: !(HashMap ContentType QualityScore)
  , qrValidationResults :: !(HashMap ContentType ValidationResult)
  , qrRecommendations :: ![Text]
  , qrProcessingTime :: !Double
  , qrGeneratedAt :: !UTCTime
  } deriving (Show, Eq)