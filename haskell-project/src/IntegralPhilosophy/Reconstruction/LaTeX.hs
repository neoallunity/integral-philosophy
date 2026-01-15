{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LaTeX reconstruction from files to Markdown+TeX
module IntegralPhilosophy.Reconstruction.LaTeX
  ( LaTeXReconstructor(..)
  , ReconstructionConfig(..)
  , reconstructLaTeX
  , extractSections
  , extractMathContent
  , extractMetadata
  ) where

import IntegralPhilosophy.Types
import IntegralPhilosophy.Validation (Validator(..), makeWarning)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.List (isPrefixOf, isInfixOf)

-- | LaTeX reconstructor
data LaTeXReconstructor = LaTeXReconstructor
  { lrConfig :: !ReconstructionConfig
  } deriving (Show, Eq)

-- | Reconstruction configuration
data ReconstructionConfig = ReconstructionConfig
  { rcPreserveMath :: !Bool
  , rcPreserveCitations :: !Bool
  , rcExtractAbstract :: !Bool
  , rcOutputFormat :: !ContentType -- Markdown or TEI
  , rcLanguage :: !Text -- "russian", "english", "both"
  } deriving (Show, Eq)

instance Validator LaTeXReconstructor where
  validate reconstructor doc = liftIO $ reconstructLaTeX reconstructor doc
  validatorName _ = "LaTeX Reconstructor"
  validatorType _ = LaTeX

-- | Reconstruct LaTeX document to Markdown+TeX
reconstructLaTeX :: LaTeXReconstructor -> Document -> IO ValidationResult
reconstructLaTeX reconstructor doc = do
  let content = reconstructDocument reconstructor doc
  errors <- validateReconstructedContent content
  
  pure $ ValidationResult
    { vrErrors = errors
    , vrWarnings = []
    , vrScore = QualityScore 95.0
    , vrProcessingTime = 0.1
    }

-- | Reconstruct document to Markdown format
reconstructDocument :: LaTeXReconstructor -> Document -> Text
reconstructDocument reconstructor doc = T.unlines
  [ "# " <> docTitle doc
  , "**" <> docAuthor doc <> "**"
  , ""
  ] <> 
  (if rcExtractAbstract (lrConfig reconstructor) 
   then "\n## Abstract\n\n" <> docAbstract doc <> "\n\n" 
   else "") <>
  T.unlines (map (reconstructSection reconstructor) (docSections doc)) <>
  (if rcPreserveMath (lrConfig reconstructor)
   then reconstructMathContent (docMathExpressions doc)
   else "")

-- | Reconstruct individual section
reconstructSection :: LaTeXReconstructor -> Section -> Text
reconstructSection _ sec = T.unlines
  [ T.replicate (secLevel sec + 1) "#" <> " " <> secTitle sec
  , ""
  , secContent sec
  , ""
  ] <> T.unlines (map (\s -> reconstructSection (LaTeXReconstructor defaultConfig) s) (secSubsections sec))
  where
    defaultConfig = ReconstructionConfig True True True Markdown "english"

-- | Reconstruct mathematical content
reconstructMathContent :: [MathExpression] -> Text
reconstructMathContent [] = ""
reconstructMathContent math = T.unlines
  [ "## Mathematical Content"
  , ""
  , T.unlines $ map reconstructMathExpr math
  , ""
  ]

-- | Reconstruct individual math expression
reconstructMathExpr :: MathExpression -> Text
reconstructMathExpr = \case
  InlineMath expr -> "$" <> expr <> "$"
  DisplayMath expr -> "$$\n" <> expr <> "\n$$"
  Equation expr labels -> "\\[\n" <> expr <> "\n\\]"

-- | Validate reconstructed content
validateReconstructedContent :: Text -> IO [ValidationError]
validateReconstructedContent content = pure $
  [ makeWarning "reconstruction" "LaTeX content reconstructed to Markdown" "" 0 0
  | T.null content || T.length content < 10
  ]

-- | Extract sections from LaTeX text
extractSections :: Text -> [Section]
extractSections content = 
  let sectionLines = filter (isSectionLine . T.unpack) (T.lines content)
  in map parseSectionLine sectionLines

-- | Check if line is a section
isSectionLine :: String -> Bool
isSectionLine line = any (`isPrefixOf` line) ["\\section", "\\subsection", "\\subsubsection"]

-- | Parse section line
parseSectionLine :: Text -> Section
parseSectionLine line = Section
  { secLevel = countSectionLevel line
  , secTitle = extractSectionTitle line
  , secContent = ""
  , secSubsections = []
  }

-- | Count section level
countSectionLevel :: Text -> Int
countSectionLevel line
  | "\\section" `T.isPrefixOf` line = 1
  | "\\subsection" `T.isPrefixOf` line = 2
  | "\\subsubsection" `T.isPrefixOf` line = 3
  | otherwise = 0

-- | Extract section title
extractSectionTitle :: Text -> Text
extractSectionTitle line = 
  case T.splitOn "{" line of
    (_:title:_) -> T.replace "}" "" title
    _ -> line

-- | Extract math content from text
extractMathContent :: Text -> [MathExpression]
extractMathContent content = 
  let display = extractDisplayMath content
      inline = extractInlineMath content
      equations = extractEquations content
  in display ++ inline ++ equations

-- | Extract display math
extractDisplayMath :: Text -> [MathExpression]
extractDisplayMath content = 
  case T.splitOn "\\\\[" content of
    [_, math, rest] -> 
      case T.splitOn "\\\\]" math of
        [expr, _] -> [DisplayMath (T.strip expr)]
        _ -> []
    _ -> []

-- | Extract inline math
extractInlineMath :: Text -> [MathExpression]
extractInlineMath content = 
  let parts = T.splitOn "$" content
  in case length parts of
    n | n >= 3 -> take (n `div` 2) $ zipWith (\p1 p2 -> InlineMath p1) parts (drop 1 parts)
    _ -> []

-- | Extract equations
extractEquations :: Text -> [MathExpression]
extractEquations content = 
  case T.splitOn "\\begin{equation}" content of
    [_, rest] ->
      case T.splitOn "\\end{equation}" rest of
        [eq, _] -> [Equation (T.strip eq) []]
        _ -> []
    _ -> []

-- | Extract metadata from LaTeX
extractMetadata :: Text -> Metadata
extractMetadata content = Metadata
  { metaLanguage = detectLanguage content
  , metaCreationDate = Nothing
  , metaModificationDate = Nothing
  , metaWordCount = countWords content
  , metaCharCount = T.length content
  , metaCustomFields = mempty
  }

-- | Detect document language
detectLanguage :: Text -> Text
detectLanguage content
  | "\\begin{english}" `T.isInfixOf` content = "english"
  | "\\begin{russian}" `T.isInfixOf` content = "russian"
  | T.any (\c -> c >= 'А' && c <= 'я') content = "russian"
  | otherwise = "english"

-- | Count words in text
countWords :: Text -> Int
countWords = length . T.words . T.replace "\\" " "