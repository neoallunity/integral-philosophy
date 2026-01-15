{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simplified LaTeX validator without complex parsing
module IntegralPhilosophy.Validation.LaTeX
  ( LaTeXValidator(..)
  , LaTeXConfig(..)
  , validateLaTeX
  , extractMathExpressions
  ) where

import IntegralPhilosophy.Types
import IntegralPhilosophy.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Data.Time (getCurrentTime, UTCTime)
import Data.Maybe (mapMaybe)
import System.Process (readProcess)
import Data.List (tails, inits, isPrefixOf, isInfixOf)

-- | LaTeX validator configuration
data LaTeXValidator = LaTeXValidator
  { lvEngine :: FilePath  -- Path to LaTeX engine (pdflatex, xelatex, etc.)
  , lvConfig :: LaTeXConfig
  } deriving (Show, Eq)

-- | LaTeX validation configuration
data LaTeXConfig = LaTeXConfig
  { lcCheckMath :: Bool
  , lcCheckReferences :: Bool
  , lcCheckSyntax :: Bool
  , lcTimeout :: Int  -- seconds
  } deriving (Show, Eq)

-- | Simple LaTeX parser type
type LaTeXParser = String

instance Validator LaTeXValidator where
  validate validator doc = liftIO $ validateLaTeX validator doc
  validatorName _ = "LaTeX Validator"
  validatorType _ = LaTeX

-- | Validate LaTeX document
validateLaTeX :: LaTeXValidator -> Document -> IO ValidationResult
validateLaTeX validator doc = 
  let content = getLaTeXContent doc
      errors = concat
        [ if hasLatexSyntaxIssues content 
          then [makeError Error "latex_syntax" "LaTeX syntax issues detected" "" 0 0]
          else []
        , if hasMissingReferences content
          then [makeError Error "missing_references" "Missing LaTeX references" "" 0 0]
          else []
        ]
      score = calculateLaTeXScore errors content
  
  in return $ ValidationResult errors [] score 0.1

-- | Get LaTeX content from document
getLaTeXContent :: Document -> String
getLaTeXContent doc = T.unpack $ T.unlines $ map secContent (docSections doc)

-- | Check for basic LaTeX syntax issues
hasLatexSyntaxIssues :: String -> Bool
hasLatexSyntaxIssues content = 
  "\\begin{" `isPrefixOf` content && not ("\\end{" `isInfixOf` content)
  || "\\section{" `isPrefixOf` content && not ("\\section" `isPrefixOf` drop 8 content)
  || length (filter (== '\\') content) > length (filter (== '}') content)

-- | Check for missing references
hasMissingReferences :: String -> Bool
hasMissingReferences content = 
  "\\ref{" `isInfixOf` content && not ("\\label{" `isInfixOf` content)

-- | Calculate LaTeX-specific quality score
calculateLaTeXScore :: [ValidationError] -> String -> QualityScore
calculateLaTeXScore errors content = 
  let errorCount = length errors
      contentLength = length content
      errorPenalty = fromIntegral errorCount * 10.0
      lengthPenalty = fromIntegral contentLength / 10000.0 * 0.1
  in QualityScore $ max 0 $ 100.0 - errorPenalty - lengthPenalty

-- | Extract mathematical expressions
extractMathExpressions :: Text -> [MathExpression]
extractMathExpressions content = 
  let displayExprs = extractDisplayMath content
      inlineExprs = extractInlineMath content
  in displayExprs ++ inlineExprs

-- | Extract display math
extractDisplayMath :: Text -> [MathExpression]
extractDisplayMath content = 
  case T.splitOn "\\\\[" content of
    [_, math, _] -> 
      case T.splitOn "\\\\]" math of
        [expr, _] -> [DisplayMath $ T.strip expr]
        _ -> []
    _ -> []

-- | Extract inline math
extractInlineMath :: Text -> [MathExpression]
extractInlineMath content = 
  let parts = T.splitOn "$" content
  in case length parts of
    n | n >= 3 -> take (n `div` 2) $ zipWith (\p1 _ -> InlineMath p1) parts (drop 1 parts)
    _ -> []