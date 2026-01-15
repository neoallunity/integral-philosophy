{-# LANGUAGE OverloadedStrings #-}

module IntegralPhilosophy.Prelude where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Char as Char
import qualified System.IO as IO
import qualified Data.Time as Time
import Data.Time (diffUTCTime)

-- | Enhanced error handling for academic publishing
safeReadFile :: FilePath -> IO (Either String Text.Text)
safeReadFile path = Exception.catch
  (Right <$> TextIO.readFile path)
  (\e -> pure $ Left $ "Failed to read " ++ path ++ ": " ++ show (e :: Exception.SomeException))

-- | Safe file writing with backup
safeWriteFile :: FilePath -> Text.Text -> IO (Either String ())
safeWriteFile path content = Exception.catch
  (do
    let backupPath = path ++ ".backup"
    IO.withFile backupPath IO.WriteMode $ \handle -> TextIO.hPutStr handle content
    TextIO.writeFile path content
    pure $ Right ()
  ) (\e -> pure $ Left $ "Failed to write " ++ path ++ ": " ++ show (e :: Exception.SomeException))

-- | Academic publication validation utilities
validateISBN :: Text.Text -> Bool
validateISBN isbn = Text.length isbn == 13 && Text.all Char.isDigit isbn

-- | Enhanced timing utilities for publication processing
measureTime :: IO a -> IO (a, Double)
measureTime action = do
  startTime <- Time.getCurrentTime
  result <- action
  endTime <- Time.getCurrentTime
  pure (result, realToFrac $ Time.diffUTCTime endTime startTime)

-- | Common validation patterns for academic content
containsMathContent :: Text.Text -> Bool
containsMathContent content = 
  "$" `Text.isInfixOf` content || 
  "\\begin{" `Text.isInfixOf` content ||
  "\\(" `Text.isInfixOf` content

-- | Metadata extraction utilities
extractTitle :: Text.Text -> Maybe Text.Text
extractTitle content = 
  case Text.lines content of
    [] -> Nothing
    (firstLine:_) -> Just $ Text.strip firstLine

-- | LaTeX structure detection
detectLatexStructure :: Text.Text -> LatexStructure
detectLatexStructure content
  | "\\documentclass" `Text.isInfixOf` content = FullDocument
  | "\\begin{" `Text.isInfixOf` content = Fragment
  | otherwise = PlainText

-- | Data type for LaTeX structure classification
data LatexStructure = FullDocument | Fragment | PlainText
  deriving (Show, Eq, Ord)

-- | Core validation result type
data ValidationResult = ValidationResult
  { valid :: Bool
  , errors :: [Text.Text]
  , warnings :: [Text.Text]
  , metadata :: [(Text.Text, Text.Text)]
  } deriving (Show, Eq)

-- | Monoid instance for combining validation results
instance Semigroup ValidationResult where
  (ValidationResult v1 e1 w1 m1) <> (ValidationResult v2 e2 w2 m2) =
    ValidationResult (v1 && v2) (e1 ++ e2) (w1 ++ w2) (m1 ++ m2)

instance Monoid ValidationResult where
  mempty = ValidationResult True [] [] []

-- | Success constructor for validation
validationSuccess :: ValidationResult
validationSuccess = ValidationResult True [] [] []

-- | Error constructor for validation
validationError :: [Text.Text] -> ValidationResult
validationError errs = ValidationResult False errs [] []

-- | Warning constructor for validation
validationWarning :: [Text.Text] -> ValidationResult
validationWarning warns = ValidationResult True [] warns []

-- | Enhanced result type for complex operations
data Result a = Success a | Failure Text.Text [Text.Text]
  deriving (Show, Eq, Functor)

-- | Utility functions for Result type
isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Failure _ _) = False

getSuccess :: Result a -> Maybe a
getSuccess (Success val) = Just val
getSuccess (Failure _ _) = Nothing

getErrors :: Result a -> [Text.Text]
getErrors (Success _) = []
getErrors (Failure err _) = [err]

getWarnings :: Result a -> [Text.Text]
getWarnings (Success _) = []
getWarnings (Failure _ warns) = warns

-- | Academic publishing constants
standardPaperSize :: (Int, Int)
standardPaperSize = (210, 297) -- A4 in mm

standardFontSize :: Int
standardFontSize = 12

maxAbstractLength :: Int
maxAbstractLength = 300

-- | Common LaTeX environments for academic papers
academicEnvironments :: [Text.Text]
academicEnvironments = 
  [ "abstract", "keywords", "acknowledgments"
  , "theorem", "lemma", "proposition", "corollary"
  , "proof", "definition", "example", "remark"
  , "figure", "table", "algorithm", "equation"
  ]

-- | Essential LaTeX packages for academic publishing
essentialPackages :: [Text.Text]
essentialPackages = 
  [ "inputenc", "fontenc", "graphicx", "amsmath"
  , "amssymb", "amsfonts", "hyperref", "geometry"
  , "babel", "times", "natbib", "url"
  ]

-- | Validation helpers
hasRequiredPackages :: [Text.Text] -> Bool
hasRequiredPackages used = all (`elem` used) essentialPackages

hasValidStructure :: LatexStructure -> Bool
hasValidStructure FullDocument = True
hasValidStructure Fragment = True
hasValidStructure PlainText = True

-- | Performance monitoring utilities
timeExecution :: String -> IO a -> IO a
timeExecution description action = do
  (result, duration) <- measureTime action
  putStrLn $ description ++ " took " ++ show duration ++ " seconds"
  return result

-- | File processing utilities with error handling
processFile :: (Text.Text -> Result a) -> FilePath -> IO (Result a)
processFile processor path = do
  result <- safeReadFile path
  case result of
    Left err -> pure $ Failure (Text.pack err) []
    Right content -> pure $ processor content

-- | Batch processing utilities
processFiles :: (Text.Text -> Result a) -> [FilePath] -> IO [Result a]
processFiles processor paths = mapM (processFile processor) paths

-- | Academic metadata extraction
extractMetadata :: Text.Text -> [(Text.Text, Text.Text)]
extractMetadata content = 
  let lines' = Text.lines content
      metadataLines = filter (\line -> 
        "\\title" `Text.isPrefixOf` line ||
        "\\author" `Text.isPrefixOf` line ||
        "\\date" `Text.isPrefixOf` line) lines'
   in map parseMetadataLine metadataLines
  where
    parseMetadataLine line = 
      let (key, value) = Text.breakOn "{" line
          cleanValue = Text.drop 1 $ Text.init value
       in (Text.tail key, cleanValue)

