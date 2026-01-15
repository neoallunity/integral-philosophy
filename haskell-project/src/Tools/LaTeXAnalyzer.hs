{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LaTeX analyzer tool
module Tools.LaTeXAnalyzer where

import IntegralPhilosophy.Types
import IntegralPhilosophy.Reconstruction.LaTeX
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (when, forM_, filterM)

-- | Main analyzer function
analyzeLaTeXFiles :: FilePath -> IO ()
analyzeLaTeXFiles dir = do
  putStrLn $ "Analyzing LaTeX files in: " ++ dir
  
  files <- findLaTeXFiles dir
  putStrLn $ "Found " ++ show (length files) ++ " LaTeX files"
  
  forM_ files $ \file -> do
    putStrLn $ "\n--- Analyzing: " ++ file ++ " ---"
    analyzeSingleFile file

-- | Find all LaTeX files recursively
findLaTeXFiles :: FilePath -> IO [FilePath]
findLaTeXFiles dir = do
  exists <- doesFileExist dir
  if exists
    then do
      allFiles <- listDirectory dir
      let texFiles = filter (\f -> takeExtension f == ".tex") allFiles
      return $ map (dir </>) texFiles
    else return []

-- | Analyze single LaTeX file
analyzeSingleFile :: FilePath -> IO ()
analyzeSingleFile file = do
  content <- TIO.readFile file
  
  let sections = extractSections content
  let math = extractMathContent content
  let metadata = extractMetadata content
  
  putStrLn $ "Sections: " ++ show (length sections)
  putStrLn $ "Math expressions: " ++ show (length math)
  putStrLn $ "Language: " ++ T.unpack (metaLanguage metadata)
  putStrLn $ "Words: " ++ show (metaWordCount metadata)
  putStrLn $ "Characters: " ++ show (metaCharCount metadata)
  
  when (not (null sections)) $ do
    putStrLn "\nSections found:"
    mapM_ printSection (take 5 sections)
    when (length sections > 5) $
      putStrLn $ "... and " ++ show (length sections - 5) ++ " more"
  
  when (not (null math)) $ do
    putStrLn "\nMath expressions (first 3):"
    mapM_ printMath (take 3 math)
    when (length math > 3) $
      putStrLn $ "... and " ++ show (length math - 3) ++ " more"

-- | Print section information
printSection :: Section -> IO ()
printSection sec = putStrLn $ 
  "  " ++ replicate (secLevel sec) '#' ++ " " ++ T.unpack (secTitle sec)

-- | Print math expression
printMath :: MathExpression -> IO ()
printMath = \case
  InlineMath expr -> putStrLn $ "  Inline: " ++ T.unpack expr
  DisplayMath expr -> putStrLn $ "  Display: " ++ T.unpack expr
  Equation expr _ -> putStrLn $ "  Equation: " ++ T.unpack expr

-- | Batch analysis of all LaTeX files in project
batchAnalyze :: IO ()
batchAnalyze = do
  putStrLn "=== Integral Philosophy LaTeX Analysis ==="
  
  -- Analyze main directories
  analyzeLaTeXFiles "."
  analyzeLaTeXFiles "chapters"
  analyzeLaTeXFiles "frontmatter"
  analyzeLaTeXFiles "articles"
  analyzeLaTeXFiles "cfg"
  
  -- Generate summary
  putStrLn "\n=== Summary ==="
  putStrLn "Analysis complete. See above for details."

-- | Entry point when run as standalone
main :: IO ()
main = batchAnalyze