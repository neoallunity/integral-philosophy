-- | Simple regex parsing utilities
module IntegralPhilosophy.Utils.Regex where

import qualified Data.Text as T
import Data.Foldable (any)

-- | Simple regex extraction (placeholder implementation)
extractRegex :: T.Text -> T.Text -> [T.Text]
extractRegex pattern text = 
  let parts = T.splitOn pattern text
  in case length parts of
    1 -> []  -- No matches found
    2 -> [T.strip $ parts !! 1]  -- Extract content between pattern
    _ -> []  -- Complex patterns not supported yet

-- | Check if text contains any LaTeX commands
hasInvalidCommands :: T.Text -> Bool
hasInvalidCommands text = 
  let invalidCommands = ["\\", "{", "}", "[", "]"]
  in any (`T.isInfixOf` text) (map T.pack invalidCommands)