-- | Time calculation utilities for academic publishing
module IntegralPhilosophy.Utils.Time
  ( calculateTime
  ) where

import Data.Time (UTCTime, diffUTCTime)

-- | Calculate time difference between two UTCTime values
calculateTime :: UTCTime -> UTCTime -> Double
calculateTime start end = realToFrac $ diffUTCTime end start