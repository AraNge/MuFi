{-# LANGUAGE FlexibleContexts #-}

module Processing.FingerPrints (
    createFingerPrints
) where

import Processing.Types (Frequency, Fingerprint(..), fpHash, fpTime)
import Processing.Settings (chunkSize, fs)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Processing.Types (Hash)

-- hash function
hash :: Frequency -> Frequency -> Frequency -> Frequency -> Hash
hash p1 p2 p3 p4 = (p4 - (p4 `mod` fuz_factor)) * 100000000 +
                  (p3 - (p3 `mod` fuz_factor)) * 100000 +
                  (p2 - (p2 `mod` fuz_factor)) * 100 +
                  (p1 - (p1 `mod` fuz_factor))
  where
    fuz_factor = 2

-- Create fingerprints from frequency peaks
createFingerPrints :: [[Frequency]] -> [Fingerprint]
createFingerPrints peaks = go peaks 0
  where
    go [] _ = []
    go ([p1, p2, p3, p4]:rest) i = fp : go rest (i+1)
      where
        fp = Fingerprint {
          fpHash = hash p1 p2 p3 p4,
          fpTime = round (fromIntegral (i*chunkSize) / (fromIntegral fs) * 1000 / 2)
        }
    go (_:rest) i = go rest (i+1)  -- Handle cases with != 4 peaks