module Processing.Types where

type Hash = Int
type Time = Int                  -- milliseconds
type Frequency = Int
type Peak = (Frequency, Power)   -- peak type for most intensice frequency in range and its power
type Power = Int

data Fingerprint = Fingerprint {
    fpHash :: Hash,
    fpTime :: Time
} deriving (Show, Eq)