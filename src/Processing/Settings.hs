module Processing.Settings where

import Processing.Types (Frequency)

-- sample frequency
fs :: Frequency
fs = 44100

-- cut off frequency of input signal
fsDown :: Frequency
fsDown = 5000

-- size of each block (chunck) in bytes
-- so in each chunck then 2048 points
-- since each points is Int16
chunkSize :: Int
chunkSize = 4096