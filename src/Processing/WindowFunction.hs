module Processing.WindowFunction (hamming) where


-- Hamming window function
hamming :: Int -> [Double]
hamming n = [ 0.54 - 0.46 * cos (2 * pi * fromIntegral i / (n' - 1)) | i <- [0 .. n-1] ]
  where n' = fromIntegral n