module Processing.ChooseBands (chooseBands) where

import Processing.FFT (fft, powerSpectrum)
import Processing.WindowFunction (hamming)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Processing.Types (Frequency, Power)
import Processing.Settings (fs)

-- Frequency ranges for peak detection
frequencyRanges :: [(Frequency, Frequency)]
frequencyRanges = [(40,80), (80,120), (120,180), (180,300)]

-- Select significant peaks from each frequency range
chooseBands :: [Double] -> IO [Frequency]
chooseBands samples = do
    let windowed = zipWith (*) samples (hamming $ length samples)
        spectrum = powerSpectrum (fft windowed) (fromIntegral fs)
    
    return $ map (findStrongestPeak spectrum) frequencyRanges

-- Find strongest peak in a frequency range
findStrongestPeak :: [(Frequency, Power)] -> (Frequency, Frequency) -> Frequency
findStrongestPeak spectrum (low, high) =
    let inRange = filter (\(f,_) -> f >= low && f <= high) spectrum
    in if null inRange 
       then 0 
       else fst $ maximumBy (comparing snd) inRange