module Processing.FFT
  ( fft,
    powerSpectrum
  ) where

import Data.Complex ( Complex(..), cis, magnitude ) 
import Processing.Types (Frequency, Power)


splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd [] = ([], [])
splitEvenOdd [x] = ([x], [])
splitEvenOdd (x:y:xs) = (x : evens, y : odds)
  where (evens, odds) = splitEvenOdd xs


-- Compute power spectrum (frequency, power)
powerSpectrum :: [Complex Double] -> Double -> [(Frequency, Power)]
powerSpectrum fftResult fs = zip freqs power
  where
    n = length fftResult
    numBins = n `div` 2
    -- round frequencies
    freqs = [ round ( fromIntegral i * fs / fromIntegral n ) | i <- [0 .. numBins-1] ]
    -- round magnitudes
    magnitudes = map (round . magnitude) (take numBins fftResult)
    power = map (^2) magnitudes



-- | Compute the Discrete Fourier Transform (DFT) of a real-valued signal.
--fft :: [Double]          -- ^ Input samples
--    -> [Complex Double] -- ^ FFT results
fft :: [Double] -> [Complex Double]
fft [] = []
fft [x] = [x :+ 0]
fft samples = top ++ bottom
    where
        n = length samples
        -- primitive n-th root of unity
        w = cis (- (2 * pi / fromIntegral n))
        -- split into evens and odds
        (evens, odds) = splitEvenOdd samples
        -- recursive FFTs
        ye = fft evens
        yo = fft odds
        -- build the twiddle factors [1, w_n, w_n^2, ...]

        ws = take (n `div` 2) $ iterate (* w) 1

        -- combine even/odd halves

        top = zipWith (+) ye (zipWith (*) (take (n `div` 2) ws) yo)

        bottom = zipWith (-) ye (zipWith (*) (take (n `div` 2) ws) yo)
