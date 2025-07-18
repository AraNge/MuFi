module Processing.Parse (getMatrix) where

import qualified Data.ByteString as BS
import System.IO (Handle)
import Data.Int (Int16)
import Data.Binary.Get ( Get, runGet, isEmpty, getInt16le )  
import Processing.ChooseBands (chooseBands)
import Processing.FingerPrints (createFingerPrints)
import Processing.Types (Frequency)
import Processing.Settings (chunkSize)


-- Transform PCM into [Int16].
-- Last byte is ignored if PCM has odd number of bytes
parsePCM16le :: BS.ByteString -> [Int16]
parsePCM16le bs = runGet go bsLazy
  where
    bsLazy = BS.fromStrict bs

    go :: Get [Int16]
    go = do
      empty <- isEmpty
      if empty
        then return []
        else do
          -- read 16-bit int little-endian
          samp <- getInt16le
          rest <- go
          return (samp : rest)


getMatrix :: Handle -> IO [[Frequency]]
getMatrix h = do   
  bs <- BS.hGet h chunkSize
  if BS.null bs
    then return []
    else do
      let samples = map fromIntegral ( parsePCM16le bs )
      -- putStrLn $ "Read " ++ show (length samples) ++ " samples (first 3: " ++ show (take 3 samples) ++ ")"
      rest <- getMatrix h
      bands <- chooseBands samples -- get only frequncies in ranges
      return (bands : rest)