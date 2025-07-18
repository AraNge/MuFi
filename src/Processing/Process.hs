module Processing.Process (
    openAndProcessFile,
    openAndProcessMicrophone
) where

import System.Process (createProcess, proc, std_out, waitForProcess)
import System.Process( StdStream( CreatePipe ) )
import System.IO (hSetBinaryMode, hClose, Handle)
import Processing.Parse (getMatrix)
import Processing.Types (Hash, Time, Fingerprint)
import Processing.FingerPrints (createFingerPrints)
import System.FilePath ((</>))


ffmpegPath = "audioProcessing" </> "bin" </> "ffmpeg.exe"

processAudio :: Handle -> ([Fingerprint] -> IO ()) -> IO ()
processAudio handle callback = do
    hSetBinaryMode handle True
    matrix <- getMatrix handle
    let fingerprints = createFingerPrints matrix
    callback fingerprints
    hClose handle

openAndProcessFile :: ([Fingerprint] -> IO ()) -> FilePath -> IO ()
openAndProcessFile callback filepath = do
    let args = ["-i", filepath, "-ar", "44100", "-ac", "1", "-f", "s16le", "-"]
    (_, Just stdout, _, phandle) <- createProcess (proc ffmpegPath args) { std_out = CreatePipe }
    processAudio stdout callback
    _ <- waitForProcess phandle
    return ()

openAndProcessMicrophone :: ([Fingerprint] -> IO ()) -> IO ()
openAndProcessMicrophone callback = do
    let args = ["-loglevel", "quiet",
            "-f", "dshow",
            "-i", "audio=Microphone (High Definition Audio Device)",
            "-ar", "44100",
            "-ac", "1",
            "-f", "s16le",
            "-acodec", "pcm_s16le",
            "-af", "volume=1.0",
            "-t", "30",
            "-"]
    (_, Just stdout, _, phandle) <- createProcess (proc ffmpegPath args) { std_out = CreatePipe }
    processAudio stdout callback
    _ <- waitForProcess phandle
    return ()