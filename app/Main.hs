{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where
  
import Processing.Process
import Database.Database
import Text.Read (readMaybe)
import Control.Monad (unless)
import System.Exit (exitSuccess)
import Data.List (sortBy)
import Data.Function (on)
import Web.Browser (openBrowser)
import Processing.Types (Time)

main :: IO ()
main = do
  putStrLn "Welcome to Shazam"
  putStrLn "Choose action:"
  putStrLn "1. Recognize song"
  putStrLn "2. Add song to library"
  putStrLn "3. Show library"
  putStrLn "4. Exit"

  -- Initialize Database
  initDB
  
  mainLoop

mainLoop :: IO ()
mainLoop = do
  putStrLn "\nEnter your choice (1-4):"
  choice <- getLine
 
  case readMaybe choice of
    Just 1 -> recognizeSong
    Just 2 -> addSongToLibrary
    Just 3 -> showLibrary >> mainLoop
    Just 4 -> putStrLn "Goodbye!" >> exitSuccess
    _ -> putStrLn "Invalid choice! Please enter 1-4." >> mainLoop


recognizeSong :: IO ()
recognizeSong = do
  putStrLn "Please specify file path:"
  filepath <- getLine
  openAndProcessFile (\fingerprints -> searchSong fingerprints >>= printResult) filepath
  -- putStrLn "Listening for 30 seconds..."
  -- openAndProcessMicrophone (\fingerprints -> searchSong fingerprints >>= printResult)
  mainLoop


addSongToLibrary :: IO ()
addSongToLibrary = do
  putStrLn "Please specify file path:"
  filepath <- getLine
  putStrLn "Please specify song's name:"
  name <- getLine
  putStrLn "Please specify artist:"
  artist <- getLine
  putStrLn "Please specify album (press Enter to skip):"
  album <- getLine
  putStrLn "Please specify YouTube link (press Enter to skip):"
  link <- getLine
  
  putStrLn "Processing audio file..."
  openAndProcessFile (\fingerprints -> addSong name artist album link fingerprints >> putStrLn "Song added successfully!") 
    filepath
  mainLoop

openPage :: String -> Time -> IO ()
openPage link time = do
  if not (null link)
    then do 
      print (time)
      result <- openBrowser (link ++ "&t=" ++ show m ++ "m" ++ show s ++ "s")
      return ()
    else return ()
  where
    m = (time `div` 1000) `div` 60
    s = time `div` 1000 - m * 60


printResult :: SearchResult -> IO ()
printResult (SearchResult bestMatch allMatches) = do
    case bestMatch of
        Nothing -> putStrLn "No strong match found (need at least 5 matching points)"
        Just (song, fm) -> do
          putStrLn $ "Best match: " ++ formatSong song
          openPage (link song) fm
          
    
    putStrLn "\nAll potential matches:"
    if null allMatches
        then putStrLn "  No matches found"
        else mapM_ printMatch (sortBy (flip compare `on` snd) allMatches)
  where
    formatSong :: SongInfo -> String
    formatSong song = 
        title song ++ " by " ++ artist song ++ 
        " (Album: " ++ album song ++ ")" ++
        if null (link song) then "" else "\n  Link: " ++ link song
    
    
    printMatch :: (SongInfo, Int) -> IO ()
    printMatch (song, count) = 
        putStrLn $ "  " ++ show count ++ " matches: " ++ 
                  title song ++ " - " ++ artist song