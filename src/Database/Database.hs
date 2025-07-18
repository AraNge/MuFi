{-# LANGUAGE OverloadedStrings #-}

module Database.Database (
    initDB,
    addSong,
    searchSong,
    showLibrary,
    SongInfo,
    SearchResult,
    SearchResult(..),
    title,
    artist,
    album,
    link
) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad (mapM_, forM)
import Data.List (groupBy, maximumBy, sortBy, intercalate, sort, sortOn, group, minimumBy)
import Data.Ord (comparing, Down(..))
import Data.String (fromString)
import Processing.Types (Fingerprint(..), fpHash, fpTime, Time, Hash)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map



instance FromRow SongInfo where
    fromRow = SongInfo <$> field <*> field <*> field <*> field <*> field

-- Initialize database tables
initDB :: IO ()
initDB = do
    conn <- open "music.db"
    -- Create songs table if it doesn't exist
    execute_ conn "CREATE TABLE IF NOT EXISTS songs (\
                 \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                 \name TEXT NOT NULL,\
                 \artist TEXT NOT NULL,\
                 \album TEXT,\
                 \link TEXT)"
    -- Create fingerprints table if it doesn't exist
    execute_ conn "CREATE TABLE IF NOT EXISTS fingerprints (\
                 \hash INTEGER NOT NULL,\
                 \time_offset INTEGER NOT NULL,\
                 \song_id INTEGER NOT NULL,\
                 \FOREIGN KEY(song_id) REFERENCES songs(id),\
                 \PRIMARY KEY (hash, time_offset, song_id))"
    close conn

-- Add song to database
addSong :: String -> String -> String -> String -> [Fingerprint] -> IO ()
addSong name artist album link fingerprints = do
    conn <- open "music.db"

    -- Debug print before insertion
    putStrLn $ "Adding song: " ++ name ++ " with " ++ show (length fingerprints) ++ " fingerprints"
    
    -- Add song metadata
    execute conn "INSERT INTO songs (name, artist, album, link) VALUES (?,?,?,?)" 
        (name, artist, album, link)
    
    -- Get inserted song ID (with explicit type annotation)
    [[songId]] <- query conn "SELECT last_insert_rowid()" () :: IO [[Int]]
    
    -- Add fingerprints in batches
    executeMany conn "INSERT INTO fingerprints (hash, time_offset, song_id) VALUES (?,?,?)"
        (map (\fs -> (fpHash fs, fpTime fs, songId)) fingerprints)
    
    putStrLn $ "Added song: " ++ name ++ " (" ++ show (length fingerprints) ++ " fingerprints)"
    close conn


data SongInfo = SongInfo {
    songId :: Int,
    title :: String,
    artist :: String,
    album :: String,
    link :: String
} deriving (Show)


-- Internal data type for database matches
data Match = Match
  { mSongId :: Int      -- Song ID from database
  , mDbTime :: Int      -- Time offset in the song
  , mHash :: Int        -- Hash value
  , mName :: String     -- Song name
  , mArtist :: String   -- Artist name
  } deriving (Show)


instance FromRow Match where
    fromRow = Match <$> field <*> field <*> field <*> field <*> field

-- Result of the search
-- Song and number of matches
data SearchResult = SearchResult
    { bestMatch :: Maybe (SongInfo, Time)
    , allMatches :: [(SongInfo, Int)]
    } deriving (Show)


-- Function to print first 3 fingerprints
-- Just debugging info
printFirst3Fingerprints :: [Fingerprint] -> IO ()
printFirst3Fingerprints fingerprints = do
    putStrLn "First 3 fingerprints:"
    mapM_ print (take 3 fingerprints)

-- Search for a song using its fingerprints
searchSong :: [Fingerprint] -> IO SearchResult
searchSong fingerprints
  | null fingerprints = return $ SearchResult Nothing []
  | otherwise = do
        conn <- open "music.db"
        
        let hashes = map fpHash fingerprints
            placeholders = intercalate "," (replicate (length hashes) "?")
            sql = "SELECT f.song_id, f.time_offset, f.hash, s.name, s.artist " ++
                  "FROM fingerprints f " ++
                  "JOIN songs s ON f.song_id = s.id " ++
                  "WHERE f.hash IN (" ++ placeholders ++ ")"
        
        -- Execute query and get matches
        matches <- query conn (fromString sql) (toRow hashes)

        -- Group matches by song ID
        let songMatches = Map.toList $ Map.fromListWith (++) [(mSongId m, [(mHash m, mDbTime m)]) | m <- matches]
        
        -- Count matches for each song
        let matchPairs = map (\(sid, dbHashes) -> (sid, countMatches fingerprints dbHashes)) songMatches
        -- Sort songs by match count (descending)
        let sortedSongs = sortBy (comparing (Down . snd)) matchPairs
        
        -- Fetch full song info for each match
        allResults <- forM sortedSongs $ \(sid, cnt) -> do
            songs <- query conn 
                "SELECT id, name, artist, album, link FROM songs WHERE id = ?" 
                (Only sid)
            case songs of
                [(id, name, artist, album, link)] -> 
                    return (SongInfo id name artist (fromMaybe "" album) (fromMaybe "" link), cnt)
                _ -> error "Song not found"
        
        -- Determine best match (must have at least 5 matching points)
        let best = case filter (\(_, cnt) -> cnt >= 5) allResults of
                    [] -> Nothing
                    ((song, cnt):_) -> 
                        case lookup (songId song) songMatches of
                            Just dbHashes ->
                                let bestTime = selectBestTime ( map (\x->(fpHash x, fpTime x)) fingerprints ) dbHashes
                                in Just (song, bestTime)
                            Nothing -> Just (song, 0)
    
        close conn
        return $ SearchResult best allResults
    where
        -- Count matching hashes between input and database
        countMatches fps dbHashes =
            let dbMap = Map.fromListWith (++) [(h, [t]) | (h, t) <- dbHashes]
                -- Calculate all time differences between matching hashes
                timeDiffs = [ dbTime - fpTime fp
                             | fp <- fps
                             , dbTime <- Map.findWithDefault [] (fpHash fp) dbMap
                             ]
                timeGroups = Map.fromListWith (+) [(delta, 1) | delta <- timeDiffs]
            in if null timeGroups then 0 else maximum (Map.elems timeGroups)

        -- Find the best matching time offset
        selectBestTime :: [(Hash, Time)] -> [(Hash, Time)] -> Int
        selectBestTime fp db = 
            let dbMap = Map.fromListWith (++) [(h, [t]) | (h, t) <- db]
                -- Calculate time differences between matching hashes
                timeDiffs = [ (t' - t, [(t', t)]) 
                            | (h, t) <- fp
                            , t' <- Map.findWithDefault [] h dbMap
                            ]
                timeGroups = Map.fromListWith (++) timeDiffs
            in case null timeGroups of
                True -> 0  -- No matches found
                _  -> let (bestOffset, times) = maximumBy (comparing snd) (Map.toList timeGroups)
                          (t', t) = head times
                          t_start_in_db = t' - t
                    in t_start_in_db


-- List all songs
showLibrary :: IO ()
showLibrary = do
    conn <- open "music.db"
    songs <- query_ conn "SELECT id, name, artist, album, link FROM songs" :: IO [SongInfo]
    mapM_ print songs
    close conn