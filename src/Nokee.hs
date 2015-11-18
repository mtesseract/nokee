-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

{-# LANGUAGE OverloadedStrings #-}

module Nokee where

import Control.Monad
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Time.Clock
import Database.SQLite.Simple
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Text.Regex.TDFA
import Utilities

baseDirectory :: String
baseDirectory = ".nokee"

storeDirectory :: String
storeDirectory = baseDirectory </> "stores"

storeSuffix :: String
storeSuffix = ".db"

sqliteExecutable :: String
sqliteExecutable = "sqlite3"

sqliteStoreTableDef :: String
sqliteStoreTableDef = "CREATE TABLE NOTES(ID INTEGER PRIMARY KEY, \
                                         \TITLE TEXT, BODY TEXT, \
                                         \CTIME DATETIME DEFAULT CURRENT_TIMESTAMP, \
                                         \MTIME DATETIME DEFAULT CURRENT_TIMESTAMP);\
                      \CREATE TABLE TAGS(ID INTEGER PRIMARY KEY,\
                                        \NAME TEXT);\
                      \CREATE TABLE NOTETAGS(NOTE INTEGER,\
                                            \TAG INTEGER);"

tmpfileTemplate :: String
tmpfileTemplate = "nokee.txt"

nokeeStringDelimiter :: String
nokeeStringDelimiter = ""

defaultEditor :: String
defaultEditor = "emacs"

editor :: String -> IO ExitCode
editor = execEditor defaultEditor

type Tag = String

data Note = Note { noteID    :: Maybe NoteID
                 , noteTitle :: String
                 , noteTags  :: [Tag]
                 , noteBody  :: String
                 , noteCTime :: Maybe UTCTime
                 , noteMTime :: Maybe UTCTime
                 } deriving (Show)

data DBNote = DBNote { dbnoteID    :: NoteID
                     , dbnoteTitle :: String
                     , dbnoteBody  :: String
                     , dbnoteCTime :: UTCTime
                     , dbnoteMTime :: UTCTime
                     } deriving (Show)

type StoreName = String
type NoteID = Integer
type TagID = Integer
type StoreHandle = Connection

instance FromRow DBNote where
  fromRow = DBNote <$> field <*> field <*> field <*> field <*> field

-- To be called from within an transaction.
retrieveTags :: StoreHandle -> NoteID -> IO [Tag]
retrieveTags storeHandle nId = do
  noteTagIds <- query storeHandle "SELECT * FROM NOTETAGS WHERE NOTE=?;"
                  (Only nId) :: IO [(Integer, Integer)]
  let tagIds = map snd noteTagIds
  liftM catMaybes (mapM (lookupTagName storeHandle) tagIds)

lookupTagName :: StoreHandle -> TagID -> IO (Maybe String)
lookupTagName storeHandle tId = do
  rows <- query storeHandle "SELECT NAME FROM TAGS WHERE ID=?;"
            (Only tId) :: IO [[String]]
  return $ head <$> listToMaybe rows -- FIXME (head)?

dbnoteRetrieve :: StoreHandle -> NoteID -> IO (Maybe DBNote)
dbnoteRetrieve storeHandle nId = do
  rows <- query storeHandle "SELECT * FROM NOTES WHERE ID = ?;" (Only nId) :: IO [DBNote]
  return (listToMaybe rows)

noteRetrieve :: StoreHandle -> NoteID -> IO (Maybe Note)
noteRetrieve storeHandle nId = do
  maybeDBNote <- dbnoteRetrieve storeHandle nId
  case maybeDBNote of
    Just dbNote -> do tags <- retrieveTags storeHandle nId
                      return $ Just Note { noteID    = Just (dbnoteID dbNote)
                                         , noteTitle = dbnoteTitle dbNote
                                         , noteTags  = tags
                                         , noteBody  = dbnoteBody dbNote
                                         , noteCTime = Just (dbnoteCTime dbNote)
                                         , noteMTime = Just (dbnoteMTime dbNote) }
    _ -> return Nothing

-- To be called within a transaction.
tagsAdd :: StoreHandle -> [Tag] -> IO ()
tagsAdd storeHandle = mapM_ tagAdd
  where tagAdd tag = execute storeHandle "REPLACE INTO TAGS (ID, NAME) VALUES\
                                         \ ((SELECT ID FROM TAGS WHERE NAME=?), ?)"
                       (tag, tag)

-- To be called within a transaction.
noteTagsAdd :: StoreHandle -> NoteID -> [Tag] -> IO ()
noteTagsAdd storeHandle nId = mapM_ noteTagAdd
  where noteTagAdd :: Tag -> IO ()
        noteTagAdd tag =
          execute storeHandle "INSERT INTO NOTETAGS (NOTE, TAG) VALUES \
                              \ (?, (SELECT ID FROM TAGS WHERE NAME=?));" (nId, tag)

noteAdd :: Note -> StoreHandle -> IO ()
noteAdd note storeHandle = do
  execute storeHandle "INSERT INTO NOTES (TITLE, BODY) VALUES (?, ?);"
    (noteTitle note, noteBody note)
  noteId <- lastInsertRowId storeHandle
  tagsAdd storeHandle (noteTags note)
  noteTagsAdd storeHandle (toInteger noteId) (noteTags note)

noteUpdate :: StoreHandle -> Note -> IO ()
noteUpdate storeHandle note = do
  -- FIXME: Exception, if no noteID!
  execute storeHandle "UPDATE NOTES SET TITLE=?, BODY=?, MTIME=CURRENT_TIMESTAMP WHERE ID=?;"
    (noteTitle note, noteBody note, noteID note)
  tagsAdd storeHandle (noteTags note)
  updateTags (fromJust (noteID note)) (noteTags note)

  where updateTags :: NoteID -> [Tag] -> IO ()
        updateTags nId tags = do
          execute storeHandle "DELETE FROM NOTETAGS WHERE NOTE=?;" (Only nId)
          noteTagsAdd storeHandle nId tags

storeFileName :: String -> IO String
storeFileName storeName = do
  storeDir <- storeDirectoryName
  return $ storeDir </> storeName ++ storeSuffix

storeDirectoryName :: IO String
storeDirectoryName = do
  home <- getEnv "HOME"
  return $ home </> storeDirectory

noteStoreInitialize :: String -> IO ()
noteStoreInitialize storeFile = do
  _ <- execProcess sqliteExecutable [storeFile] sqliteStoreTableDef -- FIXME, error checking
  return ()

noteSearch :: String -> StoreHandle -> IO [Note]
noteSearch pattern storeHandle = do
  let pattern' = "%" ++ pattern ++ "%"
  noteList storeHandle (Just pattern')

noteToString :: Note -> String
noteToString note =
  noteTitle note ++ "\n"
    ++ packTags (noteTags note) ++ "\n"
    ++ nokeeStringDelimiter ++ "\n"
    ++ (unlines . lines . noteBody) note

packTags :: [Tag] -> String
packTags = intercalate ", "
    
stringToNote :: String -> Maybe Note
stringToNote string =
  let stringLines = lines string
  in case stringLines of
       title:tags:line2:body -> if line2 == nokeeStringDelimiter
                                   then Just Note { noteTitle = title
                                                  , noteTags = unpackTags tags
                                                  , noteBody = unlines body
                                                  , noteCTime = Nothing
                                                  , noteMTime = Nothing
                                                  , noteID = Nothing }
                                   else Nothing
       _ -> Nothing

unpackTags :: String -> [Tag]
unpackTags = filter (/= "") . map strip . split ","

cmdNoteAdd :: StoreHandle -> IO ()
cmdNoteAdd storeHandle =
  withSystemTempFile tmpfileTemplate
    (\ filename handle -> do
        storeEmpty <- isEmptyStore storeHandle
        when storeEmpty $
          hPutStr handle (noteToString helperNote)
        hClose handle
        cmdNoteAdd' filename)

  where cmdNoteAdd' filename = do
          _ <- editor filename
          -- FIXME: exception.
          contents <- readFile filename
          if (null . strip) contents
             then putStrLn "Empty note file, aborting."
             else case stringToNote contents of
                    Just note -> noteAdd note storeHandle
                    Nothing -> cmdNoteAdd' filename

helperNote :: Note
helperNote = Note { noteID = Nothing
                  , noteTitle = "Here you have to insert the title of the note"
                  , noteCTime = Nothing
                  , noteMTime = Nothing
                  , noteTags = ["firsttag", "anotherTag" ]
                  , noteBody = "Welcome to Nokee! It seems you are adding your first note to this store.\n\
                               \This note, which you are free to edit, serves as a guiding example for the\n\
                               \format used and expected by Nokee.\n\
                               \\n\
                               \The first line is expected to contain the title of your note. Every note has\n\
                               \to have a title (although it may be empty). The second line contains a\n\
                               \comma-seperated list of tags, which should be associated with the note. This\n\
                               \list may, of course, be empty. The third line serves as a delimiter line\n\
                               \between the note head (consisting of title and tags) and the note body; this\n\
                               \line must be empty. The text body of the note follows directly after this\n\
                               \empty line.\n\n\
                               \Happy Hacking!" }

isEmptyStore :: StoreHandle -> IO Bool
isEmptyStore storeHandle = do
  rows <- query_ storeHandle "SELECT ID FROM NOTES;" :: IO [[Integer]]
  return $ null rows

cmdNoteRetrieve :: NoteID -> StoreHandle -> IO ()
cmdNoteRetrieve nId storeHandle = do
  maybeNote <- noteRetrieve storeHandle nId
  case maybeNote of
    Just note -> putStr (noteToString note)
    Nothing -> putStr "Note not found."

cmdNoteEdit :: NoteID -> StoreHandle -> IO ()
cmdNoteEdit nId storeHandle =
  withSystemTempFile tmpfileTemplate
    (\ filename handle -> do
        maybeNote <- noteRetrieve storeHandle nId
        case maybeNote of
          Just note -> do hPutStr handle (noteToString note)
                          hClose handle -- from now on we only work with the path.
                          cmdNoteUpdate' filename
          Nothing -> putStrLn "Note not found.") -- exception?

  where cmdNoteUpdate' filename = do
          _ <- editor filename
          -- FIXME: exception.
          contents <- readFile filename
          case stringToNote contents of
            Just note -> noteUpdate storeHandle (note { noteID = Just nId })
            Nothing -> cmdNoteUpdate' filename

noteList :: StoreHandle -> Maybe String -> IO [Note]
noteList storeHandle maybePattern = do
  dbnotes <- case maybePattern of
               Just p -> query storeHandle "SELECT * FROM NOTES WHERE BODY LIKE ?;" (Only p) :: IO [DBNote]
               _      -> query_ storeHandle "SELECT ID, TITLE, BODY, CTIME, MTIME FROM NOTES;" :: IO [DBNote]
  mapM prepareNote dbnotes
  where prepareNote :: DBNote -> IO Note
        prepareNote dbnote = do
          tags <- retrieveTags storeHandle (dbnoteID dbnote)
          return Note { noteID = Just (dbnoteID dbnote)
                      , noteTitle = dbnoteTitle dbnote
                      , noteBody = dbnoteBody dbnote
                      , noteTags = tags
                      , noteCTime = Just (dbnoteCTime dbnote)
                      , noteMTime = Just (dbnoteMTime dbnote) }

filterByTags :: [Tag] -> Note -> Bool
filterByTags tags note = not $ null (listIntersection tags (noteTags note))

cmdNoteList :: String -> StoreHandle -> IO ()
cmdNoteList tagsStr storeHandle = do
  let tags = unpackTags tagsStr
  notes <- noteList storeHandle Nothing
  let notes' = if null tags
               then notes
               else filter (filterByTags tags) notes
  mapM_ (putStrLn . notePrintSummary) notes'

cmdNoteListStores :: IO ()
cmdNoteListStores = do
  storeDir <- storeDirectoryName
  allFiles <- getDirectoryContents storeDir
  let storeFiles = filter (\ name -> name /= "." && name /= "..") allFiles
      storeNames = mapMaybe removeSuffix storeFiles
  mapM_ putStrLn storeNames

  where removeSuffix :: String -> Maybe String
        removeSuffix s = let pattern = "^(.+).db$" :: String
                             matches :: [[String]]
                             matches = s =~ pattern
                         in case matches of
                              ([_, name]:_) -> Just name
                              _ -> Nothing

cmdNoteListTags :: StoreHandle -> IO ()
cmdNoteListTags storeHandle =
  retrieveReferencedTags storeHandle >>= mapM_ putStrLn

retrieveReferencedTags :: StoreHandle -> IO [Tag]
retrieveReferencedTags storeHandle = do
  tags <- query_ storeHandle "SELECT TAGS.NAME FROM TAGS INNER JOIN NOTETAGS \
                             \ON TAGS.ID = NOTETAGS.TAG;" :: IO [[String]]
  return $ nubSort (map head tags)

notePrintSummary :: Note -> String
notePrintSummary note =
  let nId   = maybe "0" show (noteID note)
      cTime = maybe "" show (noteCTime note)
  in nId ++ " " ++ cTime ++ " " ++ noteTitle note

cmdNoteSearch :: String -> StoreHandle -> IO ()
cmdNoteSearch pattern storeHandle = do
  notes <- noteSearch pattern storeHandle
  mapM_ (putStrLn . notePrintSummary) notes

cmdNoteInit :: String -> IO ()
cmdNoteInit storeName = do
  storeFile <- storeFileName storeName
  let storeDir = takeDirectory storeFile
  createDirectoryIfMissing True storeDir
  noteStoreInitialize storeFile

noteDelete :: NoteID -> StoreHandle -> IO ()
noteDelete nId storeHandle = do
  execute storeHandle "DELETE FROM NOTES WHERE ID=?;" (Only nId)
  execute storeHandle "DELETE FROM NOTETAGS WHERE NOTE=?;" (Only nId)

cmdNoteDelete :: NoteID -> StoreHandle -> IO ()
cmdNoteDelete = noteDelete

-- Wraps the IO action in a transaction.
withStore :: StoreName -> (StoreHandle -> IO a) -> IO a
withStore storeName f = do
  -- FIXME, what about exceptions?
  storeFile <- storeFileName storeName
  storeHandle <- open storeFile
  withTransaction storeHandle (f storeHandle)
