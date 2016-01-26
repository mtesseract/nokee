-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Nokee
Description : Module implementing the Nokee core functionality.
Copyright   : (C) 2015-2016 Moritz Schulte
License     : BSD3
Maintainer  : Moritz Schulte <mtesseract@silverratio.net>
Stability   : experimental
Portability : POSIX
-}

module Nokee (StoreName,
              StoreHandle,
              NoteID,
              NokeeException(..),
              cmdNoteAdd,
              cmdNoteDelete,
              cmdNoteRetrieve,
              cmdNoteEdit,
              cmdNoteList,
              cmdNoteListStores,
              cmdNoteListTags,
              cmdNoteSearch,
              cmdNoteInit,
              nokeeRunCommand)
       where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Time.Clock
import Data.Time.Format
import Data.Typeable
import Database.SQLite.Simple
import GHC.Int
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Text.Regex.TDFA
import Utilities

-----------------------------
-- Define Nokee exceptions --
-----------------------------

-- | Exception type used in Nokee.
data NokeeException =
  NokeeExceptionNone            -- ^ Exception value representing no
                                -- exception
  | NokeeExceptionString String -- ^ Exception value holding an error
                                -- message
  deriving (Show, Typeable)

-- | A 'NokeeException' is an 'Exception'.
instance Exception NokeeException

---------------
-- Constants --
---------------

-- | The subdirectory in the calling user's home directory containing
-- Nokees data (potentially configuration data and the data stores).
baseDirectory :: String
baseDirectory = ".nokee"

-- | The subdirectory below 'baseDirectory' in which Nokee expects to
-- find its data stores.
storeDirectory :: String
storeDirectory = baseDirectory </> "stores"

-- | The filename suffix for data store files in 'storeDirectory'.
storeSuffix :: String
storeSuffix = "db"

-- | The name of the SQLite executable to use for data store
-- initialization.
sqliteExecutable :: String
sqliteExecutable = "sqlite3"

-- | The SQL commands used for initializing data stores.
sqliteStoreTableDef :: String
sqliteStoreTableDef = "CREATE TABLE NOTES(ID INTEGER PRIMARY KEY, \
                                         \TITLE TEXT, BODY TEXT, \
                                         \CTIME DATETIME DEFAULT CURRENT_TIMESTAMP, \
                                         \MTIME DATETIME DEFAULT CURRENT_TIMESTAMP);\
                      \CREATE TABLE TAGS(ID INTEGER PRIMARY KEY,\
                                        \NAME TEXT);\
                      \CREATE TABLE NOTETAGS(NOTE INTEGER,\
                                            \TAG INTEGER);"

-- | Template used for temporary files.
tmpfileTemplate :: String
tmpfileTemplate = "nokee.txt"

-- | The delimiter string used in plain text representations of Notes
-- as line no. 3 (after the first line holding the title and the
-- second line holding a list of tags).
nokeeStringDelimiter :: String
nokeeStringDelimiter = ""

-- | The default editor to use in case the environment variable EDITOR
-- is not set.
defaultEditor :: String
defaultEditor = "emacs"

editorEnvironment :: [String]
editorEnvironment = ["NOKEEEDITOR", "EDITOR"]

-- | Opens the specified file in the editor, returning an ExitCode.
editor :: String -> IO ExitCode
editor = execEditor editorSpec
  where editorSpec = EditorSpec editorEnvironment defaultEditor

----------------
-- Data Types --
----------------

-- | A store name is a 'String'.
type StoreName   = String

-- | A note identifier is an 'Integer'.
type NoteID      = Integer

-- | A tag is a 'String'.
type Tag         = String

-- | A TagID is an 'Integer'.
type TagID       = Integer

-- | A store handle is an SQLite 'Connection'.
type StoreHandle = Connection

-- | Datatype contanining a note.
data Note = Note { noteID    :: Maybe NoteID
                 , noteTitle :: String
                 , noteTags  :: [Tag]
                 , noteBody  :: String
                 , noteCTime :: Maybe UTCTime
                 , noteMTime :: Maybe UTCTime
                 } deriving (Show)

-- | A row as contained in the NOTES table.
data DBNote = DBNote { dbnoteID    :: NoteID
                     , dbnoteTitle :: String
                     , dbnoteBody  :: String
                     , dbnoteCTime :: UTCTime
                     , dbnoteMTime :: UTCTime
                     } deriving (Show)

-- | Implement 'FromRow' instance so that 'DBNote's can be retrieved
-- directly from the database.
instance FromRow DBNote where
  fromRow = DBNote <$> field <*> field <*> field <*> field <*> field

------------------------------------------
-- Reader Monad Transformer Abstraction --
------------------------------------------

-- | A computation of type 'NokeeIO a' is a ReaderT reading from an
-- environment of type NokeeEnv and producing a value of type 'a' in
-- the IO monad.
type NokeeIO a = ReaderT NokeeEnv IO a

-- | Definition of the NokeeEnv type. So far it only contains a
-- StoreHandle.
data NokeeEnv = NokeeEnv { nokeeEnvStoreHandle :: StoreHandle }

-- | Wrapper for SQLite's query_ based on NokeeIO.
nokeeQuery_ :: FromRow r => Query -> NokeeIO [r]
nokeeQuery_ quer = do
  conn <- nokeeEnvStoreHandle <$> ask
  liftIO $ query_ conn quer

-- | Wrapper for SQLite's query based on NokeeIO.
nokeeQuery :: (ToRow q, FromRow r) => Query -> q -> NokeeIO [r]
nokeeQuery quer q = do
  conn <- nokeeEnvStoreHandle <$> ask
  liftIO $ query conn quer q

-- | Wrapper for SQLite's execute based on NokeeIO.
nokeeExecute :: ToRow q => Query -> q -> NokeeIO ()
nokeeExecute quer q = do
  conn <- nokeeEnvStoreHandle <$> ask
  liftIO $ execute conn quer q

-- | Wrapper for SQLite's lastInsertRowId based on NokeeIO.
nokeeLastInsertRowId :: NokeeIO GHC.Int.Int64
nokeeLastInsertRowId = do
  conn <- nokeeEnvStoreHandle <$> ask
  liftIO $ lastInsertRowId conn

-- | Wrapper for putStr.
nokeePutStr :: String -> NokeeIO ()
nokeePutStr = liftIO . putStr

-- | Wrapper for putStrLn.
nokeePutStrLn :: String -> NokeeIO ()
nokeePutStrLn = liftIO . putStrLn

--------------------------
-- High Level Interface --
--------------------------

-- | Implementation of the command add: Spawns an editor, lets user edit
-- a new note and then adds the new note to the database.
cmdNoteAdd :: NokeeIO ()
cmdNoteAdd =
  withSystemTempFile tmpfileTemplate
    (\ fName fHandle -> do
        storeEmpty <- isEmptyStore
        liftIO $ do when storeEmpty $
                      hPutStr fHandle (noteToString helperNote)
                    hClose fHandle
        cmdNoteAdd' fName)

  where cmdNoteAdd' :: String -> NokeeIO ()
        cmdNoteAdd' filename = do
          _ <- liftIO $ editor filename
          -- FIXME: exception.
          contents <- liftIO $ readFile filename
          if (null . strip) contents
             then nokeePutStrLn "Empty note file, aborting."
             else case stringToNote contents of
                    Just note -> noteAdd note
                    Nothing -> cmdNoteAdd' filename

        isEmptyStore :: NokeeIO Bool
        isEmptyStore = do
          rows <- nokeeQuery_ "SELECT ID FROM NOTES;" :: NokeeIO [[Integer]]
          return $ null rows

        helperNote :: Note
        helperNote = Note
         { noteID = Nothing
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

-- | Implementation of the command delete: Removes a note given its
-- 'NoteID'.
cmdNoteDelete :: NoteID -> NokeeIO ()
cmdNoteDelete = noteDelete

-- | Implementation of the command retrieve: Retrieves the note with
-- the specified 'NoteID' and displays it on standard out.
cmdNoteRetrieve :: NoteID -> NokeeIO ()
cmdNoteRetrieve nId = do
  maybeNote <- noteRetrieve nId
  case maybeNote of
    Just note -> nokeePutStr (noteToString note)
    Nothing   -> nokeePutStr "Note not found."

-- | Implementation of the command edit: Retrieves the note with the
-- specified 'NoteID' from the database, spawns an editor and lets the
-- user edit the note. After editing, updates the note in the
-- database.
cmdNoteEdit :: NoteID -> NokeeIO ()
cmdNoteEdit nId =
  withSystemTempFile tmpfileTemplate
    (\ filename fHandle -> do
        maybeNote <- noteRetrieve nId
        case maybeNote of
          Just note -> do liftIO $ do hPutStr fHandle (noteToString note)
                                      hClose fHandle -- from now on we only work with the path.
                          cmdNoteUpdate' filename
          Nothing -> nokeePutStrLn "Note not found.") -- exception?

  where cmdNoteUpdate' filename = do
          _ <- liftIO $ editor filename
          -- FIXME: exception.
          contents <- liftIO $ readFile filename
          case stringToNote contents of
            Just note -> noteUpdate (note { noteID = Just nId })
            Nothing -> cmdNoteUpdate' filename

-- | Implementation of the command list: Displays a summary of notes
-- matching the given list of tags or all notes in the store if no
-- tags are specified.
cmdNoteList :: String -> NokeeIO ()
cmdNoteList tagsStr = do
  let tags = unpackTags tagsStr
  notes <- noteList Nothing
  let notes' = if null tags
               then notes
               else filter (filterByTags tags) notes
  nokeePutStr $ notesPrintSummary notes'
  where filterByTags :: [Tag] -> Note -> Bool
        filterByTags tags note = not $ null (listIntersection tags (noteTags note))

-- | Implementation of the command list-stores: Displays all existing
-- stores.
cmdNoteListStores :: IO ()
cmdNoteListStores = do
  storeDir <- storeDirectoryName
  allFiles <- getDirectoryContents storeDir
  let storeFiles = filter filterFiles allFiles
      storeNames = mapMaybe removeSuffix storeFiles
  mapM_ putStrLn storeNames

  where filterFiles :: String -> Bool
        filterFiles fname = fname =~ ("\\." ++ storeSuffix ++ "$") :: Bool

        removeSuffix :: String -> Maybe String
        removeSuffix s = let patt = "^(.+)\\.db$" :: String
                             matches :: [[String]]
                             matches = s =~ patt
                         in case matches of
                              ([_, name]:_) -> Just name
                              _ -> Nothing

-- | Implementation of the command list-tags: Displays a sorted list of
-- (referenced) tags. That is, tags which are contained in the
-- database but not associated with any notes, will not be displayed.
cmdNoteListTags :: NokeeIO ()
cmdNoteListTags = retrieveReferencedTags >>= mapM_ (liftIO . putStrLn)

-- | Implementation of the command search: Displays a summary of the
-- notes which match the specified pattern.
cmdNoteSearch :: String -- ^ The search pattern
              -> NokeeIO ()
cmdNoteSearch patt = do
  notes <- noteSearch patt
  nokeePutStr $ notesPrintSummary notes

-- | Implementation of the command init: Initialize a store with the
-- specified name.
cmdNoteInit :: StoreName -> IO ()
cmdNoteInit storeName = do
  storeFile <- storeFileName storeName
  let storeDir = takeDirectory storeFile
  createDirectoryIfMissing True storeDir
  noteStoreInitialize storeFile

-- | Given a 'NoteID', retrieves the list of tags associated with that
-- note.
retrieveTags :: NoteID -> NokeeIO [Tag]
retrieveTags nId = do
  noteTagIds <- nokeeQuery "SELECT * FROM NOTETAGS WHERE NOTE=?;"
                  (Only nId) :: NokeeIO [(Integer, Integer)]
  let tagIds = map snd noteTagIds
  liftM catMaybes (mapM lookupTagName tagIds)

  where lookupTagName :: TagID -> NokeeIO (Maybe String)
        lookupTagName tId = do
          rows <- nokeeQuery "SELECT NAME FROM TAGS WHERE ID=?;"
                    (Only tId) :: NokeeIO [[String]]
          return $ head <$> listToMaybe rows -- FIXME (head)?

-- | Given a 'NoteID', retrieves the matching row, wrapped in a 'DBNote',
-- from the database.
dbnoteRetrieve :: NoteID -> NokeeIO (Maybe DBNote)
dbnoteRetrieve nId = do
  rows <- nokeeQuery "SELECT * FROM NOTES WHERE ID = ?;"
            (Only nId) :: NokeeIO [DBNote]
  return (listToMaybe rows)

-- | Retrieve a complete 'Note' given its 'NoteID'.
noteRetrieve :: NoteID -> NokeeIO (Maybe Note)
noteRetrieve nId = do
  maybeDBNote <- dbnoteRetrieve nId
  tags <- retrieveTags nId
  let maybeNote = case maybeDBNote of
                    Just dbNote ->
                      Just Note { noteID    = Just (dbnoteID dbNote)
                                , noteTitle = dbnoteTitle dbNote
                                , noteTags  = tags
                                , noteBody  = dbnoteBody dbNote
                                , noteCTime = Just (dbnoteCTime dbNote)
                                , noteMTime = Just (dbnoteMTime dbNote) }
                    _ -> Nothing
  return maybeNote

-- | Add tags to the database.
tagsAdd :: [Tag] -> NokeeIO ()
tagsAdd = mapM_ tagAdd
  where tagAdd :: Tag -> NokeeIO ()
        tagAdd tag =
          nokeeExecute "REPLACE INTO TAGS (ID, NAME) VALUES\
                       \ ((SELECT ID FROM TAGS WHERE NAME=?), ?)" (tag, tag)

-- | Associates given tags with the given 'NoteID'.
noteTagsAdd :: NoteID -> [Tag] -> NokeeIO ()
noteTagsAdd nId = mapM_ noteTagAdd
  where noteTagAdd :: Tag -> NokeeIO ()
        noteTagAdd tag =
          nokeeExecute "INSERT INTO NOTETAGS (NOTE, TAG) VALUES \
                       \ (?, (SELECT ID FROM TAGS WHERE NAME=?));" (nId, tag)

-- | Adds a new note to the database. The metadata fields in note will be
-- ignored.
noteAdd :: Note -> NokeeIO ()
noteAdd note = do
  nokeeExecute "INSERT INTO NOTES (TITLE, BODY) VALUES (?, ?);"
    (noteTitle note, noteBody note)
  noteId <- nokeeLastInsertRowId
  tagsAdd (noteTags note)
  noteTagsAdd (toInteger noteId) (noteTags note)

-- | Updates a note; the note must contain a valid 'NoteID'.
noteUpdate :: Note -> NokeeIO ()
noteUpdate note = do
  -- FIXME: Exception, if no noteID!
  nokeeExecute "UPDATE NOTES SET TITLE=?, BODY=?, MTIME=CURRENT_TIMESTAMP WHERE ID=?;"
    (noteTitle note, noteBody note, noteID note)
  tagsAdd (noteTags note)
  updateTags (fromJust (noteID note)) (noteTags note)
  where updateTags :: NoteID -> [Tag] -> NokeeIO ()
        updateTags nId tags = do
          nokeeExecute "DELETE FROM NOTETAGS WHERE NOTE=?;" (Only nId)
          noteTagsAdd nId tags

-- | Computes the directory containing the database stores.
storeDirectoryName :: IO String
storeDirectoryName = do
  home <- getEnv "HOME"
  return $ home </> storeDirectory

-- | Computes the database store filename for a given store name.
storeFileName :: StoreName -> IO String
storeFileName storeName = do
  storeDir <- storeDirectoryName
  return $ storeDir </> storeName ++ "." ++ storeSuffix

-- | Initializes a new store for the given store name.
noteStoreInitialize :: String -> IO ()
noteStoreInitialize storeFile = do
  _ <- execProcess sqliteExecutable [storeFile] sqliteStoreTableDef -- FIXME, error checking
  return ()

-- | Given a search pattern, retrieve the list of those notes matching
-- that search pattern.
noteSearch :: String -> NokeeIO [Note]
noteSearch patt = do
  let patt' = "%" ++ patt ++ "%"
  noteList (Just patt')

-- | Converts list of 'Tag's into its string representation.
packTags :: [Tag] -> String
packTags = intercalate ", "

-- | Converts the string representation of a tags list into a list of
-- 'Tag's.
unpackTags :: String -> [Tag]
unpackTags = filter (/= "") . map strip . split ","

-- | Converts a 'Note' into its string representation.
noteToString :: Note -> String
noteToString note =
  noteTitle note ++ "\n"
    ++ packTags (noteTags note) ++ "\n"
    ++ nokeeStringDelimiter ++ "\n"
    ++ (unlines . lines . noteBody) note

-- | Converts the String representation of a note into a 'Note'.
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

-- | Retrieves a list of notes. If a search pattern is specified,
-- retrieve only those notes matching the search pattern, otherwise
-- return all notes in the active store.
noteList :: Maybe String -> NokeeIO [Note]
noteList maybePatt = do
  dbnotes <- (case maybePatt of
                Just p -> nokeeQuery "SELECT * FROM NOTES WHERE \
                                     \TITLE LIKE ? OR BODY LIKE ?;" (p, p)
                _      -> nokeeQuery_ "SELECT ID, TITLE, BODY, CTIME, MTIME \
                                      \FROM NOTES;") :: NokeeIO [DBNote]
  mapM prepareNote dbnotes
  where prepareNote :: DBNote -> NokeeIO Note
        prepareNote dbnote = do
          tags <- retrieveTags (dbnoteID dbnote)
          return Note { noteID = Just (dbnoteID dbnote)
                      , noteTitle = dbnoteTitle dbnote
                      , noteBody = dbnoteBody dbnote
                      , noteTags = tags
                      , noteCTime = Just (dbnoteCTime dbnote)
                      , noteMTime = Just (dbnoteMTime dbnote) }

-- | Retrieves the list of those tags which are referenced by at least
-- one note.
retrieveReferencedTags :: NokeeIO [Tag]
retrieveReferencedTags = do
  tags <- nokeeQuery_ "SELECT TAGS.NAME FROM TAGS INNER JOIN NOTETAGS \
                      \ON TAGS.ID = NOTETAGS.TAG;" :: NokeeIO [[String]]
  return $ nubSort (map head tags)

-- | Produces a summary string given a list of notes.
notesPrintSummary :: [Note] -> String
notesPrintSummary notes =
  let idMax = length $ show (maximum (mapMaybe noteID notes))
  in concatMap (\ note -> notePrintSummary note idMax ++ "\n") notes

  where notePrintSummary :: Note -> Int -> String
        notePrintSummary note padding =
          let nId   = maybe "0" show (noteID note)
              cTime = fromMaybe "                   "
                                (formatTime defaultTimeLocale "%F %R" <$> noteCTime note)
          in pad nId ++ nId ++ " " ++ cTime ++ " " ++ noteTitle note
          where pad n = replicate (padding - length n) ' '

-- | Delete a note from the database given its 'NoteID'.
noteDelete :: NoteID -> NokeeIO ()
noteDelete nId = do
  nokeeExecute "DELETE FROM NOTES WHERE ID=?;" (Only nId)
  nokeeExecute "DELETE FROM NOTETAGS WHERE NOTE=?;" (Only nId)

nokeeRunCommand :: StoreName -- ^ The name of the store to use
                -> Bool      -- ^ True, if the store shall be created
                             -- in case it does not exist yet
                -> NokeeIO a -- ^ The NokeeIO action to run
                -> IO a      -- ^ Returns the resulting IO action
nokeeRunCommand storeName createStore f = do
  -- FIXME, what about exceptions?
  storeFile <- storeFileName storeName
  storeExists <- doesFileExist storeFile
  when (not createStore && not storeExists) $
    throw (NokeeExceptionString "Store does not exist")
  storeHandle <- open storeFile
  let nokeeEnv = NokeeEnv storeHandle
  withTransaction storeHandle $ runReaderT f nokeeEnv
