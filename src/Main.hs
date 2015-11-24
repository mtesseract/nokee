-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Main where

import Options.Applicative
import Control.Exception
import Data.Typeable
import Nokee

---------------------
-- Nokee constants --
---------------------
programName :: String
programName = "nokee"

programVersion :: String
programVersion = "0.1-git"

programDescription :: String
programDescription = "(Simple & Stupid) Note Manager"

-----------------------------
-- Define Nokee exceptions --
-----------------------------

data NokeeException = ExceptionString String | ExceptionNone
    deriving (Show, Typeable)

instance Exception NokeeException

-- This gets called after arguments have been parsed. This function is
-- just a wrapper around the function 'nokee', adding exception
-- handling.
main' :: NokeeOptions -> IO ()
main' opts =
  catch (nokee opts)
        (\ e -> case (e :: NokeeException) of
                  ExceptionString s -> putStrLn $ "Error: " ++ s
                  ExceptionNone     -> return ())

-- Implements the main program logic. May throw NokeeExceptions, they
-- will be handled in the caller.
nokee :: NokeeOptions -> IO ()
nokee opts = do
  let store = optsStore opts
  case optsCommand opts of
    InitOptions ->
      cmdNoteInit store
    EditOptions nId ->
      nokeeWithStore store $ cmdNoteEdit nId
    DeleteOptions nId ->
      nokeeWithStore store $ cmdNoteDelete nId
    AddOptions ->
      nokeeWithStore store cmdNoteAdd
    ListOptions tags ->
      nokeeWithStore store (cmdNoteList tags)
    ListStoreOptions ->
      cmdNoteListStores
    ListTagsOptions ->
      nokeeWithStore store cmdNoteListTags
    SearchOptions pattern ->
      nokeeWithStore store $ cmdNoteSearch pattern
    RetrieveOptions nId ->
      nokeeWithStore store $ cmdNoteRetrieve nId

-- This type holds the information about parsed arguments.
data NokeeOptions = NokeeOptions
  { optsVersion :: Bool
  , optsStore   :: String
  , optsCommand :: Command }

-- Type for storing information about the parsed commands.
data Command
  = RetrieveOptions Integer
  | AddOptions
  | ListOptions String
  | EditOptions Integer
  | DeleteOptions Integer
  | SearchOptions String
  | InitOptions
  | ListStoreOptions
  | ListTagsOptions

-- The Nokee argument parser.
nokeeOptions :: Parser NokeeOptions
nokeeOptions = NokeeOptions
     <$> switch
         (long "version"
          <> help "Display version information")
     <*> strOption
         (value "default"
          <> long "store"
          <> short 's'
          <> metavar "STORENAME"
          <> help "Specify store to use")
     <*> subparser
         (command "edit" (info editOptions
                          (progDesc "Edit a note"))
          <> command "add" (info addOptions
                             (progDesc "Add a note"))
          <> command "delete" (info deleteOptions
                             (progDesc "Delete a note"))
          <> command "list" (info listOptions
                             (progDesc "List notes"))
          <> command "list-stores" (info listStoresOptions
                             (progDesc "List Stores"))
          <> command "list-tags" (info listTagsOptions
                                  (progDesc "List Tags"))
          <> command "init" (info initOptions
                             (progDesc "Initialize store"))
          <> command "search" (info searchOptions
                             (progDesc "Search notes"))
          <> command "retrieve" (info retrieveOptions
                             (progDesc "Retrieve a note")))

  where initOptions       = pure InitOptions
        addOptions        = pure AddOptions
        listOptions       = ListOptions <$> strOption (value ""
                                                       <> long "tags"
                                                       <> short 't'
                                                       <> metavar "TAGS"
                                                       <> help "Specify tags.")
        listStoresOptions = pure ListStoreOptions
        listTagsOptions   = pure ListTagsOptions
        editOptions       = EditOptions     <$> argument auto (metavar "ID")
        deleteOptions     = DeleteOptions   <$> argument auto (metavar "ID")
        searchOptions     = SearchOptions   <$> argument str  (metavar "PATTERN")
        retrieveOptions   = RetrieveOptions <$> argument auto (metavar "ID")

-- Main entry point. This parses arguments and passes the parsed
-- arguments to main'.
main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> nokeeOptions)
                 (fullDesc
                  <> progDesc "Utility for managing plaintext notes."
                  <> header (programName ++ " - " ++ programDescription))
