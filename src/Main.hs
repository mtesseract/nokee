-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

{-|
Module      : Main
Description : Module implementing the Nokee CLI interface.
Copyright   : (C) 2015 Moritz Schulte
License     : BSD3
Maintainer  : Moritz Schulte <mtesseract@silverratio.net>
Stability   : experimental
Portability : POSIX
-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Version
import Nokee
import Options.Applicative
import Paths_Nokee
import System.Exit

---------------------
-- Nokee constants --
---------------------

-- | The name of the program.
programName :: String
programName = "Nokee"

-- | The version of the program.
programVersion :: String
programVersion = showVersion version

-- | Short description of the program.
programDescription :: String
programDescription = "(Simple & Stupid) Note Manager"

-----------------------------
-- Define Nokee exceptions --
-----------------------------

-- | Exception type used in Nokee.
data NokeeException =
  ExceptionNone -- ^ Exception value representing no exception
  | ExceptionString String -- ^ Exception value holding an error
                           -- message
  deriving (Show, Typeable)

-- | A 'NokeeException' is an 'Exception'.
instance Exception NokeeException

-- | This function is just a wrapper around the function 'nokee', adding
-- exception handling. It gets called after arguments have been
-- parsed.
main' :: NokeeOptions -> IO ()
main' opts =
  catch (nokee opts)
        (\ e -> case (e :: NokeeException) of
                  ExceptionString s -> do putStrLn $ "Error: " ++ s
                                          exitFailure
                  ExceptionNone     -> return ())

-- | Print version information to stdout.
printVersion :: IO ()
printVersion = putStrLn $ programName ++ " " ++ programVersion

-- | This function implements the main program logic. May throw
-- NokeeExceptions, they will be handled in the caller.
nokee :: NokeeOptions -> IO ()
nokee opts = do
  let store = optsStore opts
  when (optsVersion opts) $ do
    printVersion
    throw ExceptionNone
  case optsCommand opts of
    Just InitOptions ->
      cmdNoteInit store
    Just (EditOptions nId) ->
      nokeeWithStore store $ cmdNoteEdit nId
    Just (DeleteOptions nId) ->
      nokeeWithStore store $ cmdNoteDelete nId
    Just AddOptions ->
      nokeeWithStore store cmdNoteAdd
    Just (ListOptions tags) ->
      nokeeWithStore store (cmdNoteList tags)
    Just ListStoreOptions ->
      cmdNoteListStores
    Just ListTagsOptions ->
      nokeeWithStore store cmdNoteListTags
    Just (SearchOptions pattern) ->
      nokeeWithStore store $ cmdNoteSearch pattern
    Just (RetrieveOptions nId) ->
      nokeeWithStore store $ cmdNoteRetrieve nId
    Nothing ->
      throw (ExceptionString "No command specified -- what should I do? Try --help.")

-- | Type holding the information about parsed arguments.
data NokeeOptions = NokeeOptions
  { optsVersion :: Bool
  , optsStore   :: String
  , optsCommand :: Maybe Command }

-- | Type for storing information about the parsed commands.
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

-- | The Nokee main argument parser.
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
     <*> (optional $ subparser
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
                                    (progDesc "Retrieve a note"))))
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

-- | Main entry point. This parses arguments and passes the parsed
-- arguments to main'.
main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> nokeeOptions)
                 (fullDesc
                  <> progDesc "Utility for managing plaintext notes."
                  <> header (programName ++ " - " ++ programDescription))
