-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Main where

import Options.Applicative

import Control.Exception
import Data.Typeable
import Nokee

programName :: String
programName = "nokee"

programVersion :: String
programVersion = "0.1-git"

programDescription :: String
programDescription = "(Simple & Stupid) Note Manager"

data NokeeException = ExceptionString String | ExceptionNone
    deriving (Show, Typeable)

instance Exception NokeeException

nokee :: NokeeOptions -> IO ()
nokee opts = do
  let store = optsStore opts
  case optsCommand opts of
    InitOptions -> cmdNoteInit store
    EditOptions nId -> withStore store $ cmdNoteEdit nId
    DeleteOptions nId -> withStore store $ cmdNoteDelete nId
    AddOptions -> withStore store cmdNoteAdd
    ListOptions tags -> withStore store (cmdNoteList tags)
    ListStoreOptions -> cmdNoteListStores
    SearchOptions pattern -> withStore store $ cmdNoteSearch pattern
    RetrieveOptions nId -> withStore store $ cmdNoteRetrieve nId

main' :: NokeeOptions -> IO ()
main' opts =
  catch (nokee opts)
        (\ e -> case (e :: NokeeException) of
                  ExceptionString s -> putStrLn $ "Error: " ++ s
                  ExceptionNone     -> return ())

data NokeeOptions = NokeeOptions
  { optsVersion :: Bool
  , optsStore   :: String
  , optsCommand :: Command }

data Command
  = RetrieveOptions Integer
  | AddOptions
  | ListOptions String
  | EditOptions Integer
  | DeleteOptions Integer
  | SearchOptions String
  | InitOptions
  | ListStoreOptions
  
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
        editOptions       = EditOptions     <$> argument auto (metavar "ID")
        deleteOptions     = DeleteOptions   <$> argument auto (metavar "ID")
        searchOptions     = SearchOptions   <$> argument str  (metavar "PATTERN")
        retrieveOptions   = RetrieveOptions <$> argument auto (metavar "ID")

main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> nokeeOptions)
                 (fullDesc
                  <> header (programName ++ " - " ++ programDescription))
