-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

{-|
Module      : Utilities
Description : Some utility function used by Nokee
Copyright   : (C) 2015 Moritz Schulte
License     : BSD3
Maintainer  : Moritz Schulte <mtesseract@silverratio.net>
Stability   : experimental
Portability : POSIX
-}

module Utilities ( EditorSpec(..)
                 , execProcess
                 , execEditor
                 , listIntersection
                 , nubSort 
                 ) where

import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import System.Process
import Data.List

-- | Convenience wrapper for createProcess. Feeds the specified String
-- as input to the newly created process, inherits stdout, stderr and
-- the controlling terminal.
execProcess :: String      -- ^ The executable to execute
            -> [String]    -- ^ The list of arguments
            -> String      -- ^ The input to be passed to the
                           -- executable as its standard input
            -> IO ExitCode -- ^ The exit code returned by the
                           -- executable
execProcess cmd args input = do
  let processSpec = proc cmd args
      processSpec' = processSpec { std_in = CreatePipe }
  (Just inHandle, _, _, pHandle) <- createProcess processSpec'
  hPutStr inHandle input
  hClose inHandle
  waitForProcess pHandle

data EditorSpec = EditorSpec [String] String

-- | Spawns an editor, opening the specified filename.
execEditor :: EditorSpec  -- ^ The editor specification
           -> String      -- ^ The name of the file to open
           -> IO ExitCode -- ^ The exit code returned by the editor
execEditor (EditorSpec editorEnv defaultEditor) filename = do
  maybeEditors <- mapM lookupEnv editorEnv
  let editor = (fromMaybe defaultEditor . listToMaybe . catMaybes) maybeEditors
  rawSystem editor [filename]

-- | Forms the intersection list of two lists, i.e. the list of those
-- elements, which are contained in both lists. Duplicate elements are
-- removed.
listIntersection :: (Eq a) =>
                    [a] -> [a] -- ^ The input lists
                 -> [a]        -- ^ The computed intersection list
listIntersection list0 list1 =
  let (list0', list1') = (nub list0, nub list1)
  in filter ((flip elem) list1') list0'

-- | Returns the input list sorted and cleaned of duplicate elements.
nubSort :: (Ord a) =>
           [a] -- ^ The input list
        -> [a] -- ^ The output list
nubSort = nub . sort
