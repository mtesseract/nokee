-- Nokee (Note Keeper).
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

module Utilities where

import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import System.Process
import Data.List

execProcess :: String -> [String] -> String -> IO ExitCode
execProcess cmd args input = do
  let processSpec = proc cmd args
      processSpec' = processSpec { std_in = CreatePipe }
  (Just inHandle, _, _, pHandle) <- createProcess processSpec'
  hPutStr inHandle input
  hClose inHandle
  waitForProcess pHandle

execEditor :: String -> String -> IO ExitCode
execEditor defaultEditor filename = do
  editor <- fromMaybe defaultEditor <$> lookupEnv "EDITOR"
  rawSystem editor [filename]

-- Form the intersection list of two lists, i.e. the list of those
-- elements, which are contained in both lists (the list elements are
-- regarded as pairwise distinct).
listIntersection :: (Eq a) => [a] -> [a] -> [a]
listIntersection list0 list1 =
  filter containedInList1 list0
  where containedInList1 x = x `elem` list1

nubSort :: (Ord a) => [a] -> [a]
nubSort = nub . sort
