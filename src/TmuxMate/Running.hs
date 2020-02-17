{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.Running where

import Control.Exception
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (catMaybes, listToMaybe)
import System.Process
import Text.Read
import TmuxMate.Types

-- "foo:yes Pane 2\nfoo:yes Pane 1\n"

askRunning :: SessionName -> IO [Running]
askRunning seshName = do
  str <- catch readTmuxProcess (\(e :: IOError) -> pure "")
  pure $ parseRunning seshName str

-- ask Tmux what's cooking
readTmuxProcess :: IO String
readTmuxProcess =
  readCreateProcess
    (shell "tmux list-pane -as -F '#{session_name}:#{pane_index}:#{pane_start_command}'")
    ""

-- stop unrequired

removeQuotes :: String -> String
removeQuotes = (filter ((/=) '\''))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

myLookup :: Int -> [a] -> Maybe a
myLookup _ [] = Nothing
myLookup 0 (x : _) = Just x
myLookup i (_ : xs) = myLookup (i - 1) xs

parseSingle :: String -> Maybe Running
parseSingle str =
  Running <$> seshName <*> cmd <*> index
  where
    seshName = myLookup 0 subStrs
    index = myLookup 1 subStrs >>= readMaybe
    cmd = case intercalate ":" (drop 2 subStrs) of
      "" -> Nothing
      a -> Just a
    subStrs = wordsWhen (== ':') str

parseRunning :: SessionName -> String -> [Running]
parseRunning (SessionName seshName) as =
  catMaybes (parseSingle <$> (lines as))
