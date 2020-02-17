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
    (shell "tmux list-pane -as -F '#{session_name}:#{window_name}:#{pane_index}:#{pane_start_command}'")
    ""

-- stop unrequired

removeQuotes :: PaneCommand -> PaneCommand
removeQuotes (PaneCommand s) =
  PaneCommand $ (filter ((/=) '\'')) s

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
  Running
    <$> seshName
      <*> windowName
      <*> cmd
      <*> index
  where
    seshName = SessionName <$> myLookup 0 subStrs
    windowName = WindowName <$> myLookup 1 subStrs
    index = myLookup 2 subStrs >>= readMaybe
    cmd = case intercalate ":" (drop 3 subStrs) of
      "" -> Nothing
      a -> Just (PaneCommand a)
    subStrs = wordsWhen (== ':') str

parseRunning :: SessionName -> String -> [Running]
parseRunning (SessionName seshName) as =
  catMaybes (parseSingle <$> (lines as))
