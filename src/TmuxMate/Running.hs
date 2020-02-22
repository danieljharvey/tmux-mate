{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.Running where

import Control.Exception
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (catMaybes, listToMaybe)
import System.Environment
import System.Process
import Text.Read
import TmuxMate.Types
import TmuxMate.Validate

buildTmuxState :: IO TmuxState
buildTmuxState = do
  sessions <- askRunningSessions
  running <- askRunning
  inTmux <- askIfWeAreInTmux
  pure $ TmuxState inTmux running sessions

askTmuxState :: IO TmuxState
askTmuxState =
  catch
    (buildTmuxState)
    (\(e :: IOError) -> pure def)
  where
    def = TmuxState
      { inSession = NotInTmuxSession,
        running = mempty,
        sessions = mempty
      }

-- "foo:yes Pane 2\nfoo:yes Pane 1\n"

askRunning :: IO [Running]
askRunning = do
  str <- catch readTmuxProcess (\(e :: IOError) -> pure "")
  pure $ parseRunning str

-- ask Tmux what's cooking
readTmuxProcess :: IO String
readTmuxProcess =
  readCreateProcess
    (shell "tmux list-pane -as -F '#{session_name}:#{window_name}:#{pane_index}:#{pane_start_command}'")
    ""

-- "foo/npoo/n0/n"
askRunningSessions :: IO [VSessionName]
askRunningSessions = do
  str <- catch readTmuxSessions (\(e :: IOError) -> pure "")
  pure $ catMaybes $
    ( hush
        . parseSessionName
        . SessionName
    )
      <$> lines str

readTmuxSessions :: IO String
readTmuxSessions =
  readCreateProcess
    (shell "tmux list-sessions -F '#{session_name}'")
    ""

-- are we currently in a tmux session? (if so, don't nest)

askIfWeAreInTmux :: IO InTmuxSession
askIfWeAreInTmux = do
  tmuxEnv <- lookupEnv "TMUX"
  seshName <- askCurrentSessionName
  case tmuxEnv of
    Nothing -> pure NotInTmuxSession
    Just "" -> pure NotInTmuxSession
    Just a -> do
      case (parseSessionName seshName) of
        Right seshName' -> pure $ InTmuxSession seshName'
        _ -> pure NotInTmuxSession

askCurrentSessionName :: IO SessionName
askCurrentSessionName =
  SessionName
    <$> readCreateProcess
      (shell "tmux display-message -p '#S'")
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
    seshName =
      (SessionName <$> myLookup 0 subStrs)
        >>= (hush . parseSessionName)
    windowName =
      (WindowName <$> myLookup 1 subStrs)
        >>= (hush . parseWindowName)
    index =
      myLookup 2 subStrs
        >>= readMaybe
    cmd = case intercalate ":" (drop 3 subStrs) of
      "" -> Nothing
      a -> Just (PaneCommand a)
    subStrs = wordsWhen (== ':') str

parseRunning :: String -> [Running]
parseRunning as =
  catMaybes (parseSingle <$> (lines as))

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a
