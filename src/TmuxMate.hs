{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
    DidItWork (..),
  )
where

import qualified Dhall as Dhall
import System.Process
import TmuxMate.Commands
import TmuxMate.Running
import TmuxMate.TmuxCommands
import TmuxMate.Types
import TmuxMate.Validate

getCommands :: [TmuxCommand] -> [Command]
getCommands =
  concatMap createActualCommand

runCommands :: [Command] -> IO ()
runCommands =
  mapM_
    ( \(Command a) -> callCommand a
    )

data DidItWork
  = Yeah
  | Nah Int

loadTestSession :: FilePath -> IO DidItWork
loadTestSession path = do
  let (decoder :: Dhall.Decoder Session) = Dhall.auto
  config <- Dhall.inputFile decoder path
  case parseSession config of
    Left e -> do
      putStrLn $ "Error parsing config at " <> path
      print e
      pure (Nah 1)
    Right config' -> do
      tmuxState <- askTmuxState
      -- print tmuxState
      let tmuxCommands = getTmuxCommands config' tmuxState
      -- putStrLn "Tmux Commands"
      -- print tmuxCommands
      let commands = getCommands tmuxCommands
      -- putStrLn "Shell commands"
      -- print commands
      runCommands commands
      pure Yeah
