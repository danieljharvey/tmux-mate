{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
    DidItWork (..),
    CLIOptions (..),
    ConfigFilePath (..),
    Verbosity (..),
  )
where

import qualified Dhall as Dhall
import System.Process
import TmuxMate.Commands
import TmuxMate.Logger
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

loadTestSession :: CLIOptions -> IO DidItWork
loadTestSession options = do
  let (decoder :: Dhall.Decoder Session) = Dhall.auto
  let path = getConfigFilePath $ configFilePath options
      myLog = logger (verbosity options)
  config <- Dhall.inputFile decoder path
  case parseSession config of
    Left e -> do
      putStrLn $ "Error parsing config at " <> path
      myLog (show e)
      pure (Nah 1)
    Right config' -> do
      tmuxState <- askTmuxState
      myLog "Current tmux state"
      myLog (show tmuxState)
      let tmuxCommands = getTmuxCommands config' tmuxState
      myLog "Tmux Commands"
      _ <- traverse (myLog . show) tmuxCommands
      let commands = getCommands tmuxCommands
      myLog "Shell commands"
      _ <- traverse (myLog . show) commands
      runCommands commands
      pure Yeah
