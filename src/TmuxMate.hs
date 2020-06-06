{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
    DidItWork (..),
    CLIOptions (..),
    ConfigFilePath (..),
    Verbosity (..),
    createTmuxMateDhall,
  )
where

import Data.Maybe (fromMaybe)
import qualified Dhall as Dhall
import System.Process
import TmuxMate.Commands
import TmuxMate.Init
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
  let path = fromMaybe "tmux-mate.dhall" (getConfigFilePath <$> configFilePath options)
      myLog = logger (verbosity options)
  config <- Dhall.detailed (Dhall.inputFile decoder path)
  case parseSession config of
    Left e -> do
      myLog Highlight ("Error parsing config at " <> path)
      myLog Error (show e)
      pure (Nah 1)
    Right config' -> do
      case verbosity options of
        DryRun -> myLog Highlight "Dry run mode - will not execute"
        _ -> pure ()
      tmuxState <- askTmuxState
      myLog Highlight "Current tmux state"
      myLog Info (show tmuxState)
      let tmuxCommands = getTmuxCommands config' tmuxState
      myLog Highlight "Tmux Commands"
      _ <- traverse (myLog Info . show) tmuxCommands
      let commands = getCommands tmuxCommands
      myLog Highlight "Shell commands"
      _ <- traverse (myLog Info . getCommand) commands
      case verbosity options of
        DryRun -> pure Yeah
        _ -> do
          runCommands commands
          pure Yeah
