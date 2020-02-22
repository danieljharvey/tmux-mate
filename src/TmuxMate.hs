{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
  )
where

import Control.Exception
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid (Any (..))
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

testSession :: Session
testSession =
  Session
    { sessionTitle = SessionName "foo",
      sessionWindows =
        [ Window
            { windowTitle = WindowName "first-window",
              windowPanes =
                [ Pane (PaneCommand "yes 'Pane 1'"),
                  Pane (PaneCommand "yes 'Pane 2'"),
                  Pane (PaneCommand "yes 'Pane 3'"),
                  Pane (PaneCommand "yes 'Pane 4'")
                ]
            },
          Window
            { windowTitle = WindowName "second-window",
              windowPanes =
                [ Pane (PaneCommand "yes 'Second Window - Pane 1'"),
                  Pane (PaneCommand "yes 'Second Window - Pane 2'"),
                  Pane (PaneCommand "yes 'Second Window - Pane 3'"),
                  Pane (PaneCommand "yes 'Second Window - Pane 4'")
                ]
            }
        ]
    }

loadTestSession :: FilePath -> IO ()
loadTestSession path = do
  --let (decoder :: Dhall.Decoder Session) = Dhall.auto
  --- config <- Dhall.inputFile decoder path
  let config = testSession
  case parseSession config of
    Left e -> print e
    Right config' -> do
      tmuxState <- askTmuxState
      print tmuxState
      let tmuxCommands = getTmuxCommands config' tmuxState
      putStrLn "Tmux Commands"
      print tmuxCommands
      let commands = getCommands tmuxCommands
      putStrLn "Shell commands"
      print commands
      runCommands commands
