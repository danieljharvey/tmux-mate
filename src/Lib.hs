{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( loadTestSession,
  )
where

import Control.Exception
import qualified Dhall as Dhall
import System.Process
import TmuxMate.Commands
import TmuxMate.Running
import TmuxMate.Types

getCommands :: Session -> [Running] -> [Command]
getCommands sesh running =
  startSession isNewSession (title sesh)
    <> concatMap (addPane (title sesh)) filteredPanes
    <> concatMap (removePane (title sesh)) filteredRunning
    <> attach isNewSession (title sesh)
  where
    filteredPanes =
      filterPanes (title sesh) running (panes sesh)
    filteredRunning =
      filterRunning (title sesh) (panes sesh) running
    isNewSession =
      if length running > 0
        then OldSession
        else NewSession

runCommands :: [Command] -> IO ()
runCommands =
  mapM_
    ( \(a) -> case a of
        Required a -> callCommand a
        Optional a -> do
          res <- (system a)
          pure ()
    )

testSession :: Session
testSession =
  Session
    { title = SessionName "foo",
      panes =
        [ Pane (PaneCommand "yes 'Pane 1'") (PaneTitle "One"),
          Pane (PaneCommand "yes 'Pane 2'") (PaneTitle "Two"),
          Pane (PaneCommand "yes 'Pane 3'") (PaneTitle "Three"),
          Pane (PaneCommand "yes 'Pane 4'") (PaneTitle "Four")
        ]
    }

filterPanes :: SessionName -> [Running] -> [Pane] -> [Pane]
filterPanes (SessionName seshName) running panes =
  filter (\pane -> not $ matchCommand (removeQuotes (getPaneCommand $ paneCommand pane))) panes
  where
    matchCommand str =
      length
        ( filter
            ( \(Running seshName' run _) ->
                removeQuotes run == str
                  && seshName == seshName'
            )
            running
        )
        > 0

filterRunning :: SessionName -> [Pane] -> [Running] -> [Running]
filterRunning (SessionName seshName) panes running =
  filter
    ( \(Running seshName' run _) ->
        not $ (matchCommand (removeQuotes run) && seshName == seshName')
    )
    running
  where
    matchCommand str =
      length
        ( filter
            ( \pane ->
                removeQuotes (getPaneCommand $ paneCommand pane) == str
            )
            panes
        )
        > 0

loadTestSession :: FilePath -> IO ()
loadTestSession path = do
  let (decoder :: Dhall.Decoder Session) = Dhall.auto
  config <- Dhall.inputFile decoder path
  running <- askRunning (title config)
  print running
  let commands = (getCommands config running)
  print commands
  runCommands commands
