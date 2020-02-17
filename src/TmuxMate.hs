{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
    createSession,
    createWindow,
    createWindowPanes,
    removeWindowPanes,
    removeWindows,
  )
where

import Control.Exception
import Data.List (nub)
import Data.Maybe
import Data.Monoid (Any (..))
import qualified Dhall as Dhall
import System.Process
import TmuxMate.Commands
import TmuxMate.Running
import TmuxMate.Types

getCommands :: Session -> [Running] -> [Command]
getCommands sesh running =
  concatMap createActualCommand (getTmuxCommands sesh running)

getTmuxCommands :: Session -> [Running] -> [TmuxCommand]
getTmuxCommands sesh running =
  maybeToList (createSession (sessionTitle sesh) running)
    <> ( concatMap
           (maybeToList . createWindow (sessionTitle sesh) running)
           (windowTitle <$> sessionWindows sesh)
       )
    <> ( concatMap
           (createWindowPanes (sessionTitle sesh) running)
           (sessionWindows sesh)
       )
    <> (removeWindowPanes (sessionTitle sesh) running (sessionWindows sesh))
    <> (removeWindows (sessionTitle sesh) running (sessionWindows sesh))
  where

{-startSession isNewSession (sessionTitle sesh)
  <> concatMap (addPane (sessionTitle sesh)) filteredPanes
  <> concatMap (removePane (sessionTitle sesh)) filteredRunning
  <> attach isNewSession (sessionTitle sesh)-}

{-filteredPanes =
  filterPanes (sessionTitle sesh) running (sessionPanes sesh)
filteredRunning =
  filterRunning (sessionTitle sesh) (sessionPanes sesh) running
isNewSession =
  if length running > 0
    then IsOldSession
    else IsNewSession-}

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
                [ Pane (PaneCommand "yes 'Pane 1'") (PaneTitle "One"),
                  Pane (PaneCommand "yes 'Pane 2'") (PaneTitle "Two"),
                  Pane (PaneCommand "yes 'Pane 3'") (PaneTitle "Three"),
                  Pane (PaneCommand "yes 'Pane 4'") (PaneTitle "Four")
                ]
            }
        ]
    }

-- create a new session if required
createSession :: SessionName -> [Running] -> Maybe TmuxCommand
createSession seshName running =
  if sessionExists seshName running
    then Nothing
    else Just (NewSession seshName)

sessionExists :: SessionName -> [Running] -> Bool
sessionExists seshName running =
  length (filter (\a -> sessionName a == seshName) running) > 0

-- do we need to create this window?
createWindow :: SessionName -> [Running] -> WindowName -> Maybe TmuxCommand
createWindow seshName running winName =
  if windowExists seshName winName running
    then Nothing
    else Just (CreateWindow seshName winName)

windowExists :: SessionName -> WindowName -> [Running] -> Bool
windowExists seshName winName running =
  length (filter (\a -> windowName a == winName && sessionName a == seshName) running) > 0

-- create panes we need for a given window
createWindowPanes :: SessionName -> [Running] -> Window -> [TmuxCommand]
createWindowPanes seshName running window =
  ( \pane ->
      CreatePane
        seshName
        (windowTitle window)
        (paneCmdToCmd pane)
  )
    <$> filterPanes
      seshName
      (windowTitle window)
      running
      (windowPanes window)
  where
    paneCmdToCmd =
      Command . getPaneCommand . paneCommand

-- work out what panes we need to create
filterPanes :: SessionName -> WindowName -> [Running] -> [Pane] -> [Pane]
filterPanes seshName winName running panes =
  filter (\pane -> not $ matchCommand (removeQuotes (paneCommand pane))) panes
  where
    matchCommand str =
      length
        ( filter
            ( \(Running seshName' winName' run _) ->
                removeQuotes run == str
                  && seshName == seshName'
                  && winName == winName'
            )
            running
        )
        > 0

--------------------------
-- removing stuff again

removeWindowPanes :: SessionName -> [Running] -> [Window] -> [TmuxCommand]
removeWindowPanes seshName running windows =
  (\(Running _ _ _ i) -> KillPane seshName i)
    <$> (filterRunning seshName windows running)

filterRunning :: SessionName -> [Window] -> [Running] -> [Running]
filterRunning seshName windows running =
  filter
    ( \(Running seshName' winName' run _) ->
        not $
          anyMatch (removeQuotes run) windows
            && seshName == seshName'
    )
    running
  where
    anyMatch :: PaneCommand -> [Window] -> Bool
    anyMatch str windows' =
      getAny (foldMap (matchCommand str) windows)
    matchCommand :: PaneCommand -> Window -> Any
    matchCommand str window =
      Any $
        length
          ( filter
              ( \pane ->
                  removeQuotes (paneCommand pane) == str
              )
              (windowPanes window)
          )
          > 0

removeWindows :: SessionName -> [Running] -> [Window] -> [TmuxCommand]
removeWindows seshName running windows =
  ( \winTitle' ->
      KillWindow
        seshName
        winTitle'
  )
    <$> filter
      ( \win' ->
          notElem win' requiredWindowNames
      )
      runningWindowNames
  where
    requiredWindowNames =
      windowTitle <$> windows
    runningWindowNames =
      nub $ windowName <$> filter (\(Running sesh' win' _ _) -> sesh' == seshName) running

--- attaching or switching to session

-- if the stuff we've made is running, switch to it
-- attach sometimes?

-- remove admin window (always)

---- load dhall file and let's get cracking

loadTestSession :: FilePath -> IO ()
loadTestSession path = do
  --let (decoder :: Dhall.Decoder Session) = Dhall.auto
  --- config <- Dhall.inputFile decoder path
  let config = testSession
  running <- askRunning (sessionTitle config)
  print running
  let tmuxCommands = getTmuxCommands config running
  print tmuxCommands
  let commands = (getCommands config running)
  print commands
  runCommands commands
