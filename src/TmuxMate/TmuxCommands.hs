{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.TmuxCommands
  ( createSession,
    createWindow,
    createWindowPanes,
    removeWindowPanes,
    removeWindows,
    attachToSession,
    getTmuxCommands,
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
import TmuxMate.Types
import TmuxMate.Validate

getTmuxCommands :: ValidatedSession -> TmuxState -> [TmuxCommand]
getTmuxCommands sesh tmuxState =
  let runningPanes =
        running tmuxState
      runningSessions =
        sessions tmuxState
      runningInTmux =
        inSession tmuxState
      sTitle =
        vSessionTitle sesh
      sWindows =
        vSessionWindows sesh
   in (createSession runningInTmux sTitle runningSessions)
        <> ( concatMap
               (maybeToList . createWindow sTitle runningPanes)
               (vWindowTitle <$> sWindows)
           )
        <> ( concatMap
               (createWindowPanes sTitle runningPanes)
               (vSessionWindows sesh)
           )
        <> (removeWindowPanes sTitle runningPanes sWindows)
        <> (removeWindows sTitle runningPanes sWindows)
        <> (removeAdminPane sTitle)
        <> (attachToSession runningInTmux sTitle runningSessions)

-- create a new session if required
createSession :: InTmuxSession -> VSessionName -> [VSessionName] -> [TmuxCommand]
createSession inTmux seshName runningSessions =
  if sessionExists seshName runningSessions
    then case inTmux of
      InTmuxSession _ -> [SwitchToSession seshName]
      NotInTmuxSession -> []
    else case inTmux of
      InTmuxSession _ -> [NewSession seshName, SwitchToSession seshName]
      NotInTmuxSession -> [NewSession seshName]

sessionExists :: VSessionName -> [VSessionName] -> Bool
sessionExists = elem

-- do we need to create this window?
createWindow :: VSessionName -> [Running] -> VWindowName -> Maybe TmuxCommand
createWindow seshName running winName =
  if windowExists seshName winName running
    then Nothing
    else Just (CreateWindow seshName winName)

windowExists :: VSessionName -> VWindowName -> [Running] -> Bool
windowExists seshName winName running =
  length (filter (\a -> windowName a == winName && sessionName a == seshName) running) > 0

-- create panes we need for a given window
createWindowPanes :: VSessionName -> [Running] -> VWindow -> [TmuxCommand]
createWindowPanes seshName running window =
  ( \pane ->
      CreatePane
        seshName
        (vWindowTitle window)
        (paneCmdToCmd pane)
  )
    <$> filterPanes
      seshName
      (vWindowTitle window)
      running
      (vWindowPanes window)
  where
    paneCmdToCmd =
      Command . getPaneCommand . paneCommand

-- work out what panes we need to create
filterPanes :: VSessionName -> VWindowName -> [Running] -> NE.NonEmpty Pane -> [Pane]
filterPanes seshName winName running panes =
  NE.filter (\pane -> not $ matchCommand (removeQuotes (paneCommand pane))) panes
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

removeWindowPanes :: VSessionName -> [Running] -> NE.NonEmpty VWindow -> [TmuxCommand]
removeWindowPanes seshName running windows =
  (\(Running _ _ _ i) -> KillPane seshName i)
    <$> (filterRunning seshName windows running)

filterRunning :: VSessionName -> NE.NonEmpty VWindow -> [Running] -> [Running]
filterRunning seshName windows running =
  filter
    ( \(Running seshName' winName' run _) ->
        not $
          anyMatch (removeQuotes run) windows
            && seshName == seshName'
    )
    running
  where
    anyMatch :: PaneCommand -> NE.NonEmpty VWindow -> Bool
    anyMatch str windows' =
      getAny (foldMap (matchCommand str) windows)
    matchCommand :: PaneCommand -> VWindow -> Any
    matchCommand str window =
      Any $
        length
          ( NE.filter
              ( \pane ->
                  removeQuotes (paneCommand pane) == str
              )
              (vWindowPanes window)
          )
          > 0

removeWindows :: VSessionName -> [Running] -> NE.NonEmpty VWindow -> [TmuxCommand]
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
      vWindowTitle <$> windows
    runningWindowNames =
      nub $ windowName <$> filter (\(Running sesh' win' _ _) -> sesh' == seshName) running

--- attaching or switching to session
attachToSession :: InTmuxSession -> VSessionName -> [VSessionName] -> [TmuxCommand]
attachToSession inTmux seshName runningSessions =
  case inTmux of
    InTmuxSession sesh -> [SwitchToSession sesh]
    _ -> [AttachToSession seshName]

-- remove admin window (always)

removeAdminPane :: VSessionName -> [TmuxCommand]
removeAdminPane seshName = pure (KillAdminPane seshName)
