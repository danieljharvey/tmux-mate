{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.TmuxCommands
  ( createSession,
    createWindow,
    removeWindowPanes,
    removeWindows,
    getTmuxCommands,
  )
where

import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Any (..))
import TmuxMate.Running
import TmuxMate.Types

getTmuxCommands :: ValidatedSession -> TmuxState -> [TmuxCommand]
getTmuxCommands sesh tmuxState =
  let runningPanes =
        running tmuxState
      runningSessions =
        sessions tmuxState
      runningInTmux =
        inSession tmuxState
      sTitle =
        case runningInTmux of
          NotInTmuxSession -> vSessionTitle sesh
          InTmuxSession sesh' -> sesh'
      sWindows =
        NE.toList (vSessionWindows sesh)
   in (createSession runningInTmux sesh runningSessions)
        <> ( concatMap
               (createWindow sTitle runningPanes)
               sWindows
           )
        <> (removeWindowPanes sTitle runningPanes sWindows)
        <> (removeWindows sTitle runningPanes sWindows)
        <> ( if needsNewSession runningInTmux sTitle runningSessions
               then removeAdminPane sTitle
               else []
           )
        <> [AttachToSession sTitle]

-- create a new session if required
createSession :: InTmuxSession -> ValidatedSession -> [VSessionName] -> [TmuxCommand]
createSession inTmux session runningSesh =
  if needsNewSession inTmux (vSessionTitle session) runningSesh
    then [NewSession (vSessionTitle session)]
    else []

needsNewSession :: InTmuxSession -> VSessionName -> [VSessionName] -> Bool
needsNewSession NotInTmuxSession seshName runningSesh = not (elem seshName runningSesh)
needsNewSession _ _ _ = False

-- do we need to create this window?
createWindow :: VSessionName -> [Running] -> VWindow -> [TmuxCommand]
createWindow seshName running' window =
  if windowExists seshName (vWindowTitle window) running'
    then
      createWindowPanes
        seshName
        (vWindowTitle window)
        (NE.toList $ vWindowPanes window)
        running'
    else
      pure
        ( CreateWindow
            seshName
            (vWindowTitle window)
            (paneCmdToCmd (NE.head (vWindowPanes window)))
        )
        <> createWindowPanes
          seshName
          (vWindowTitle window)
          (NE.tail $ vWindowPanes window)
          running'

windowExists :: VSessionName -> VWindowName -> [Running] -> Bool
windowExists seshName winName running' =
  length
    ( filter
        ( \a ->
            windowName a == winName
              && sessionName a == seshName
        )
        running'
    )
    > 0

-- create panes we need for a given window
createWindowPanes :: VSessionName -> VWindowName -> [Pane] -> [Running] -> [TmuxCommand]
createWindowPanes seshName windowName' panes running' =
  ( \pane ->
      CreatePane
        seshName
        windowName'
        (paneCmdToCmd pane)
  )
    <$> filterPanes
      seshName
      windowName'
      running'
      panes

paneCmdToCmd :: Pane -> Command
paneCmdToCmd =
  Command . getPaneCommand . paneCommand

-- work out what panes we need to create
filterPanes :: VSessionName -> VWindowName -> [Running] -> [Pane] -> [Pane]
filterPanes seshName winName running' panes =
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
            running'
        )
        > 0

--------------------------
-- removing stuff again

removeWindowPanes :: VSessionName -> [Running] -> [VWindow] -> [TmuxCommand]
removeWindowPanes seshName running' windows =
  (\(Running _ _ _ i) -> KillPane seshName i)
    <$> (filterRunning seshName windows running')

filterRunning :: VSessionName -> [VWindow] -> [Running] -> [Running]
filterRunning seshName windows running' =
  filter
    ( \(Running seshName' _ run _) ->
        not $
          anyMatch (removeQuotes run) windows
            && seshName == seshName'
    )
    running'
  where
    anyMatch :: PaneCommand -> [VWindow] -> Bool
    anyMatch str windows' =
      getAny (foldMap (matchCommand str) windows')
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

removeWindows :: VSessionName -> [Running] -> [VWindow] -> [TmuxCommand]
removeWindows seshName running' windows =
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
      nub $
        windowName
          <$> filter
            ( \(Running sesh' _ _ _) ->
                sesh'
                  == seshName
            )
            running'

-- remove admin window (always)

removeAdminPane :: VSessionName -> [TmuxCommand]
removeAdminPane seshName = pure (KillAdminPane seshName)
