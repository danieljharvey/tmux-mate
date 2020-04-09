{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.TmuxCommands
  ( createSession,
    createWindow,
    removeWindowPanes,
    removeWindows,
    attachToSession,
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
        <> (removeWindowPanes runningInTmux sTitle runningPanes sWindows)
        <> (removeWindows runningInTmux sTitle runningPanes sWindows)
        <> ( if needsNewSession runningInTmux sTitle runningSessions
               then removeAdminPane sTitle
               else []
           )
        <> (attachToSession sTitle runningInTmux)

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
        (vWindowArrangement window)
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
          (vWindowArrangement window)
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
createWindowPanes ::
  VSessionName -> VWindowName -> VPaneArrangement -> [Pane] -> [Running] -> [TmuxCommand]
createWindowPanes seshName windowName' arrange panes running' =
  ( \pane ->
      CreatePane
        seshName
        windowName'
        arrange
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

removeWindowPanes :: InTmuxSession -> VSessionName -> [Running] -> [VWindow] -> [TmuxCommand]
removeWindowPanes inTmux seshName running' windows =
  (\(Running _ _ _ i) -> KillPane seshName i)
    <$> (filterRunning inTmux seshName windows running')

filterRunning :: InTmuxSession -> VSessionName -> [VWindow] -> [Running] -> [Running]
filterRunning inTmux seshName windows running' =
  filter
    ( \(Running seshName' winName' run _) ->
        windowMatch winName'
          && ( not $
                 anyMatch (removeQuotes run) windows
                   && seshName == seshName'
             )
    )
    running'
  where
    -- is this even in a window relevant to us?
    windowMatch :: VWindowName -> Bool
    windowMatch winName' = case inTmux of
      NotInTmuxSession -> True
      InTmuxSession _ -> elem winName' (vWindowTitle <$> windows)
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

-- important thing here is whether this is actually our session to run
-- if we're running in tmux we're a guest in another session and we should only
-- edit windows that are in our config file, and leave the rest be.
removeWindows ::
  InTmuxSession ->
  VSessionName ->
  [Running] ->
  [VWindow] ->
  [TmuxCommand]
removeWindows inTmux seshName running' windows =
  case inTmux of
    NotInTmuxSession ->
      ( ( \winTitle' ->
            KillWindow
              seshName
              winTitle'
        )
          <$> filter
            ( \win' ->
                notElem win' requiredWindowNames
            )
            runningWindowNames
      )
    _ -> []
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

-- don't attach to session if we're in a session
attachToSession :: VSessionName -> InTmuxSession -> [TmuxCommand]
attachToSession _ (InTmuxSession _) = []
attachToSession sTitle _ = [AttachToSession sTitle]
