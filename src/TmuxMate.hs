{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate
  ( loadTestSession,
    createSession,
    createWindow,
    createWindowPanes,
    removeWindowPanes,
    removeWindows,
    attachToSession,
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

getCommands :: [TmuxCommand] -> [Command]
getCommands =
  concatMap createActualCommand

getTmuxCommands :: Session -> TmuxState -> [TmuxCommand]
getTmuxCommands sesh tmuxState =
  let runningPanes =
        running tmuxState
      runningSessions =
        sessions tmuxState
      runningInTmux =
        inSession tmuxState
      sTitle =
        sessionTitle sesh
      sWindows =
        sessionWindows sesh
   in (createSession runningInTmux sTitle runningSessions)
        <> ( concatMap
               (maybeToList . createWindow sTitle runningPanes)
               (windowTitle <$> sWindows)
           )
        <> ( concatMap
               (createWindowPanes sTitle runningPanes)
               (sessionWindows sesh)
           )
        <> (removeWindowPanes sTitle runningPanes sWindows)
        <> (removeWindows sTitle runningPanes sWindows)
        <> (removeAdminPane sTitle)
        <> (attachToSession runningInTmux sTitle runningSessions)

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
            },
          Window
            { windowTitle = WindowName "second-window",
              windowPanes =
                [ Pane (PaneCommand "yes 'Second Window - Pane 1'") (PaneTitle "2One"),
                  Pane (PaneCommand "yes 'Second Window - Pane 2'") (PaneTitle "2Two"),
                  Pane (PaneCommand "yes 'Second Window - Pane 3'") (PaneTitle "2Three"),
                  Pane (PaneCommand "yes 'Second Window - Pane 4'") (PaneTitle "2Four")
                ]
            }
        ]
    }

-- create a new session if required
createSession :: InTmuxSession -> SessionName -> [SessionName] -> [TmuxCommand]
createSession inTmux seshName runningSessions =
  if sessionExists seshName runningSessions
    then case inTmux of
      InTmuxSession _ -> [SwitchToSession seshName]
      NotInTmuxSession -> []
    else case inTmux of
      InTmuxSession _ -> [NewSession seshName, SwitchToSession seshName]
      NotInTmuxSession -> [NewSession seshName]

sessionExists :: SessionName -> [SessionName] -> Bool
sessionExists = elem

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
attachToSession :: InTmuxSession -> SessionName -> [SessionName] -> [TmuxCommand]
attachToSession inTmux seshName runningSessions =
  case inTmux of
    InTmuxSession sesh -> [SwitchToSession sesh]
    _ -> [AttachToSession seshName]

-- remove admin window (always)

removeAdminPane :: SessionName -> [TmuxCommand]
removeAdminPane seshName = pure (KillAdminPane seshName)

---- load dhall file and let's get cracking

loadTestSession :: FilePath -> IO ()
loadTestSession path = do
  --let (decoder :: Dhall.Decoder Session) = Dhall.auto
  --- config <- Dhall.inputFile decoder path
  let config = testSession
  tmuxState <- askTmuxState (sessionTitle config)
  print tmuxState
  let tmuxCommands = getTmuxCommands config tmuxState
  putStrLn "Tmux Commands"
  print tmuxCommands
  let commands = getCommands tmuxCommands
  putStrLn "Shell commands"
  print commands
  runCommands commands
