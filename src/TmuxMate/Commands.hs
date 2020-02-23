module TmuxMate.Commands where

import qualified Data.List.NonEmpty as NE
import TmuxMate.Types

sendKeys :: VSessionName -> String -> Command
sendKeys (VSessionName name) str =
  Command $
    "tmux send-keys -t " <> (NE.toList name) <> " \""
      <> str
      <> "\" ENTER"

adminPaneName :: String
adminPaneName = "tmux-mate-admin"

-- turns our DSL into actual tmux commands
createActualCommand :: TmuxCommand -> [Command]
createActualCommand (CreateAdminPane (VSessionName seshName)) =
  pure $ Command $
    "tmux split-window -v -t "
      <> NE.toList seshName
createActualCommand (KillAdminPane seshName) =
  [ Command $ "tmux select-window -t " <> adminPaneName,
    sendKeys seshName "exit"
  ]
createActualCommand (CreatePane _ (VWindowName winName) newCmd) =
  [ Command $ "tmux select-window -t " <> NE.toList winName,
    Command $
      "tmux split-window "
        <> (getCommand newCmd),
    Command $ "tmux select-layout even-horizontal" -- for now let's stop it filling up
  ]
createActualCommand (KillPane seshName paneIndex) =
  pure $
    sendKeys
      seshName
      ( "tmux kill-pane -t "
          <> show paneIndex
      )
createActualCommand (AttachToSession (VSessionName seshName)) =
  pure $ Command $
    "tmux attach-session -t "
      <> NE.toList seshName
createActualCommand (KillSession (VSessionName seshName)) =
  pure $ Command $
    "tmux kill-session -t "
      <> NE.toList seshName
createActualCommand (NewSession (VSessionName seshName)) =
  pure $ Command $
    "tmux new-session -d -s "
      <> NE.toList seshName
      <> " -n "
      <> adminPaneName
createActualCommand (CreateWindow _ (VWindowName winName) (Command newCmd)) =
  [ Command $
      "tmux new-window -n "
        <> NE.toList winName
        <> " "
        <> newCmd
  ]
createActualCommand (KillWindow _ (VWindowName winName)) =
  [ Command $
      "tmux kill-window -t "
        <> NE.toList winName
  ]
