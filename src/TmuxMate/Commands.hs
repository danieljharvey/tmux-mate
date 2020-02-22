module TmuxMate.Commands where

import qualified Data.List.NonEmpty as NE
import TmuxMate.Types

sendKeys :: VSessionName -> String -> Command
sendKeys (VSessionName name) str =
  Command $
    "tmux send-keys -t " <> (NE.toList name) <> " \""
      <> str
      <> "\" ENTER"

--

-- turns our DSL into actual tmux commands
createActualCommand :: TmuxCommand -> [Command]
createActualCommand (CreateAdminPane (VSessionName seshName)) =
  pure $ Command $
    "tmux split-window -v -t "
      <> NE.toList seshName
createActualCommand (KillAdminPane seshName) =
  [ Command $ "tmux select-window -t tmux-mate-admin",
    sendKeys seshName "exit"
  ]
createActualCommand (CreatePane seshName (VWindowName winName) cmd) =
  [ Command $ "tmux select-window -t " <> NE.toList winName,
    Command $
      "tmux split-window "
        <> (getCommand cmd),
    Command $ "tmux select-layout even-horizontal" -- for now let's stop it filling up
  ]
createActualCommand (KillPane seshName index) =
  pure $
    sendKeys
      seshName
      ( "tmux kill-pane -t "
          <> show index
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
      <> " -n tmux-mate-admin"
createActualCommand (CreateWindow (VSessionName seshName) (VWindowName winName) (Command cmd)) =
  pure $ Command $
    "tmux new-window -n "
      <> NE.toList winName
      <> " "
      <> cmd
