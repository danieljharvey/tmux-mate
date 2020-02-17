module TmuxMate.Commands where

import TmuxMate.Types

sendKeys :: SessionName -> String -> Command
sendKeys (SessionName name) str =
  Command $
    "tmux send-keys -t " <> name <> " \""
      <> str
      <> "\" ENTER"

--

-- turns our DSL into actual tmux commands
createActualCommand :: TmuxCommand -> [Command]
createActualCommand (CreateAdminPane (SessionName seshName)) =
  pure $ Command $ "tmux split-window -v -t " <> seshName
createActualCommand (KillAdminPane seshName) =
  pure $ sendKeys seshName "exit"
createActualCommand (CreatePane seshName winName cmd) =
  pure $ sendKeys seshName ("tmux split-window -h -d " <> (getCommand cmd))
createActualCommand (KillPane seshName index) =
  pure $ sendKeys seshName ("tmux kill-pane -t " <> show index)
createActualCommand (AttachToSession (SessionName seshName)) =
  pure $ Command $ "tmux attach-session -t " <> seshName
createActualCommand (SwitchToSession (SessionName seshName)) =
  pure $ Command $ "tmux switch -t " <> seshName
createActualCommand (KillSession (SessionName seshName)) =
  pure $ Command $ "tmux kill-session -t " <> seshName
createActualCommand (NewSession (SessionName seshName)) =
  pure $ Command $ "tmux new-session -d -s " <> seshName
createActualCommand (CreateWindow (SessionName seshName) (WindowName winName)) =
  pure $ Command $ "tmux new-window -n '" <> seshName <> "'"
