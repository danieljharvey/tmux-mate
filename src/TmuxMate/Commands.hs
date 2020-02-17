module TmuxMate.Commands (startSession, addPane, attach, removePane) where

import TmuxMate.Types

-- trash session in case it's already there
startSession :: IsNewSession -> SessionName -> [Command]
startSession isNew seshTitle =
  case isNew of
    NewSession -> [newSession seshTitle]
    OldSession -> [createAdminWindow seshTitle]

addPane :: SessionName -> Pane -> [Command]
addPane name (Pane command title) =
  [ sendKeys name ("tmux split-window -h -d " <> (getPaneCommand command))
  ]

removePane :: SessionName -> Running -> [Command]
removePane name (Running _ _ index) =
  [ sendKeys name ("tmux kill-pane -t " <> show index)
  ]

attach :: IsNewSession -> SessionName -> [Command]
attach isNew seshTitle =
  case isNew of
    NewSession ->
      [ killAdminWindow seshTitle,
        attachToSession seshTitle
      ]
    OldSession -> [killAdminWindow seshTitle]

sendKeys :: SessionName -> String -> Command
sendKeys (SessionName name) str =
  Required $
    "tmux send-keys -t " <> name <> " \""
      <> str
      <> "\" ENTER"

--

attachToSession :: SessionName -> Command
attachToSession (SessionName name) =
  Required $ "tmux attach-session -t " <> name

killSession :: SessionName -> Command
killSession (SessionName name) =
  Optional $ "tmux kill-session -t " <> name

newSession :: SessionName -> Command
newSession (SessionName name) =
  Required $ "tmux new-session -d -s " <> name

createAdminWindow :: SessionName -> Command
createAdminWindow (SessionName name) =
  Required $ "tmux split-window -v -t " <> name

killAdminWindow :: SessionName -> Command
killAdminWindow name =
  sendKeys name "exit"
