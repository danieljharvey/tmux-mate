module TmuxMate.Commands where

import qualified Data.List.NonEmpty as NE
import TmuxMate.Types

sendKeys :: VSessionName -> String -> Command
sendKeys (VSessionName name) str =
  Command $
    "tmux send-keys -t " <> (quoteAndEscape . NE.toList) name <> " "
      <> quoteAndEscape str
      <> " ENTER"

adminPaneName :: String
adminPaneName = quoteAndEscape "tmux-mate-admin"

-- turns our DSL into actual tmux commands
createActualCommand :: TmuxCommand -> [Command]
createActualCommand (CreateAdminPane (VSessionName seshName)) =
  pure $ Command $
    "tmux split-window -v -t "
      <> (quoteAndEscape . NE.toList) seshName
createActualCommand (KillAdminPane seshName) =
  [ Command $ "tmux select-window -t " <> adminPaneName,
    sendKeys seshName "exit"
  ]
createActualCommand (CreatePane _ (VWindowName winName) arrangement newCmd) =
  let windowName' = (quoteAndEscape . NE.toList) winName
   in [ Command $ "tmux select-window -t " <> windowName',
        Command $
          "tmux split-window "
            <> (quoteAndEscape . getCommand) newCmd,
        Command $
          "tmux select-layout -t " <> windowName' <> " "
            <> (showPaneArrangement arrangement)
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
      <> (quoteAndEscape . NE.toList) seshName
createActualCommand (KillSession (VSessionName seshName)) =
  pure $ Command $
    "tmux kill-session -t "
      <> (quoteAndEscape . NE.toList) seshName
createActualCommand (NewSession (VSessionName seshName)) =
  pure $ Command $
    "tmux new-session -d -s "
      <> (quoteAndEscape . NE.toList) seshName
      <> " -n "
      <> adminPaneName
createActualCommand (CreateWindow _ (VWindowName winName) (Command newCmd)) =
  [ Command $
      "tmux new-window -n "
        <> (quoteAndEscape . NE.toList) winName
        <> " "
        <> quoteAndEscape newCmd
  ]
createActualCommand (KillWindow _ (VWindowName winName)) =
  [ Command $
      "tmux kill-window -t "
        <> (quoteAndEscape . NE.toList) winName
  ]

showPaneArrangement :: VPaneArrangement -> String
showPaneArrangement Tiled = "tiled"
showPaneArrangement EvenHorizontal = "even-horizontal"
showPaneArrangement EvenVertical = "even-vertical"
showPaneArrangement MainHorizontal = "main-horizontal"
showPaneArrangement MainVertical = "main-vertical"

quote :: String -> String
quote s = "\"" <> s <> "\""

escape :: String -> String
escape "" = ""
escape ('\"' : t) = "\\\"" <> escape t
escape (x : xs) = x : escape xs

quoteAndEscape :: String -> String
quoteAndEscape = quote . escape
