module TmuxMate.Init (createTmuxMateDhall) where

-- where we make a new empty session file

import Data.Text.IO
import Dhall
import Dhall.Core (pretty)
import TmuxMate.Types
  ( Pane (..),
    PaneArrangement (..),
    PaneCommand (..),
    Session (..),
    SessionName (..),
    Window (..),
    WindowName (..),
  )

createTmuxMateDhall :: IO ()
createTmuxMateDhall = do
  let dhallVal = pretty (embed inject defaultSession)
  Data.Text.IO.writeFile "./tmux-mate.dhall" dhallVal

defaultSession :: Session
defaultSession =
  Session
    (SessionName "tmux-mate")
    [ Window
        (WindowName "first")
        [ Pane
            ( PaneCommand "watch echo \"hello from tmux-mate\""
            ),
          Pane
            ( PaneCommand "watch echo \"hello again from tmux-mate\""
            )
        ]
        (PaneArrangement "tiled")
    ]
