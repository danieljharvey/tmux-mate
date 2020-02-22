module Tests.TmuxMate.Validate where

import Dhall
import Dhall.Core (pretty)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.TmuxMate.Types (Session)
import TmuxMate
import TmuxMate.Running
import TmuxMate.Types
  ( Command (..),
    InTmuxSession (..),
    Pane (..),
    PaneCommand (..),
    PaneTitle (..),
    Running (..),
    Session (..),
    SessionName (..),
    TmuxCommand (..),
    ValidationError (..),
    Window (..),
    WindowName (..),
  )
import TmuxMate.Validate

spec :: Spec
spec = do
  describe "Validating Session to VSession" $ do
    it "Fails on an empty name" $ do
      let sesh = Session
            { sessionTitle = SessionName "",
              sessionWindows = []
            }
      parseSession sesh
        `shouldBe` Left EmptySessionName
    it "Fails with no windows" $ do
      let sesh = Session
            { sessionTitle = SessionName "Whoa",
              sessionWindows = []
            }
      parseSession sesh
        `shouldBe` Left NoWindows
    it "Fails with empty window name" $ do
      let sesh = Session
            { sessionTitle = SessionName "Whoa",
              sessionWindows =
                [ Window
                    { windowTitle = WindowName "",
                      windowPanes = [Pane {paneCommand = PaneCommand "", paneTitle = PaneTitle ""}]
                    }
                ]
            }
      parseSession sesh
        `shouldBe` Left EmptyWindowName
    it "Fails with no windows" $ do
      let sesh = Session
            { sessionTitle = SessionName "Whoa",
              sessionWindows = [Window {windowTitle = WindowName "", windowPanes = []}]
            }
      parseSession sesh
        `shouldBe` Left NoWindows
