module Tests.TmuxMate.Validate where

import qualified Data.List.NonEmpty as NE
import Dhall
import Dhall.Core (pretty)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.TmuxMate.Types (Session)
import TmuxMate
import TmuxMate.Running
import TmuxMate.Types
import TmuxMate.Validate

spec :: Spec
spec = do
  describe "Validating Session to VSession" $ do
    it "Fails on an empty name" $ do
      let sesh = Session
            { sessionTitle = SessionName "",
              sessionWindows =
                [ Window
                    { windowTitle = WindowName "OK",
                      windowPanes = [Pane {paneCommand = PaneCommand "", paneTitle = PaneTitle ""}]
                    }
                ]
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
    it "Fails with no window panes" $ do
      let sesh = Session
            { sessionTitle = SessionName "Whoa",
              sessionWindows = [Window {windowTitle = WindowName "empty-boy", windowPanes = []}]
            }
      parseSession sesh
        `shouldBe` Left (WindowWithNoPanes (VWindowName $ NE.fromList "empty-boy"))
