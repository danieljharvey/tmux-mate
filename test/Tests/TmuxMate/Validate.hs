module Tests.TmuxMate.Validate where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
import TmuxMate.Types
import TmuxMate.Validate

spec :: Spec
spec = do
  describe "Validating Session to VSession" $ do
    it "Fails on an empty name" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "",
                sessionWindows =
                  [ Window
                      { windowTitle = WindowName "OK",
                        windowPanes = [Pane {paneCommand = PaneCommand ""}],
                        windowArrangement = Tiled
                      }
                  ]
              }
      parseSession sesh
        `shouldBe` Left EmptySessionName
    it "Fails on just a newline" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "\n",
                sessionWindows =
                  [ Window
                      { windowTitle = WindowName "OK",
                        windowPanes = [Pane {paneCommand = PaneCommand ""}],
                        windowArrangement = Tiled
                      }
                  ]
              }
      parseSession sesh
        `shouldBe` Left EmptySessionName
    it "Fails with no windows" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "Whoa",
                sessionWindows = []
              }
      parseSession sesh
        `shouldBe` Left NoWindows
    it "Fails with empty window name" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "Whoa",
                sessionWindows =
                  [ Window
                      { windowTitle = WindowName "",
                        windowPanes = [Pane {paneCommand = PaneCommand ""}],
                        windowArrangement = Tiled
                      }
                  ]
              }
      parseSession sesh
        `shouldBe` Left EmptyWindowName
    it "Fails on a newline" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "Whoa",
                sessionWindows =
                  [ Window
                      { windowTitle = WindowName "\n",
                        windowPanes = [Pane {paneCommand = PaneCommand ""}],
                        windowArrangement = Tiled
                      }
                  ]
              }
      parseSession sesh
        `shouldBe` Left EmptyWindowName
    it "Fails with no window panes" $ do
      let sesh =
            Session
              { sessionTitle = SessionName "Whoa",
                sessionWindows =
                  [ Window
                      { windowTitle =
                          WindowName "empty-boy",
                        windowPanes = [],
                        windowArrangement = Tiled
                      }
                  ]
              }
      parseSession sesh
        `shouldBe` Left
          ( WindowWithNoPanes
              (VWindowName $ NE.fromList "empty-boy")
          )
