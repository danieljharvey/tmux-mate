{-# LANGUAGE ScopedTypeVariables #-}

module Tests.TmuxMate.TmuxCommands where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
import TmuxMate.TmuxCommands
import TmuxMate.Types

sampleSession :: ValidatedSession
sampleSession = ValidatedSession
  { vSessionTitle = VSessionName $ NE.fromList "horses",
    vSessionWindows =
      NE.fromList
        [ VWindow
            { vWindowTitle = VWindowName $ NE.fromList "window",
              vWindowPanes = undefined
            }
        ]
  }

spec :: Spec
spec = do
  describe "createSession" $ do
    it "Does nothing if we're already attached" $ do
      createSession
        ( InTmuxSession
            (VSessionName $ NE.fromList "dogs")
        )
        sampleSession
        []
        `shouldBe` []
    it "Does nothing if session already exists" $ do
      createSession
        NotInTmuxSession
        sampleSession
        [VSessionName $ NE.fromList "horses"]
        `shouldBe` [] -- AttachToSession (VSessionName $ NE.fromList "horses")]
    it "Creates a session if we are not in tmux and session is not running" $ do
      createSession NotInTmuxSession sampleSession []
        `shouldBe` [NewSession (VSessionName $ NE.fromList "horses")]
  describe "createWindow" $ do
    it "Creates a window if needed" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        []
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` pure
          ( CreateWindow
              (VSessionName $ NE.fromList "horses")
              (VWindowName $ NE.fromList "window")
              (Command "go")
          )
    it "Creates a window if there is matching one but it's in another session" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "other")
            (VWindowName $ NE.fromList "window")
            (PaneCommand "go")
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` [ ( CreateWindow
                         (VSessionName $ NE.fromList "horses")
                         (VWindowName $ NE.fromList "window")
                         (Command "go")
                     )
                   ]
    it "Does nothing if one already exists" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "horses")
            (VWindowName $ NE.fromList "window")
            (PaneCommand "go")
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` []
  {-describe "createWindowPanes" $ do
    it "Creates one if nothing is there" $ do
      createWindowPanes
        (VSessionName $ NE.fromList "horses")
        []
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` [ CreatePane
                       (VSessionName $ NE.fromList "horses")
                       (VWindowName $ NE.fromList "window")
                       (Command "go")
                   ]
    it "Creates one if something matches in another window" $ do
      createWindowPanes
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "horses")
            (VWindowName $ NE.fromList "different-window")
            (PaneCommand "go")
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` [ CreatePane
                       (VSessionName $ NE.fromList "horses")
                       (VWindowName $ NE.fromList "window")
                       (Command "go")
                   ]
  it "Does nothing if one exists" $ do
    createWindowPanes
      (VSessionName (NE.fromList "horses"))
      [ Running
          (VSessionName (NE.fromList "horses"))
          (VWindowName (NE.fromList "window"))
          (PaneCommand "go")
          0
      ]
      ( VWindow
          (VWindowName (NE.fromList "window"))
          $ NE.fromList [Pane (PaneCommand "go")]
      )
      `shouldBe` []-}
  describe "removeWindowPanes" $ do
    it "Does nothing if nothing running" $ do
      removeWindowPanes
        (VSessionName (NE.fromList "horses"))
        []
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "go")]
          )
        ]
        `shouldBe` []
    it "Does nothing if running pane is still needed" $ do
      removeWindowPanes
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "go")
            0
        ]
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "go")]
          )
        ]
        `shouldBe` []
    it "Creates a remove events if pane is no longer needed" $ do
      removeWindowPanes
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "different-window"))
            (PaneCommand "go")
            24
        ]
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "whoa-no")]
          )
        ]
        `shouldBe` [KillPane (VSessionName (NE.fromList "horses")) 24]
  describe "removeWindows" $ do
    it "Does nothing if no window to remove" $ do
      removeWindows
        (VSessionName (NE.fromList "horses"))
        []
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "go")]
          )
        ]
        `shouldBe` []
    it "Should remove a window if it's no longer needed" $ do
      removeWindows
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window2"))
            (PaneCommand "no")
            10
        ]
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList
                [Pane (PaneCommand "go")]
          )
        ]
        `shouldBe` [ KillWindow
                       (VSessionName (NE.fromList "horses"))
                       (VWindowName (NE.fromList "window2"))
                   ]
