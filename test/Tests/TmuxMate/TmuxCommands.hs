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
    it "Adds second pane to existing window" $ do
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
            $ NE.fromList [Pane (PaneCommand "go"), Pane (PaneCommand "whoa")]
        )
        `shouldBe` [ CreatePane
                       (VSessionName $ NE.fromList "horses")
                       (VWindowName $ NE.fromList "window")
                       (Command "whoa")
                   ]
    it "Creates a pane if something matches, but it's in another window" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "horses")
            (VWindowName $ NE.fromList "different-window")
            (PaneCommand "go")
            0,
          Running
            (VSessionName $ NE.fromList "horses")
            (VWindowName $ NE.fromList "window")
            (PaneCommand "no")
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
    it "Ignores panes that already exist" $ do
      createWindow
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "go")
            0,
          Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "yo")
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go"), Pane (PaneCommand "yo")]
        )
        `shouldBe` []
  describe "removeWindowPanes" $ do
    it "Does nothing if nothing running" $ do
      removeWindowPanes
        NotInTmuxSession
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
        NotInTmuxSession
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
        NotInTmuxSession
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "go")
            24
        ]
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "whoa-no")]
          )
        ]
        `shouldBe` [KillPane (VSessionName (NE.fromList "horses")) 24]
    it "Does not remove panes from windows that are nothing to do with us if we do not control the session" $ do
      removeWindowPanes
        (InTmuxSession (VSessionName $ NE.fromList "horses"))
        (VSessionName (NE.fromList "horses"))
        [ Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "different-window"))
            (PaneCommand "leave me")
            27,
          Running
            (VSessionName (NE.fromList "horses"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "get rid of me")
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
        NotInTmuxSession
        (VSessionName (NE.fromList "horses"))
        []
        [ ( VWindow
              (VWindowName (NE.fromList "window"))
              $ NE.fromList [Pane (PaneCommand "go")]
          )
        ]
        `shouldBe` []
    it "Should remove a window if it's no longer needed in a hosted session" $ do
      removeWindows
        NotInTmuxSession
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
    it "Should leave a window that's no longer needed if we are not the session host" $ do
      removeWindows
        (InTmuxSession (VSessionName $ NE.fromList "bruce"))
        (VSessionName (NE.fromList "bruce"))
        [ Running
            (VSessionName (NE.fromList "bruce"))
            (VWindowName (NE.fromList "window"))
            (PaneCommand "go")
            10,
          Running -- this one should be deleted really
            (VSessionName (NE.fromList "bruce"))
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
        `shouldBe` []
