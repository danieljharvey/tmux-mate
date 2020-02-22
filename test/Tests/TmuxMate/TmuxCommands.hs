{-# LANGUAGE ScopedTypeVariables #-}

module Tests.TmuxMate.TmuxCommands where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.TmuxMate.Types (Session)
import qualified Tests.TmuxMate.Validate as Validate
import TmuxMate
import TmuxMate.Running
import TmuxMate.TmuxCommands
import TmuxMate.Types

spec :: Spec
spec = do
  describe "createSession" $ do
    it "Attaches to session if it exists" $ do
      createSession
        ( InTmuxSession
            (VSessionName $ NE.fromList "horses")
        )
        (VSessionName $ NE.fromList "horses")
        `shouldBe` [AttachToSession (VSessionName $ NE.fromList "horses")]
    it "Creates a session if we are not in tmux" $ do
      createSession NotInTmuxSession (VSessionName $ NE.fromList "horses")
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
        `shouldBe` Just
          ( CreateWindow
              (VSessionName $ NE.fromList "horses")
              (VWindowName $ NE.fromList "window")
          )
    it "Creates a window if there is matching one but it's in another session" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "other")
            (VWindowName $ NE.fromList "window")
            undefined
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` Just
          ( CreateWindow
              (VSessionName $ NE.fromList "horses")
              (VWindowName $ NE.fromList "window")
          )
    it "Does nothing if one already exists" $ do
      createWindow
        (VSessionName $ NE.fromList "horses")
        [ Running
            (VSessionName $ NE.fromList "horses")
            (VWindowName $ NE.fromList "window")
            undefined
            0
        ]
        ( VWindow
            (VWindowName (NE.fromList "window"))
            $ NE.fromList [Pane (PaneCommand "go")]
        )
        `shouldBe` Nothing
  describe "createWindowPanes" $ do
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
        `shouldBe` []
  describe "removeWindowPanes" $ do
    it "Does nothing if nothing running" $ do
      removeWindowPanes
        (VSessionName (NE.fromList "horses"))
        []
        ( NE.fromList
            [ ( VWindow
                  (VWindowName (NE.fromList "window"))
                  $ NE.fromList [Pane (PaneCommand "go")]
              )
            ]
        )
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
        ( NE.fromList
            [ ( VWindow
                  (VWindowName (NE.fromList "window"))
                  $ NE.fromList [Pane (PaneCommand "go")]
              )
            ]
        )
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
        ( NE.fromList
            [ ( VWindow
                  (VWindowName (NE.fromList "window"))
                  $ NE.fromList [Pane (PaneCommand "whoa-no")]
              )
            ]
        )
        `shouldBe` [KillPane (VSessionName (NE.fromList "horses")) 24]
  describe "removeWindows" $ do
    it "Does nothing if no window to remove" $ do
      removeWindows
        (VSessionName (NE.fromList "horses"))
        []
        ( NE.fromList
            [ ( VWindow
                  (VWindowName (NE.fromList "window"))
                  $ NE.fromList [Pane (PaneCommand "go")]
              )
            ]
        )
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
        ( NE.fromList
            [ ( VWindow
                  (VWindowName (NE.fromList "window"))
                  $ NE.fromList
                    [Pane (PaneCommand "go")]
              )
            ]
        )
        `shouldBe` [ KillWindow
                       (VSessionName (NE.fromList "horses"))
                       (VWindowName (NE.fromList "window2"))
                   ]
