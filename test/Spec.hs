{-# LANGUAGE ScopedTypeVariables #-}

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
    Pane (..),
    PaneCommand (..),
    Running (..),
    SessionName (..),
    TmuxCommand (..),
    Window (..),
    WindowName (..),
  )

main :: IO ()
main = hspec $ do
  describe "createSession" $ do
    it "Creates a session if needed" $ do
      createSession
        (SessionName "horses")
        []
        `shouldBe` Just
          (NewSession (SessionName "horses"))
    it "Does nothing if one already exists" $ do
      createSession
        (SessionName "horses")
        [Running (SessionName "horses") undefined undefined 0]
        `shouldBe` Nothing
  describe "createWindow" $ do
    it "Creates a window if needed" $ do
      createWindow
        (SessionName "horses")
        []
        (WindowName "window")
        `shouldBe` Just
          (CreateWindow (SessionName "horses") (WindowName "window"))
    it "Creates a window if there is matching one but it's in another session" $ do
      createWindow
        (SessionName "horses")
        [Running (SessionName "other") (WindowName "window") undefined 0]
        (WindowName "window")
        `shouldBe` Just
          (CreateWindow (SessionName "horses") (WindowName "window"))
    it "Does nothing if one already exists" $ do
      createWindow
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "window") undefined 0]
        (WindowName "window")
        `shouldBe` Nothing
  describe "createWindowPanes" $ do
    it "Creates one if nothing is there" $ do
      createWindowPanes
        (SessionName "horses")
        []
        ( Window
            (WindowName "window")
            [Pane (PaneCommand "go") undefined]
        )
        `shouldBe` [CreatePane (SessionName "horses") (WindowName "window") (Command "go")]
    it "Creates one if something matches in another window" $ do
      createWindowPanes
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "different-window") (PaneCommand "go") 0]
        ( Window
            (WindowName "window")
            [Pane (PaneCommand "go") undefined]
        )
        `shouldBe` [CreatePane (SessionName "horses") (WindowName "window") (Command "go")]
    it "Does nothing if one exists" $ do
      createWindowPanes
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "window") (PaneCommand "go") 0]
        ( Window
            (WindowName "window")
            [Pane (PaneCommand "go") undefined]
        )
        `shouldBe` []
  describe "removeWindowPanes" $ do
    it "Does nothing if nothing running" $ do
      removeWindowPanes
        (SessionName "horses")
        []
        []
        `shouldBe` []
    it "Does nothing if running pane is still needed" $ do
      removeWindowPanes
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "window") (PaneCommand "go") 0]
        [ ( Window
              (WindowName "window")
              [Pane (PaneCommand "go") undefined]
          )
        ]
        `shouldBe` []
    it "Creates a remove events if pane is no longer needed" $ do
      removeWindowPanes
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "different-window") (PaneCommand "go") 24]
        []
        `shouldBe` [KillPane (SessionName "horses") 24]
  describe "removeWindows" $ do
    it "Does nothing if no window to remove" $ do
      removeWindows
        (SessionName "horses")
        []
        [ ( Window
              (WindowName "window")
              [Pane (PaneCommand "go") undefined]
          )
        ]
        `shouldBe` []
    it "Should remove a window if it's no longer needed" $ do
      removeWindows
        (SessionName "horses")
        [Running (SessionName "horses") (WindowName "window2") (PaneCommand "no") 10]
        [ ( Window
              (WindowName "window")
              [Pane (PaneCommand "go") undefined]
          )
        ]
        `shouldBe` [KillWindow (SessionName "horses") (WindowName "window2")]
  describe "ParseRunning" $ do
    it "Rejects nonsense" $ do
      parseSingle "sdfdsf" `shouldBe` Nothing
    it "Accepts goodness" $ do
      parseSingle "foo:bar:1:yes Pane 1"
        `shouldBe` Just
          ( Running
              (SessionName "foo")
              (WindowName "bar")
              (PaneCommand "yes Pane 1")
              1
          )
    it "Accepts goodness with double colons inside" $ do
      parseSingle "foo:bar:1:yes Pane 1:2"
        `shouldBe` Just
          ( Running
              (SessionName "foo")
              (WindowName "bar")
              (PaneCommand "yes Pane 1:2")
              1
          )
    it "returns the original number when given a positive input" $
      parseRunning (SessionName "foo") "0:0:\nfoo:bar:0:yes Pane 2\nfoo:bar:1:yes Pane 1\n"
        `shouldBe` [ Running
                       (SessionName "foo")
                       (WindowName "bar")
                       (PaneCommand "yes Pane 2")
                       0,
                     Running
                       (SessionName "foo")
                       (WindowName "bar")
                       (PaneCommand "yes Pane 1")
                       1
                   ]
  describe "Dhall" $ do
    it "Round trips Dhall encoding" $ do
      property dhallSessionRoundtrip

dhallSessionRoundtrip :: Property
dhallSessionRoundtrip =
  monadicIO $ do
    (sesh :: Session) <- pick arbitrary
    let dhallVal = pretty (embed inject sesh)
    let (decoder :: Decoder Session) = auto
    decoded <- run $ input decoder dhallVal
    assert $ decoded == sesh
