{-# LANGUAGE ScopedTypeVariables #-}

import Dhall
import Dhall.Core (pretty)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Tests.TmuxMate.Types (Session)
import TmuxMate.Running
import TmuxMate.Types (Running (..), SessionName (..))

--quickCheck dhallSessionRoundtrip

main :: IO ()
main = hspec $ do
  describe "ParseRunning" $ do
    it "Rejects nonsense" $ do
      parseSingle "sdfdsf" `shouldBe` Nothing
    it "Accepts goodness" $ do
      parseSingle "foo:1:yes Pane 1" `shouldBe` Just (Running "foo" "yes Pane 1" 1)
    it "Accepts goodness with double colons inside" $ do
      parseSingle "foo:1:yes Pane 1:2" `shouldBe` Just (Running "foo" "yes Pane 1:2" 1)
    it "returns the original number when given a positive input" $
      parseRunning (SessionName "foo") "0:0:\nfoo:0:yes Pane 2\nfoo:1:yes Pane 1\n"
        `shouldBe` [Running "foo" "yes Pane 2" 0, Running "foo" "yes Pane 1" 1]
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
