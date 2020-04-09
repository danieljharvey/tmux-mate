{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Dhall
import Dhall.Core (pretty)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Tests.TmuxMate.TmuxCommands as TmuxCommands
import Tests.TmuxMate.Types (Session)
import qualified Tests.TmuxMate.Validate as Validate
import TmuxMate.Running
import TmuxMate.Types

main :: IO ()
main = hspec $ do
  Validate.spec
  TmuxCommands.spec
  describe "ParseRunning" $ do
    it "Rejects nonsense" $ do
      parseSingle "sdfdsf" `shouldBe` Nothing
    it "Accepts goodness" $ do
      parseSingle "foo:bar:1:yes Pane 1"
        `shouldBe` Just
          ( Running
              (VSessionName (NE.fromList "foo"))
              (VWindowName (NE.fromList "bar"))
              (PaneCommand "yes Pane 1")
              1
          )
    it "Accepts goodness with double colons inside" $ do
      parseSingle "foo:bar:1:yes Pane 1:2"
        `shouldBe` Just
          ( Running
              (VSessionName (NE.fromList "foo"))
              (VWindowName (NE.fromList "bar"))
              (PaneCommand "yes Pane 1:2")
              1
          )
    it "returns the original number when given a positive input" $
      parseRunning
        "0:0:\nfoo:bar:0:yes Pane 2\nfoo:bar:1:yes Pane 1\n"
        `shouldBe` [ Running
                       (VSessionName (NE.fromList "foo"))
                       (VWindowName (NE.fromList "bar"))
                       (PaneCommand "yes Pane 2")
                       0,
                     Running
                       (VSessionName (NE.fromList "foo"))
                       (VWindowName (NE.fromList "bar"))
                       (PaneCommand "yes Pane 1")
                       1
                   ]
  describe "Dhall" $ do
    it "Round trips Dhall encoding" $ do
      property dhallSessionRoundtrip
    it "Generates a Dhall schema that matches our advertised one" $ do
      let schema = (Dhall.Core.pretty (Dhall.expected (Dhall.auto @Session)))
      savedSchema <- Text.IO.readFile "./samples/Schema.dhall"
      when
        (Text.stripEnd schema /= Text.stripEnd savedSchema)
        (Text.IO.putStrLn schema)
      Text.stripEnd schema `shouldBe` Text.stripEnd savedSchema

dhallSessionRoundtrip :: Property
dhallSessionRoundtrip =
  monadicIO $ do
    (sesh :: Session) <- pick arbitrary
    let dhallVal = pretty (embed inject sesh)
    let (decoder :: Decoder Session) = auto
    decoded <- run $ input decoder dhallVal
    assert $ decoded == sesh
