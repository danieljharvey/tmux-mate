{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tests.TmuxMate.Types
  ( Session,
  )
where

-- add arbitary types

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import TmuxMate.Types

-- helper

newtype GenericArb a
  = GenericArb {getGenericArb :: a}
  deriving (Generic)

instance (Generic a, Arbitrary a) => Arbitrary (GenericArb a) where
  arbitrary = genericArbitrary

-- helper

instance Arbitrary Session where
  arbitrary = genericArbitrary

instance Arbitrary Window where
  arbitrary = genericArbitrary

instance Arbitrary WindowName where
  arbitrary = oneof [pure (WindowName "window-name")]

instance Arbitrary SessionName where
  arbitrary = oneof [pure (SessionName "session-name")]

instance Arbitrary Pane where
  arbitrary = genericArbitrary

instance Arbitrary PaneTitle where
  arbitrary = oneof [pure (PaneTitle "pane-title")]

instance Arbitrary PaneCommand where
  arbitrary = oneof [pure (PaneCommand "pane-command")]
