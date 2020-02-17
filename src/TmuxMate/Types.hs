{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TmuxMate.Types where

import Dhall (Decoder, FromDhall, ToDhall, autoWith)
import GHC.Generics

data IsNewSession
  = NewSession
  | OldSession

data Session
  = Session
      { title :: SessionName,
        panes :: [Pane]
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

newtype PaneCommand
  = PaneCommand {getPaneCommand :: String}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToDhall, FromDhall)

newtype PaneTitle
  = PaneTitle {getPaneTitle :: String}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToDhall, FromDhall)

data Pane
  = Pane
      { paneCommand :: PaneCommand,
        paneTitle :: PaneTitle
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

newtype SessionName
  = SessionName {getSessionName :: String}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromDhall, ToDhall)

data Command
  = Required String
  | Optional String
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

data Running
  = Running
      { sessionName :: String,
        cmd :: String,
        index :: Int
      }
  deriving (Eq, Ord, Show)
