{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TmuxMate.Types where

import Data.List.NonEmpty
import Dhall (Decoder, FromDhall, ToDhall, autoWith)
import GHC.Generics

data InTmuxSession
  = InTmuxSession VSessionName
  | NotInTmuxSession
  deriving (Eq, Ord, Show)

data TmuxState
  = TmuxState
      { inSession :: InTmuxSession,
        running :: [Running],
        sessions :: [VSessionName]
      }
  deriving (Eq, Ord, Show)

data IsNewSession
  = IsNewSession
  | IsOldSession

data Session
  = Session
      { sessionTitle :: SessionName,
        sessionWindows :: [Window]
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

data Window
  = Window
      { windowTitle :: WindowName,
        windowPanes :: [Pane]
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

newtype PaneCommand
  = PaneCommand {getPaneCommand :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToDhall, FromDhall)

data Pane
  = Pane
      { paneCommand :: PaneCommand
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

newtype SessionName
  = SessionName {getSessionName :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromDhall, ToDhall)

newtype WindowName
  = WindowName {getWindowName :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromDhall, ToDhall)

data TmuxCommand
  = CreateAdminPane VSessionName
  | KillAdminPane VSessionName
  | CreatePane VSessionName VWindowName Command
  | KillPane VSessionName Int
  | CreateWindow VSessionName VWindowName Command
  | KillWindow VSessionName VWindowName
  | AttachToSession VSessionName
  | KillSession VSessionName
  | NewSession VSessionName
  | SendKeys VSessionName String
  deriving (Eq, Ord, Show, Generic)

newtype Command
  = Command {getCommand :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromDhall, ToDhall)

data Running
  = Running
      { sessionName :: VSessionName,
        windowName :: VWindowName,
        cmd :: PaneCommand,
        index :: Int
      }
  deriving (Eq, Ord, Show)

data ValidationError
  = EmptySessionName
  | NoWindows
  | EmptyWindowName
  | WindowWithNoPanes VWindowName
  deriving (Eq, Ord, Show)

newtype VSessionName
  = VSessionName {getVSessionName :: NonEmpty Char}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)

data ValidatedSession
  = ValidatedSession
      { vSessionTitle :: VSessionName,
        vSessionWindows :: NonEmpty VWindow
      }
  deriving (Eq, Ord, Show, Generic)

newtype VWindowName
  = VWindowName {getVWindowName :: NonEmpty Char}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)

data VWindow
  = VWindow
      { vWindowTitle :: VWindowName,
        vWindowPanes :: NonEmpty Pane
      }
  deriving (Eq, Ord, Show, Generic)
