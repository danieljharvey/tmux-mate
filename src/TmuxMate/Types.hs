{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TmuxMate.Types where

import Data.List.NonEmpty
import Dhall (FromDhall, ToDhall)
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

newtype PaneArrangement
  = PaneArrangement {getPaneArrangement :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToDhall, FromDhall)

data Session
  = Session
      { sessionTitle :: SessionName,
        sessionWindows :: [Window]
      }
  deriving (Eq, Ord, Show, Generic, FromDhall, ToDhall)

data Window
  = Window
      { windowTitle :: WindowName,
        windowPanes :: [Pane],
        windowArrangement :: PaneArrangement
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
  | CreatePane VSessionName VWindowName VPaneArrangement Command
  | KillPane VSessionName Int
  | CreateWindow VSessionName VWindowName Command
  | KillWindow VSessionName VWindowName
  | AttachToSession VSessionName
  | KillSession VSessionName
  | NewSession VSessionName
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
  deriving (Eq, Ord)

instance Show ValidationError where
  show EmptySessionName = "Session title must not be an empty string."
  show NoWindows = "Session must contain at least one window."
  show EmptyWindowName = "All windows must have a non-empty title."
  show (WindowWithNoPanes (VWindowName name)) =
    "Window '"
      <> toList name
      <> "' does not have any panes! All windows must contain at least one pane."

-- helper for nice Show instance
newtype NicelyPrintedNonEmpty
  = NicelyPrintedNonEmpty (NonEmpty Char)

instance Show NicelyPrintedNonEmpty where
  show (NicelyPrintedNonEmpty nec) = toList nec

-- Validated datatypes

newtype VSessionName
  = VSessionName {getVSessionName :: NonEmpty Char}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via NicelyPrintedNonEmpty

data VPaneArrangement
  = EvenHorizontal
  | EvenVertical
  | MainHorizontal
  | MainVertical
  | Tiled
  deriving (Eq, Ord, Show, Generic)

data ValidatedSession
  = ValidatedSession
      { vSessionTitle :: VSessionName,
        vSessionWindows :: NonEmpty VWindow
      }
  deriving (Eq, Ord, Show, Generic)

newtype VWindowName
  = VWindowName {getVWindowName :: NonEmpty Char}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via NicelyPrintedNonEmpty

data VWindow
  = VWindow
      { vWindowTitle :: VWindowName,
        vWindowPanes :: NonEmpty Pane,
        vWindowArrangement :: VPaneArrangement
      }
  deriving (Eq, Ord, Show, Generic)

-------

data Verbosity
  = Silent
  | Chatty
  | DryRun
  deriving (Eq, Ord, Show)

newtype ConfigFilePath
  = ConfigFilePath {getConfigFilePath :: String}
  deriving (Eq, Ord, Show)

data CLIOptions
  = CLIOptions
      { configFilePath :: ConfigFilePath,
        verbosity :: Verbosity
      }
  deriving (Eq, Ord, Show)
