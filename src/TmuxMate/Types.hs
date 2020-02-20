{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TmuxMate.Types where

import Dhall (Decoder, FromDhall, ToDhall, autoWith)
import GHC.Generics

data InTmuxSession
  = InTmuxSession SessionName
  | NotInTmuxSession
  deriving (Eq, Ord, Show)

data TmuxState
  = TmuxState
      { inSession :: InTmuxSession,
        running :: [Running],
        sessions :: [SessionName]
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

newtype PaneTitle
  = PaneTitle {getPaneTitle :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToDhall, FromDhall)

data Pane
  = Pane
      { paneCommand :: PaneCommand,
        paneTitle :: PaneTitle
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
  = CreateAdminPane SessionName
  | KillAdminPane SessionName
  | CreatePane SessionName WindowName Command
  | KillPane SessionName Int
  | CreateWindow SessionName WindowName
  | KillWindow SessionName WindowName
  | AttachToSession SessionName
  | SwitchToSession SessionName
  | KillSession SessionName
  | NewSession SessionName
  | SendKeys SessionName String
  deriving (Eq, Ord, Show, Generic)

newtype Command
  = Command {getCommand :: String}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, FromDhall, ToDhall)

data Running
  = Running
      { sessionName :: SessionName,
        windowName :: WindowName,
        cmd :: PaneCommand,
        index :: Int
      }
  deriving (Eq, Ord, Show)
