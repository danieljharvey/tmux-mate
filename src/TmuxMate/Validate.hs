module TmuxMate.Validate where

import Data.List.NonEmpty
import TmuxMate.Types

parseSession :: Session -> Either ValidationError ValidatedSession
parseSession sesh = do
  windows <- parseSessionWindows (sessionWindows sesh)
  seshTitle <- parseSessionName (sessionTitle sesh)
  pure $ ValidatedSession
    { vSessionTitle = seshTitle,
      vSessionWindows = windows
    }

parseSessionName :: SessionName -> Either ValidationError VSessionName
parseSessionName (SessionName str) =
  case nonEmpty str of
    Just neStr -> Right (VSessionName neStr)
    _ -> Left EmptySessionName

parseSessionWindows :: [Window] -> Either ValidationError (NonEmpty Window)
parseSessionWindows as =
  case nonEmpty as of
    Just as' -> Right as'

parseWindowName :: WindowName -> Either ValidationError VWindowName
parseWindowName (WindowName str) =
  case nonEmpty str of
    Just neStr -> Right (VWindowName neStr)
    _ -> Left EmptyWindowName

parseWindow :: Window -> Either ValidationError VWindow
parseWindow window = do
  name <- parseWindowName (windowTitle window)
  pure $ VWindow
    { vWindowTitle = name,
      vWindowPanes = windowPanes window
    }
