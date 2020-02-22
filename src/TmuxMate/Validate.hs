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

parseSessionWindows :: [Window] -> Either ValidationError (NonEmpty VWindow)
parseSessionWindows as = do
  vWindows <- sequence (parseWindow <$> as)
  case nonEmpty vWindows of
    Just as' -> Right as'
    _ -> Left NoWindows

parseWindowName :: WindowName -> Either ValidationError VWindowName
parseWindowName (WindowName str) =
  case nonEmpty str of
    Just neStr -> Right (VWindowName neStr)
    _ -> Left EmptyWindowName

parseWindowPanes :: VWindowName -> [Pane] -> Either ValidationError (NonEmpty Pane)
parseWindowPanes wName as =
  case nonEmpty as of
    Just as' -> Right as'
    _ -> Left $ WindowWithNoPanes wName

parseWindow :: Window -> Either ValidationError VWindow
parseWindow window = do
  name <- parseWindowName (windowTitle window)
  panes <- parseWindowPanes name (windowPanes window)
  pure $ VWindow
    { vWindowTitle = name,
      vWindowPanes = panes
    }