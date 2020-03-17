{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.Logger
  ( logger,
    Colour (..),
  )
where

import Colourista.IO
import qualified Data.Text as T
import TmuxMate.Types

data Colour
  = Error
  | Info
  | Highlight

outputMsg :: Colour -> String -> IO ()
outputMsg colour msg =
  case colour of
    Highlight -> whiteMessage (T.pack msg)
    Error -> redMessage (T.pack msg)
    Info -> magentaMessage (T.pack msg)

logger :: Verbosity -> Colour -> String -> IO ()
logger _ Error msg = redMessage (T.pack msg)
logger Silent _ _ = pure ()
logger DryRun c m = outputMsg c m
logger Chatty c m = outputMsg c m
