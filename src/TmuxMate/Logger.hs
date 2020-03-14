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

logger :: Verbosity -> Colour -> String -> IO ()
logger _ Error msg = redMessage (T.pack msg)
logger Silent _ _ = pure ()
logger Chatty colour msg =
  case colour of
    Highlight -> whiteMessage (T.pack msg)
    Error -> redMessage (T.pack msg)
    Info -> magentaMessage (T.pack msg)
