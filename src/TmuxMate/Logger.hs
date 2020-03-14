{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TmuxMate.Logger
  ( logger,
  )
where

import TmuxMate.Types

logger :: Verbosity -> String -> IO ()
logger Silent = const $ pure ()
logger Chatty = putStrLn
