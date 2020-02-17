module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  path <- lookupEnv "TMUX_MATE_PATH"
  case path of
    Just dhallPath -> loadTestSession dhallPath
    Nothing -> putStrLn "Pass a valid path to TMUX_MATE_PATH pls"
