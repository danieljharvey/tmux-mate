module Main where

import System.Environment
import System.Exit
import TmuxMate

main :: IO ()
main = do
  path <- lookupEnv "TMUX_MATE_PATH"
  case path of
    Just dhallPath -> do
      didItWork <- loadTestSession dhallPath
      case didItWork of
        Yeah -> exitWith ExitSuccess
        Nah i -> exitWith (ExitFailure i)
    Nothing -> do
      putStrLn "Pass a valid path to TMUX_MATE_PATH pls"
      exitWith (ExitFailure 1)
