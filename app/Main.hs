module Main where

import CLICommands
import Options (command)
import qualified Options.Applicative as Opt
import System.Exit
import TmuxMate

main :: IO ()
main = do
  command' <- Opt.execParser (Opt.info command Opt.fullDesc)
  case command' of
    CLIInit -> do
      createTmuxMateDhall
      putStrLn "Initial tmux-mate.dhall created!"
    CLIRun options' -> do
      didItWork <- loadTestSession options'
      case didItWork of
        Yeah -> exitWith ExitSuccess
        Nah i -> exitWith (ExitFailure i)
