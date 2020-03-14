module Main where

import Options.Applicative
import System.Exit
import TmuxMate

main :: IO ()
main = do
  options' <- execParser (info options fullDesc)
  didItWork <- loadTestSession options'
  case didItWork of
    Yeah -> exitWith ExitSuccess
    Nah i -> exitWith (ExitFailure i)

options :: Parser CLIOptions
options =
  CLIOptions
    <$> ( ConfigFilePath
            <$> argument str (metavar "<path-to-config-file>")
        )
    <*> flag Silent Chatty (short 'v')
