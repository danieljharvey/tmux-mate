module Main where

import Options.Applicative
import System.Exit
import TmuxMate

main :: IO ()
main = do
  options' <- execParser (info options fullDesc)
  didItWork <- loadTestSession (configPath options')
  case didItWork of
    Yeah -> exitWith ExitSuccess
    Nah i -> exitWith (ExitFailure i)

data Options
  = Options {configPath :: String}
  deriving (Eq, Ord, Show)

options :: Parser Options
options = Options <$> argument str (metavar "<path-to-config-file>")
