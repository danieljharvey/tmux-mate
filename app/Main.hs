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

configFilePathParser :: Parser ConfigFilePath
configFilePathParser =
  ConfigFilePath
    <$> argument str (metavar "<path-to-config-file>")

verbosityParser :: Parser Verbosity
verbosityParser =
  flag' Chatty (short 'v' <> long "verbose")
    <|> flag' DryRun (short 'd' <> long "dry-run")
    <|> pure Silent

options :: Parser CLIOptions
options =
  CLIOptions
    <$> configFilePathParser <*> verbosityParser
