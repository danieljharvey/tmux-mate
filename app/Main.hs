module Main where

import Options.Applicative ((<|>))
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

configFilePathParser :: Opt.Parser ConfigFilePath
configFilePathParser =
  ConfigFilePath
    <$> Opt.argument Opt.str (Opt.metavar "<path-to-config-file>")

verbosityParser :: Opt.Parser Verbosity
verbosityParser =
  Opt.flag' Chatty (Opt.short 'v' <> Opt.long "verbose")
    <|> Opt.flag' DryRun (Opt.short 'd' <> Opt.long "dry-run")
    <|> pure Silent

options :: Opt.Parser CLIOptions
options =
  CLIOptions
    <$> configFilePathParser <*> verbosityParser

command :: Opt.Parser CLICommand
command =
  otherCommands
    <|> (CLIRun <$> options)

otherCommands :: Opt.Parser CLICommand
otherCommands =
  Opt.subparser
    ( Opt.command
        "init"
        ( Opt.info
            (pure CLIInit)
            (Opt.progDesc "Initialise a new tmux-mate.dhall file")
        )
    )
