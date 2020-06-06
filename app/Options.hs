module Options (command) where

import CLICommands
import Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import TmuxMate

command :: Opt.Parser CLICommand
command =
  otherCommands
    <|> (CLIRun <$> options)

configFilePathParser :: Opt.Parser (Maybe ConfigFilePath)
configFilePathParser =
  ( Just <$> ConfigFilePath
      <$> Opt.argument Opt.str (Opt.metavar "<path-to-config-file>")
  )
    <|> pure Nothing

verbosityParser :: Opt.Parser Verbosity
verbosityParser =
  Opt.flag' Chatty (Opt.short 'v' <> Opt.long "verbose")
    <|> Opt.flag' DryRun (Opt.short 'd' <> Opt.long "dry-run")
    <|> pure Silent

options :: Opt.Parser CLIOptions
options =
  CLIOptions
    <$> configFilePathParser <*> verbosityParser

otherCommands :: Opt.Parser CLICommand
otherCommands =
  Opt.subparser
    ( Opt.command
        "init"
        ( Opt.info
            (pure CLIInit)
            (Opt.progDesc "Initialise a new tmux-mate.dhall file")
        )
        <> Opt.command
          "start"
          ( Opt.info
              (CLIRun <$> options)
              (Opt.progDesc "Start running everything in the selected config file")
          )
    )
