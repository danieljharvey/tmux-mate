module CLICommands where

import TmuxMate (CLIOptions (..))

data CLICommand
  = CLIRun CLIOptions
  | CLIInit
