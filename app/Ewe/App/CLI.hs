module Ewe.App.CLI
    ( getArgs
    ) where

import Ewe.App.Types
import Ewe.Language hiding (Parser)
import Options.Applicative

getArgs :: IO Command
getArgs = customExecParser (prefs showHelpOnEmpty) parser

desc :: String -> InfoMod a
desc s = fullDesc <> header "ewe - Lambda calculus interpreter" <> progDesc s

parser :: ParserInfo Command
parser = info (helper <*> commandParser) $
    desc "Interpret the lambda calculus"

commandParser :: Parser Command
commandParser = subparser $
    runProgramFileCommand
    <> runProgramCommand
    <> runExprCommand
    <> runReplCommand

runProgramFileCommand, runProgramCommand, runExprCommand, runReplCommand :: Mod CommandFields Command
runProgramFileOpts,    runProgramOpts,    runExprOpts,    runReplOpts    :: Parser Command

runProgramFileCommand = command "run" . info (helper <*> runProgramFileOpts) $
    desc "Evaluate a program from a file"

runProgramFileOpts = RunProgramFile <$> lang <*> do
    strArgument (metavar "FILE")

runProgramCommand = command "exec" . info (helper <*> runProgramOpts) $
    desc "Evaluate a program"

runProgramOpts = RunProgram <$> lang <*> do
    strArgument (metavar "PROGRAM")

runExprCommand = command "eval" . info (helper <*> runExprOpts) $
    desc "Evaluate an expression"

runExprOpts = RunExpr <$> lang <*> do
    strArgument (metavar "EXPRESSION")

runReplCommand = command "repl" . info (helper <*> runReplOpts) $
    desc "Start up the ewe REPL"

runReplOpts = RunRepl <$> lang

lang :: Parser ILang
lang = option (maybeReader pLang) $
    long "language"
    <> short 'l'
    <> help "Choose the language to run"
    <> metavar "LANG"

pLang :: String -> Maybe ILang
pLang x = ILang <$> case x of
    "untyped" -> Just Untyped
    "u" -> Just Untyped
    _ -> Nothing
