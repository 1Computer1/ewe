module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative
import GHC.IO.Encoding
import Ewe
import Repl

data EweOption
    = EvalFile FilePath
    | EvalText Text
    | EvalExpr Text
    | RunRepl

(<$.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <$.> g = \x -> f <$> g x

optsParse :: IO EweOption
optsParse = customExecParser p parser
    where
        p = prefs showHelpOnEmpty

        parser = info (helper <*> opts) $
               briefDesc
            <> header "ewe - Lambda calculus interpreter"
            <> progDesc "Interpret the lambda calculus"

        opts = file <|> text <|> expr <|> commandParser

        file = EvalFile <$.> option str $
               long "file"
            <> short 'f'
            <> help "Interpret a file containing source code"
            <> metavar "FILE"

        text = EvalText <$.> option str $
               long "str"
            <> short 's'
            <> help "Interpret a string containing source code"
            <> metavar "CODE"

        expr = EvalExpr <$.> option str $
               long "eval"
            <> short 'e'
            <> help "Interpret a string containing an expression"
            <> metavar "EXPRESSION"

        commandParser = subparser replCommand

        replCommand = command "repl" . info (helper <*> replOpts) $
                fullDesc
            <> header "ewe - Lambda calculus interpreter"
            <> progDesc "Start up the ewe REPL"

        replOpts = pure RunRepl

runProgram :: FilePath -> Text -> IO ()
runProgram path src = case parseProgram path src >>= evaluateProgram path src of
    Left err -> putStrLn err
    Right x  -> putStrLn x

runExpression :: FilePath -> Text -> IO ()
runExpression path src = case parseExpression path src >>= evaluateExpression path src of
    Left err -> putStrLn err
    Right x  -> putStrLn x

main :: IO ()
main = do
    setLocaleEncoding utf8
    opts <- optsParse
    case opts of
        EvalFile path -> TIO.readFile path >>= runProgram path
        EvalText src  -> runProgram "<eval>" src
        EvalExpr expr -> runExpression "<eval>" expr
        RunRepl       -> repl
