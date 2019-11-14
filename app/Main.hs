module Main where

import qualified Data.Text.IO as TIO
import           Ewe.App
import           GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8
    command <- getArgs
    case command of
        RunProgramFile (ILang lang) path -> do
            src <- TIO.readFile path
            runProgram lang (path, src)
        RunProgram (ILang lang) src -> runProgram lang ("<eval>", src)
        RunExpr (ILang lang) expr -> runExpr lang ("<eval>", expr)
        RunRepl (ILang lang) -> repl lang
