module Ewe.App
    ( runProgram
    , runExpr
    , module X
    ) where

import Data.Bifunctor
import Ewe.App.CLI as X
import Ewe.App.Repl as X
import Ewe.App.Types as X
import Ewe.Language

runProgram :: Language lang => lang -> Source -> IO ()
runProgram lang source = case parse >>= eval of
    Left err -> putStrLn err
    Right x  -> putStrLn $ pretty lang x
    where
        parse = first prettyParserError $ parseProgram lang source
        eval = first (prettyError source) . evalProgram lang

runExpr :: Language lang => lang -> Source -> IO ()
runExpr lang source = case parse >>= eval of
    Left err -> putStrLn err
    Right x  -> putStrLn $ pretty lang x
    where
        parse = first prettyParserError $ parseExpr lang source
        eval = first (prettyError source) . evalExpr lang (prelude lang)
