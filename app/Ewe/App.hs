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

run :: Language lang
    => (lang -> Source -> Either ParserError a)
    -> (lang -> a -> Either Error (Val lang))
    -> lang
    -> Source
    -> IO ()
run parseF evalF lang source = case parse >>= eval of
    Left err -> putStrLn err
    Right x  -> putStrLn $ pretty lang x
    where
        parse = first prettyParserError $ parseF lang source
        eval = first (prettyError source) . evalF lang

runProgram :: Language lang => lang -> Source -> IO ()
runProgram = run parseProgram evalProgram

runExpr :: Language lang => lang -> Source -> IO ()
runExpr lang = run parseExpr (flip evalExpr (prelude lang)) lang
