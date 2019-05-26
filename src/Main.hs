{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Ewe

main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then do
            let path = args !! 0
            src <- TIO.readFile path
            case parseProgram path src >>= evaluateProgram path src of
                Left err -> putStrLn err
                Right x  -> putStrLn x
        else putStrLn "No input file given"
