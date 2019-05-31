{-# LANGUAGE OverloadedStrings #-}

module Ewe where

import Control.Monad (join)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExcept)

import Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec (runParser, errorBundlePretty)

import Ewe.Parser (Definition, Tree, program, definition, expression)
import Ewe.Evaluator (Env, mkEnv, prelude, evaluateNormal, pretty)
import Ewe.Error (prettyError)

parseProgram :: FilePath -> T.Text -> Either String [Definition]
parseProgram path src = do
    case runParser program path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

evaluateProgram :: FilePath -> T.Text -> [Definition] -> Either String String
evaluateProgram path src xs = do
    let pe = prettyError path src
    env <- first pe $ mkEnv xs
    case join (M.lookup "main" env) of
        Nothing -> Left "a symbol named main must be defined"
        Just x  -> bimap pe pretty $ runExcept (runReaderT (evaluateNormal x) env)

parseDefinition :: FilePath -> T.Text -> Either String Definition
parseDefinition path src = do
    case runParser definition path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

parseExpression :: FilePath -> T.Text -> Either String Tree
parseExpression path src = do
    case runParser expression path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

evaluateExpression :: FilePath -> T.Text -> Tree -> Either String String
evaluateExpression path src expr = evaluateExpression' path src prelude expr

evaluateExpression' :: FilePath -> T.Text -> Env -> Tree -> Either String String
evaluateExpression' path src env expr = bimap (prettyError path src) pretty $ runExcept (runReaderT (evaluateNormal expr) env)
