{-# LANGUAGE OverloadedStrings #-}

module Ewe where

import Control.Monad (join)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExcept)

import Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec (runParser, errorBundlePretty)

import Ewe.Parser (Definition, program)
import Ewe.Evaluator (mkEnv, evaluateNormal, pretty)
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
