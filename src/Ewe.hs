module Ewe where

import           Control.Monad (join)
import           Control.Monad.Except (runExcept)
import           Control.Monad.Reader (runReaderT)

import           Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import qualified Data.Text as T

import           Ewe.Error (prettyError)

import           Ewe.Evaluator (Env, mkEnv, prelude, evaluateNormal, pretty)
import           Ewe.Parser (Defn, Expr, program, definition, expression)
import           Text.Megaparsec (runParser, errorBundlePretty)

parseProgram :: FilePath -> T.Text -> Either String [Defn]
parseProgram path src = do
    case runParser program path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

evaluateProgram :: FilePath -> T.Text -> [Defn] -> Either String String
evaluateProgram path src xs = do
    let pe = prettyError path src
    env <- first pe $ mkEnv xs
    case join (M.lookup "main" env) of
        Nothing -> Left "a symbol named main must be defined"
        Just x  -> bimap pe pretty . runExcept . flip runReaderT env $ evaluateNormal x

parseDefinition :: FilePath -> T.Text -> Either String Defn
parseDefinition path src = do
    case runParser definition path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

parseExpression :: FilePath -> T.Text -> Either String Expr
parseExpression path src = do
    case runParser expression path src of
        Left err -> Left (errorBundlePretty err)
        Right xs -> Right xs

evaluateExpression :: FilePath -> T.Text -> Expr -> Either String String
evaluateExpression path src expr = evaluateExpression' path src prelude expr

evaluateExpression' :: FilePath -> T.Text -> Env -> Expr -> Either String String
evaluateExpression' path src env expr =
    bimap (prettyError path src) pretty . runExcept . flip runReaderT env $ evaluateNormal expr
