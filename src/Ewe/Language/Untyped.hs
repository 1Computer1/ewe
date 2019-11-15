module Ewe.Language.Untyped
    ( Untyped(..)
    ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map as M
import           Ewe.Language.Class
import           Ewe.Language.Common.Error
import           Ewe.Language.Common.Types
import           Ewe.Language.Internal
import qualified Ewe.Language.Untyped.Evaluator as S
import qualified Ewe.Language.Untyped.Parser as S

data Untyped = Untyped

instance Language Untyped where
    type Defn Untyped = S.Defn
    type Expr Untyped = S.Expr
    type Val Untyped = S.Expr
    type Env Untyped = S.Env

    parseProgram = defaultParse S.program
    parseDefn = defaultParse S.definition
    parseExpr = defaultParse S.expression

    evalProgram _ defns = do
        env <- S.mkEnv defns
        case join (M.lookup "main" env) of
            Nothing -> Left $ mkErr Unknown "a symbol named main must be defined"
            Just x  -> runExcept . flip runReaderT env $ S.evaluateNormal x

    evalDefn _ env (S.Defn _ (S.Ident _ name) body) = pure (body, M.insert name (Just body) env)

    evalExpr _ env expr = runExcept . flip runReaderT env $ S.evaluateNormal expr
    
    typeof _ _ _ = pure "no type information"

    prelude _ = S.prelude
    envKeys _ = M.keys

    pretty _ = S.pretty
