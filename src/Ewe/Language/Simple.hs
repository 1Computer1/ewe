module Ewe.Language.Simple
    ( Simple(..)
    ) where

import           Control.Monad
import qualified Data.Map as M
import           Ewe.Language.Class
import           Ewe.Language.Common.Error
import           Ewe.Language.Common.Types
import           Ewe.Language.Internal
import qualified Ewe.Language.Simple.Evaluator as S
import qualified Ewe.Language.Simple.Parser as S

data Simple = Simple

instance Language Simple where
    type Defn Simple = S.Defn
    type Expr Simple = S.Expr
    type Val Simple = S.Val
    type Env Simple = S.BothEnv

    parseProgram = defaultParse S.program
    parseDefn = defaultParse S.definition
    parseExpr = defaultParse S.expression

    evalProgram _ defns = do
        void $ S.checkEnv defns
        env <- S.mkEnv defns
        case M.lookup "main" env of
            Nothing -> Left $ mkErr Unknown "a symbol named main must be defined"
            Just x  -> pure x

    evalDefn _ env (S.Defn _ (S.Ident _ name) body) = do
        bodyTyp <- runUsual (M.map fst env) (S.typecheck body)
        val <- runUsual (M.map snd env) (S.evaluate body)
        pure (val, M.insert name (bodyTyp, val) env)

    evalExpr _ env expr = runUsual (M.map snd env) $ S.evaluate expr

    typeof _ env expr = S.prettyType <$> runUsual (M.map fst env) (S.typecheck expr)

    prelude _ = S.prelude
    envKeys _ = M.keys

    langName _ = "Simple"
    pretty _ = S.pretty
