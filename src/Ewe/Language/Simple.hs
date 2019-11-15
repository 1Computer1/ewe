module Ewe.Language.Simple
    ( Simple(..)
    ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map as M
import           Ewe.Language.Class
import           Ewe.Language.Common.Error
import           Ewe.Language.Common.Types
-- import qualified Ewe.Language.Simple.Evaluator as S
import           Ewe.Language.Internal
import qualified Ewe.Language.Simple.Parser as S

data Simple = Simple

instance Language Simple where
    type Defn Simple = S.Defn
    type Expr Simple = S.Expr
    type Val Simple = ()
    type Env Simple = ()

    parseProgram = defaultParse S.program
    parseDefn = defaultParse S.definition
    parseExpr = defaultParse S.expression

    evalProgram _ defns = undefined

    evalDefn _ env (S.Defn _ (S.Ident _ name) _ body) = undefined

    evalExpr _ env expr = undefined

    prelude _ = undefined
    envKeys _ = undefined
    pretty _ = undefined
