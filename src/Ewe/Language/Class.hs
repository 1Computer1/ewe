module Ewe.Language.Class
    ( Language(..)
    ) where

import Data.Text (Text)
import Ewe.Language.Common.Types

class Language a where
    type Defn a
    type Expr a
    type Val a
    type Env a

    parseProgram :: a -> Source -> Either ParserError [Defn a]
    parseDefn    :: a -> Source -> Either ParserError (Defn a)
    parseExpr    :: a -> Source -> Either ParserError (Expr a)

    evalProgram :: a -> [Defn a] -> Either Error (Val a)
    evalDefn    :: a -> Env a -> Defn a -> Either Error (Val a, Env a)
    evalExpr    :: a -> Env a -> Expr a -> Either Error (Val a)
    typeof      :: a -> Env a -> Expr a -> Either Error String

    prelude :: a -> Env a
    envKeys :: a -> Env a -> [Text]

    pretty :: a -> Val a -> String
