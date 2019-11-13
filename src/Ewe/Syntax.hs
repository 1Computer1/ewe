module Ewe.Syntax
    ( Ident(..)
    , Defn(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import Ewe.Types

data Ident = Ident
    { identSpan :: Span
    , identText :: Text
    }
    deriving (Show)

instance HasSpan Ident where
    getSpan = identSpan

data Defn = Defn
    { defnSpan  :: Span
    , defnIdent :: Ident
    , defnBody  :: Expr
    }
    deriving (Show)

instance HasSpan Defn where
    getSpan = defnSpan

data Expr
    = Abs Span Ident Expr
    | App Span Expr Expr
    | Var Span Ident
    | Val Span Expr
    deriving (Show)

instance HasSpan Expr where
    getSpan = \case
        Abs s _ _ -> s
        App s _ _ -> s
        Var s _ -> s
        Val s _ -> s
