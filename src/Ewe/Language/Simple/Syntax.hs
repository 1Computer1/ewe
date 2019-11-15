module Ewe.Language.Simple.Syntax
    ( Ident(..)
    , Defn(..)
    , TypAnn(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import Ewe.Language.Common.Types

data Ident = Ident
    { identSpan :: Span
    , identText :: Text
    }
    deriving (Show)

instance HasSpan Ident where
    getSpan = identSpan

data Defn = Defn
    { defnSpan :: Span
    , defnIdent :: Ident
    , defnBody :: Expr
    }
    deriving (Show)

instance HasSpan Defn where
    getSpan = defnSpan

data TypAnn
    = TypAnnArr Span TypAnn TypAnn
    | TypAnnVar Span Ident
    | TypAnnVal Span TypAnn
    deriving (Show)

instance HasSpan TypAnn where
    getSpan = \case
        TypAnnArr s _ _ -> s
        TypAnnVar s _ -> s
        TypAnnVal s _ -> s

data Expr
    = Abs Span Ident TypAnn Expr
    | App Span Expr Expr
    | Branch Span Expr Expr Expr
    | Var Span Ident
    | LitInt Span Integer
    | LitStr Span Text
    | LitBool Span Bool
    | Val Span Expr
    deriving (Show)

instance HasSpan Expr where
    getSpan = \case
        Abs s _ _ _ -> s
        App s _ _ -> s
        Branch s _ _ _ -> s
        Var s _ -> s
        LitInt s _ -> s
        LitStr s _ -> s
        LitBool s _ -> s
        Val s _ -> s
