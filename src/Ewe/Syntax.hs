module Ewe.Syntax
    ( Identifier(..)
    , Definition(..)
    , Tree(..)
    , Node(..)
    ) where

import Data.Text (Text)
import Ewe.Types

data Identifier = Identifier
    { idName :: Text
    , idSpan :: Span
    } deriving (Show)

data Definition = Definition
    { defIdent :: Identifier
    , defBody :: Tree
    , defSpan :: Span
    } deriving (Show)

data Tree = Tree
    { treeNode :: Node
    , treeSpan :: Span
    } deriving (Show)

data Node
    = Abs Identifier Tree
    | App Tree Tree
    | Var Identifier
    | Val Tree
    deriving (Show)
