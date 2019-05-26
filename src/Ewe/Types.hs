module Ewe.Types where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

data Span
    = Known Int Int
    | Unknown
    deriving (Show)

instance Semigroup Span where
    (Known s1 e1) <> (Known s2 e2) = Known (min s1 s2) (max e1 e2)
    _ <> _ = Unknown

instance Monoid Span where
    mempty = Unknown

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

newtype Error = Error [ErrorData] deriving (Show)

data ErrorData = ErrorData
    { errMessage :: String
    , errSpan :: Span
    } deriving (Show)
