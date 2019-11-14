module Ewe.Language.Types
    ( Parser
    , ParserError
    , Source
    , Span(..)
    , Error(..)
    , ErrorData(..)
    , HasSpan(..)
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

type Source = (FilePath, Text)

data Span
    = Known Int Int
    | Unknown
    deriving (Show)

newtype Error = Error { errData :: [ErrorData] }
    deriving (Show)

data ErrorData = ErrorData
    { errSpan    :: Span
    , errMessage :: String
    }
    deriving (Show)

instance Semigroup Span where
    (Known s1 e1) <> (Known s2 e2) = Known (min s1 s2) (max e1 e2)
    _ <> _ = Unknown

instance Monoid Span where
    mempty = Unknown

class HasSpan a where
    getSpan :: a -> Span

instance HasSpan Span where
    getSpan = id

instance HasSpan ErrorData where
    getSpan = errSpan
