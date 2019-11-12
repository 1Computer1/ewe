module Ewe.Types
    ( Parser
    , Span(..)
    , Error(..)
    , ErrorData(..)
    ) where

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

newtype Error = Error { errData :: [ErrorData] } deriving (Show)

data ErrorData = ErrorData
    { errMessage :: String
    , errSpan :: Span
    } deriving (Show)
