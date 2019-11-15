module Ewe.Language.Common.Parser
    ( ws
    , withSpan
    ) where

import           Ewe.Language.Common.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

ws :: Parser ()
ws = L.space space1 line block
    where
        line  = L.skipLineComment "--"
        block = L.skipBlockCommentNested "{-" "-}"

withSpan :: Parser a -> Parser (a, Span)
withSpan f = do
    p1 <- getOffset
    x <- f
    p2 <- getOffset
    pure (x, Known p1 p2)
