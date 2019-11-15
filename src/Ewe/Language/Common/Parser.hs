module Ewe.Language.Common.Parser
    ( ws
    , string_
    , char_
    , keyword
    , stringLike
    , integerLike
    , withSpan
    ) where

import           Control.Monad
import qualified Data.Text as T
import           Data.Text (Text)
import           Ewe.Language.Common.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

ws :: Parser ()
ws = L.space space1 line block
    where
        line  = L.skipLineComment "--"
        block = L.skipBlockCommentNested "{-" "-}"

string_ :: Text -> Parser ()
string_ = void . string

char_ :: Char -> Parser ()
char_ = void . char

keyword :: Text -> Parser ()
keyword x = try $ string x *> notFollowedBy alphaNumChar

stringLike :: Parser Text
stringLike = T.concat <$> (char '"' *> many inner <* char '"')
    where inner = string "\\\"" <|> T.pack . pure <$> anySingleBut '"'

integerLike :: Parser Integer
integerLike = read <$> some digitChar

withSpan :: Parser a -> Parser (a, Span)
withSpan f = do
    p1 <- getOffset
    x <- f
    p2 <- getOffset
    pure (x, Known p1 p2)
