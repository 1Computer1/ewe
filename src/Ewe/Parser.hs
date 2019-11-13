module Ewe.Parser
    ( Parser
    , Span
    , program
    , definition
    , expression
    , module Ewe.Syntax
    ) where

import qualified Data.Text as T
import           Ewe.Syntax
import           Ewe.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

{-
    Program     = Definition* EOF
    Definition  = Identifier "=" Expression ";"
    Expression  = Lambda | Application
    Lambda      = "\" Identifier+ "." Expression
    Application = Atom+
    Atom        = Identifier | "(" Expression ")"
    Identifier  = (AlphaNum | "_")+
-}

{-
pattern Abstraction :: Text -> Span -> Tree -> Span -> Tree
pattern Abstraction idName idSpan body treeSpan = Tree { treeNode = Abs (Identifier { idName, idSpan }) body, treeSpan }

pattern Application :: Tree -> Tree -> Span -> Tree
pattern Application f x treeSpan = Tree { treeNode = App f x, treeSpan }

pattern Variable :: Text -> Span -> Span -> Tree
pattern Variable idName idSpan treeSpan = Tree { treeNode = Var (Identifier { idName, idSpan }), treeSpan }

pattern Value :: Tree -> Span -> Tree
pattern Value inner treeSpan = Tree { treeNode = Val inner, treeSpan }
-}

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

program :: Parser [Defn]
program = ws *> many (definition <* ws) <* eof

definition :: Parser Defn
definition = do
    ((ident, body), span) <- withSpan $ (,) <$> identifier <*> (ws *> eq *> ws *> expression <* ws <* end)
    pure $ Defn span ident body
    where
        eq  = choice . map string $ ["=", ":=", "≔", "≝", "≡"]
        end = char ';'

expression :: Parser Expr
expression = lambda <|> application

lambda :: Parser Expr
lambda = do
    p1 <- getOffset
    args <- fun *> some (ws *> identifier) <* ws <* arr <* ws
    body <- expression
    p2 <- getOffset
    let span = Known p1 p2
        assoc = Abs span
        a1 = Abs span (last args) body
    pure $ foldr assoc a1 (init args)
    where
        fun = choice . map string $ ["\\", "λ", "^"]
        arr = choice . map string $ [".", "->", "→", "=>", "⇒"]

application :: Parser Expr
application = do
    f1 <- atom
    fs <- many (ws *> atom)
    pure $ foldl1 assoc (f1:fs)
    where
        assoc f x = App (getSpan f <> getSpan x) f x

atom :: Parser Expr
atom = var <|> group

var :: Parser Expr
var = do
    (ident, span) <- withSpan identifier
    pure $ Var span ident

group :: Parser Expr
group = do
    (expr, span) <- withSpan $ char '(' *> ws *> expression <* ws <* char ')'
    pure $ Val span expr

identifier :: Parser Ident
identifier = do
    (name, span) <- withSpan $ T.pack <$> (some $ alphaNumChar <|> char '_')
    pure $ Ident span name
