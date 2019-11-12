{-# LANGUAGE NamedFieldPuns, OverloadedStrings, PatternSynonyms, TypeApplications  #-}

module Ewe.Parser
    ( Parser
    , Span
    , Identifier(..)
    , Definition(..)
    , Tree(..)
    , Node(..)
    , pattern Abstraction
    , pattern Application
    , pattern Variable
    , pattern Value
    , program
    , definition
    , expression
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Ewe.Syntax
import Ewe.Types

{-
    Program     = Definition* EOF
    Definition  = Identifier "=" Expression ";"
    Expression  = Lambda | Application
    Lambda      = "\" Identifier+ "." Expression
    Application = Atom+
    Atom        = Identifier | "(" Expression ")"
    Identifier  = (AlphaNum | "_")+
-}

pattern Abstraction :: Text -> Span -> Tree -> Span -> Tree
pattern Abstraction idName idSpan body treeSpan = Tree { treeNode = Abs (Identifier { idName, idSpan }) body, treeSpan }

pattern Application :: Tree -> Tree -> Span -> Tree
pattern Application f x treeSpan = Tree { treeNode = App f x, treeSpan }

pattern Variable :: Text -> Span -> Span -> Tree
pattern Variable idName idSpan treeSpan = Tree { treeNode = Var (Identifier { idName, idSpan }), treeSpan }

pattern Value :: Tree -> Span -> Tree
pattern Value inner treeSpan = Tree { treeNode = Val inner, treeSpan }

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

program :: Parser [Definition]
program = ws *> many (definition <* ws) <* eof

definition :: Parser Definition
definition = do
    ((defIdent, defBody), defSpan) <- withSpan $ (,) <$> identifier <*> (ws *> eq *> ws *> expression <* ws <* end)
    pure $ Definition { defIdent, defBody, defSpan }
    where
        eq  = choice . map string $ ["=", ":=", "≔", "≝", "≡"]
        end = char ';'

expression :: Parser Tree
expression = lambda <|> application

lambda :: Parser Tree
lambda = do
    p1 <- getOffset
    args <- fun *> some (ws *> identifier) <* ws <* arr <* ws
    body <- expression
    p2 <- getOffset
    let treeSpan = Known p1 p2
        assoc ident acc = Tree { treeNode = Abs ident acc, treeSpan }
        a1 = Tree { treeNode = Abs (last args) body, treeSpan }
    pure $ foldr assoc a1 (init args)
    where
        fun = choice . map string $ ["\\", "λ", "^"]
        arr = choice . map string $ [".", "->", "→", "=>", "⇒"]

application :: Parser Tree
application = do
    f1 <- atom
    fs <- many (ws *> atom)
    pure $ foldl1 assoc (f1:fs)
    where
        assoc f x = Tree { treeNode = App f x, treeSpan = treeSpan f <> treeSpan x }

atom :: Parser Tree
atom = do
    (treeNode, treeSpan) <- withSpan $ ident <|> grouped
    pure $ Tree { treeNode, treeSpan }
    where
        ident   = Var <$> identifier
        grouped = Val <$> (char '(' *> ws *> expression <* ws <* char ')')

identifier :: Parser Identifier
identifier = do
    (idName, idSpan) <- withSpan $ T.pack <$> (some (alphaNumChar <|> char '_'))
    pure $ Identifier { idName, idSpan }
