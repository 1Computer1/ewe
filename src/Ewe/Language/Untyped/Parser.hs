module Ewe.Language.Untyped.Parser
    ( program
    , definition
    , expression
    , module Ewe.Language.Untyped.Syntax
    ) where

import qualified Data.Text as T
import           Ewe.Language.Common.Parser
import           Ewe.Language.Common.Types
import           Ewe.Language.Untyped.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

{-
    Program     = Definition* EOF
    Definition  = Identifier "=" Expression ";"
    Expression  = Lambda | Application
    Lambda      = "\" Identifier+ "." Expression
    Application = Atom+
    Atom        = Identifier | "(" Expression ")"
    Identifier  = [A-Za-z0-9_]+
-}

program :: Parser [Defn]
program = ws *> many (definition <* ws) <* eof

definition :: Parser Defn
definition = do
    p1 <- getOffset
    ident <- identifier <* ws
    char_ '=' <* ws
    body <- expression <* ws 
    char_ ';'
    p2 <- getOffset
    pure $ Defn (Known p1 p2) ident body

expression :: Parser Expr
expression = lambda <|> application

lambda :: Parser Expr
lambda = do
    p1 <- getOffset
    char_ '\\'
    args <- some (ws *> identifier)
    ws
    char_ '.' <* ws
    body <- expression
    p2 <- getOffset
    let sp = Known p1 p2
        assoc = Abs sp
        a1 = Abs sp (last args) body
    pure $ foldr assoc a1 (init args)

application :: Parser Expr
application = do
    f1 <- atom
    fs <- many $ ws *> atom
    pure $ foldl1 assoc (f1:fs)
    where
        assoc f x = App (getSpan f <> getSpan x) f x

atom :: Parser Expr
atom = var <|> group

var :: Parser Expr
var = do
    (ident, sp) <- withSpan identifier
    pure $ Var sp ident

group :: Parser Expr
group = do
    (expr, sp) <- withSpan $ char '(' *> ws *> expression <* ws <* char ')'
    pure $ Val sp expr

identifier :: Parser Ident
identifier = do
    (name, sp) <- withSpan $ T.pack <$> (some $ alphaNumChar <|> char '_')
    pure $ Ident sp name
