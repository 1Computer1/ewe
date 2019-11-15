module Ewe.Language.Simple.Parser
    ( program
    , definition
    , expression
    , module Ewe.Language.Simple.Syntax
    ) where

import           Control.Monad
import qualified Data.Text as T
import           Ewe.Language.Common.Parser
import           Ewe.Language.Common.Types
import           Ewe.Language.Simple.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

{-
    Program       = Definition* EOF
    Definition    = Identifier ":" Type "=" Expression ";"
    Expression    = Lambda | Application
    Lambda        = "\" ("(" Identifier ":" Type ")")+ "." Expression
    Application   = Atom+
    Atom          = Identifier | Integer | String | "(" Expression ")"
    Integer       = Num+
    String        = '"' StringChar* '"'
    Identifier    = Lowercase (AlphaNum | "_")*
    Type          = TypeAtom ("->" TypeAtom)*
    TypeAtom      = TypeIdentifier | "(" Type ")"
    TypeIdentifer = Uppercase (AlphaNum | "_")*
-}

program :: Parser [Defn]
program = ws *> many (definition <* ws) <* eof

definition :: Parser Defn
definition = do
    ((ident, typ, body), sp) <- withSpan $ do
        ident <- identifier <* ws
        typ <- char ':' *> ws *> typeAnnotation <* ws
        body <- eq *> ws *> expression <* ws <* end
        pure (ident, typ, body)
    pure $ Defn sp ident typ body
    where
        eq  = choice . map string $ ["=", ":=", "≔", "≝", "≡"]
        end = char ';'

expression :: Parser Expr
expression = lambda <|> application

lambda :: Parser Expr
lambda = do
    p1 <- getOffset
    void fun
    args <- some $ do
        param <- ws *> char '(' *> identifier <* ws <* arr <* ws
        typ <- char ':' *> ws *> typeAnnotation <* ws <* char ')'
        pure (param, typ)
    body <- expression
    p2 <- getOffset
    let sp = Known p1 p2
        assoc (ident, typ) = Abs sp ident typ
        (lastParam, lastTyp) = last args
        a1 = Abs sp lastParam lastTyp body
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
    (ident, sp) <- withSpan identifier
    pure $ Var sp ident

group :: Parser Expr
group = do
    (expr, sp) <- withSpan $ char '(' *> ws *> expression <* ws <* char ')'
    pure $ Val sp expr

identifier :: Parser Ident
identifier = do
    (name, sp) <- withSpan $ T.pack <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_'))
    pure $ Ident sp name

typeAnnotation :: Parser TypAnn
typeAnnotation = do
    t1 <- typeAtom
    ts <- many (ws *> arr *> ws *> typeAtom)
    pure $ foldl1 assoc (t1:ts)
    where
        arr = choice . map string $ ["->", "→", "=>", "⇒"]
        assoc t1 t2 = TypArr (getSpan t1 <> getSpan t2) t1 t2

typeAtom :: Parser TypAnn
typeAtom = typeVar <|> typeGroup

typeVar :: Parser TypAnn
typeVar = do
    (ident, sp) <- withSpan typeIdentifier
    pure $ TypVar sp ident

typeGroup :: Parser TypAnn
typeGroup = do
    (expr, sp) <- withSpan $ char '(' *> ws *> typeAnnotation <* ws <* char ')'
    pure $ TypVal sp expr

typeIdentifier :: Parser Ident
typeIdentifier = do
    (name, sp) <- withSpan $ T.pack <$> ((:) <$> upperChar <*> many (alphaNumChar <|> char '_'))
    pure $ Ident sp name
