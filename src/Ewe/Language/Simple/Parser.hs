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
    Definition    = Identifier "=" Expression ";"
    Expression    = Lambda | Branch | Application
    Lambda        = "\" ("(" Identifier ":" Type ")")+ "." Expression
    Application   = Atom+
    Branch        = "if" Expression "then" Expression "else" Expression
    Atom          = Integer | String | Bool | Identifier | "(" Expression ")"
    Integer       = [0-9]+
    String        = '"' ('\"' | .)* '"'
    Bool          = "true" | "false"
    Identifier    = [a-z][A-Za-z0-9_]*
    Type          = TypeAtom ("->" TypeAtom)*
    TypeAtom      = TypeIdentifier | "(" Type ")"
    TypeIdentifer = [A-Z][A-Za-z0-9_]*
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
expression = lambda <|> branch <|> application

lambda :: Parser Expr
lambda = do
    p1 <- getOffset
    char_ '\\'
    args <- some . try $ do
        ws
        char_ '(' <* ws
        param <- identifier <* ws
        char_ ':' <* ws
        typ <- typeAnnotation <* ws
        char_ ')'
        pure (param, typ)
    ws
    string_ "->" <* ws
    body <- expression
    p2 <- getOffset
    let sp = Known p1 p2
        assoc (ident, typ) = Abs sp ident typ
        (lastParam, lastTyp) = last args
        a1 = Abs sp lastParam lastTyp body
    pure $ foldr assoc a1 (init args)

application :: Parser Expr
application = do
    f1 <- atom
    fs <- many . try $ ws *> atom
    pure $ foldl1 assoc (f1:fs)
    where
        assoc f x = App (getSpan f <> getSpan x) f x

branch :: Parser Expr
branch = do
    p1 <- getOffset
    keyword "if" <* ws
    p <- expression <* ws
    keyword "then" <* ws
    q <- expression <* ws
    keyword "else" <* ws
    r <- expression
    p2 <- getOffset
    pure $ Branch (Known p1 p2) p q r

atom :: Parser Expr
atom = integer <|> stringp <|> boolean <|> var <|> group

integer :: Parser Expr
integer = do
    (x, sp) <- withSpan integerLike
    pure $ LitInt sp x

stringp :: Parser Expr
stringp = do
    (x, sp) <- withSpan stringLike
    pure $ LitStr sp x

boolean :: Parser Expr
boolean = do
    (x, sp) <- withSpan bools
    pure $ LitBool sp x
    where
        bools = True <$ keyword "true" <|> False <$ keyword "false"

var :: Parser Expr
var = do
    (ident, sp) <- withSpan identifier
    pure $ Var sp ident

group :: Parser Expr
group = do
    (expr, sp) <- withSpan $ char '(' *> ws *> expression <* ws <* char ')'
    pure $ Val sp expr

identifier :: Parser Ident
identifier = try $ do
    (name, sp) <- withSpan $ T.pack <$> inner
    guard $ name `notElem` ["if", "then", "else", "true", "false"]
    pure $ Ident sp name
    where
        inner = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')

typeAnnotation :: Parser TypAnn
typeAnnotation = do
    t1 <- typeAtom
    ts <- many . try $ ws *> string "->" *> ws *> typeAtom
    pure $ foldr1 assoc (t1:ts)
    where
        assoc t acc = TypAnnArr (getSpan t <> getSpan acc) t acc

typeAtom :: Parser TypAnn
typeAtom = typeVar <|> typeGroup

typeVar :: Parser TypAnn
typeVar = do
    (ident, sp) <- withSpan typeIdentifier
    pure $ TypAnnVar sp ident

typeGroup :: Parser TypAnn
typeGroup = do
    (expr, sp) <- withSpan $ char '(' *> ws *> typeAnnotation <* ws <* char ')'
    pure $ TypAnnVal sp expr

typeIdentifier :: Parser Ident
typeIdentifier = do
    (name, sp) <- withSpan $ T.pack <$> inner
    pure $ Ident sp name
    where
        inner = (:) <$> upperChar <*> many (alphaNumChar <|> char '_')
