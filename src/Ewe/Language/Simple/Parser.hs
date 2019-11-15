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
    Expression    = Lambda | Application | Branch
    Lambda        = "\" ("(" Identifier ":" Type ")")+ "." Expression
    Application   = Atom+
    Branch        = "if" Expression "then" Expression "else" Expression
    Atom          = Identifier | Integer | String | Bool | "(" Expression ")"
    Integer       = [0-9]+
    String        = '"' ('\"' | .)* '"'
    Bool          = "true" | "false"
    Identifier    = [a-z][A-Za-z0-9_]*
    Type          = TypeAtom ("->" TypeAtom)*
    TypeAtom      = TypeIdentifier | "(" Type ")"
    TypeIdentifer = [A-Z][A-Za-z0-9_]*
-}

keyword :: T.Text -> Parser ()
keyword x = try $ string x *> notFollowedBy alphaNumChar

program :: Parser [Defn]
program = ws *> many (definition <* ws) <* eof

definition :: Parser Defn
definition = do
    ((ident, body), sp) <- withSpan $ do
        ident <- identifier <* ws
        body <- char '=' *> ws *> expression <* ws <* char ';'
        pure (ident, body)
    pure $ Defn sp ident body

expression :: Parser Expr
expression = lambda <|> branch <|> application

lambda :: Parser Expr
lambda = do
    p1 <- getOffset
    void fun
    args <- some . try $ do
        param <- ws *> char '(' *> identifier <* ws
        typ <- char ':' *> ws *> typeAnnotation <* ws <* char ')'
        pure (param, typ)
    ws *> arr *> ws
    body <- expression
    p2 <- getOffset
    let sp = Known p1 p2
        assoc (ident, typ) = Abs sp ident typ
        (lastParam, lastTyp) = last args
        a1 = Abs sp lastParam lastTyp body
    pure $ foldr assoc a1 (init args)
    where
        fun = char '\\'
        arr = string "->"

application :: Parser Expr
application = do
    f1 <- atom
    fs <- many (try $ ws *> atom)
    pure $ foldl1 assoc (f1:fs)
    where
        assoc f x = App (getSpan f <> getSpan x) f x

branch :: Parser Expr
branch = do
    p1 <- getOffset
    keyword "if" *> ws
    p <- expression <* ws
    keyword "then" *> ws
    q <- expression <* ws
    keyword "else" *> ws
    r <- expression
    p2 <- getOffset
    pure $ Branch (Known p1 p2) p q r

atom :: Parser Expr
atom = integer <|> stringp <|> boolean <|> var <|> group

integer :: Parser Expr
integer = do
    (x, sp) <- withSpan (some digitChar)
    pure $ LitInt sp (read x)

stringp :: Parser Expr
stringp = do
    (x, sp) <- withSpan $ char '"' *> many (string "\\\"" <|> T.pack . pure <$> anySingleBut '"') <* char '"'
    pure $ LitStr sp (T.concat x)

boolean :: Parser Expr
boolean = do
    (x, sp) <- withSpan (True <$ keyword "true" <|> False <$ keyword "false")
    pure $ LitBool sp x

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
    (name, sp) <- withSpan $ T.pack <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_'))
    guard (name `notElem` ["if", "then", "else", "true", "false"])
    pure $ Ident sp name

typeAnnotation :: Parser TypAnn
typeAnnotation = do
    t1 <- typeAtom
    ts <- many (try $ ws *> arr *> ws *> typeAtom)
    pure $ foldr1 assoc (t1:ts)
    where
        arr = string "->"
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
    (name, sp) <- withSpan $ T.pack <$> ((:) <$> upperChar <*> many (alphaNumChar <|> char '_'))
    pure $ Ident sp name
