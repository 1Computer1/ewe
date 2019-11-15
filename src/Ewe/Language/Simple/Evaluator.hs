module Ewe.Language.Simple.Evaluator
    ( Val(..)
    , Typ(..)
    , Env
    , TypEnv
    , BothEnv
    , prelude
    , checkEnv
    , mkEnv
    , evaluate
    , typecheck
    , fromTypAnn
    , prettyType
    , pretty
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char (toLower)
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Ewe.Language.Common.Error
import           Ewe.Language.Common.Types
import           Ewe.Language.Simple.Syntax

data Val
    = ValInt Integer
    | ValStr Text
    | ValBool Bool
    | ValFun Ident Expr Env
    | ValFunPrim (Val -> Except Error Val)

data Typ
    = TypInt
    | TypStr
    | TypBool
    | TypArr Typ Typ
    deriving (Eq)

type Env = Map Text Val

type TypEnv = Map Text Typ

type BothEnv = Map Text (Typ, Val)

type EvalM = ReaderT Env (Except Error)

type CheckM = ReaderT TypEnv (Except Error)

prelude :: BothEnv
prelude = M.fromList
    [ ("add",
        ( int -:> int -:> int
        , fun $ \(ValInt x) -> pure . fun $ \(ValInt y) -> pure . ValInt $ x + y
        ))
    , ("sub",
        ( int -:> int -:> int
        , fun $ \(ValInt x) -> pure . fun $ \(ValInt y) -> pure . ValInt $ x - y
        ))
    , ("mul",
        ( int -:> int -:> int
        , fun $ \(ValInt x) -> pure . fun $ \(ValInt y) -> pure . ValInt $ x * y
        ))
    , ("div",
        ( int -:> int -:> int
        , fun $ \(ValInt x) -> pure . fun $ \(ValInt y) -> if y == 0
            then throwError . mkErr Unknown $ "division by 0"
            else pure . ValInt $ x `div` y
        ))
    , ("negate",
        ( int -:> int
        , fun $ \(ValInt x) -> pure . ValInt $ negate x
        ))
    , ("concat",
        ( str -:> str -:> str
        , fun $ \(ValStr x) -> pure . fun $ \(ValStr y) -> pure . ValStr $ x <> y
        ))
    , ("length",
        ( str -:> int
        , fun $ \(ValStr x) -> pure . ValInt . fromIntegral $ T.length x
        ))
    , ("int_eq",
        ( int -:> int -:> bool
        , fun $ \(ValInt x) -> pure . fun $ \(ValInt y) -> pure . ValBool $ x == y
        ))
    , ("str_eq",
        ( str -:> str -:> bool
        , fun $ \(ValStr x) -> pure . fun $ \(ValStr y) -> pure . ValBool $ x == y
        ))
    , ("iff",
        ( bool -:> bool -:> bool
        , fun $ \(ValBool x) -> pure . fun $ \(ValBool y) -> pure . ValBool $ x == y
        ))
    , ("not",
        ( bool -:> bool
        , fun $ \(ValBool x) -> pure . ValBool $ not x
        ))
    , ("and",
        ( bool -:> bool -:> bool
        , fun $ \(ValBool x) -> pure . fun $ \(ValBool y) -> pure . ValBool $ x && y
        ))
    , ("or",
        ( bool -:> bool -:> bool
        , fun $ \(ValBool x) -> pure . fun $ \(ValBool y) -> pure . ValBool $ x || y
        ))
    ]
    where
        infixr 2 -:>
        (-:>) = TypArr
        int = TypInt
        str = TypStr
        bool = TypBool
        fun = ValFunPrim

checkEnv :: [Defn] -> Either Error TypEnv
checkEnv = fmap fst . foldM go (M.map fst prelude, M.map (const Unknown) prelude)
    where
        go (env, spans) (Defn _ (Ident nameSp name) body)
            | Just conflict@(Known _ _) <- M.lookup name spans = throwError . mkErrs $
                [ (nameSp, "the symbol " <> T.unpack name <> " has already been defined")
                , (conflict, "it was defined here")
                ]
            | Just Unknown <- M.lookup name spans = throwError . mkErr nameSp $
                "the symbol " <> T.unpack name <> " has already been defined in the prelude"
            | otherwise = do
                bodyTyp <- runExcept $ runReaderT (typecheck body) env
                pure (M.insert name bodyTyp env, M.insert name nameSp spans)

mkEnv :: [Defn] -> Either Error Env
mkEnv = foldM go (M.map snd prelude)
    where
        go env (Defn _ (Ident _ name) body) = do
            val <- runExcept $ runReaderT (evaluate body) env
            pure $ M.insert name val env

evaluate :: Expr -> EvalM Val
evaluate = \case
    Abs _ ident _ body -> do
        env <- ask
        pure $ ValFun ident body env

    App _ f x -> do
        fVal <- evaluate f
        xVal <- evaluate x
        case fVal of
            ValFun (Ident _ param) body env -> local (const $ M.insert param xVal env) (evaluate body)
            ValFunPrim g -> lift $ g xVal
            _ -> error "impossible: app of non-function"

    Branch _ p q r -> do
        pVal <- evaluate p
        case pVal of
            ValBool True -> evaluate q
            ValBool False -> evaluate r
            _ -> error "impossible: non-boolean condition"

    Var _ (Ident _ name) -> do
        entry <- asks $ M.lookup name
        case entry of
            Nothing -> error "impossible: undefined variable"
            Just x -> pure x

    LitInt _ x -> pure $ ValInt x

    LitStr _ x -> pure $ ValStr x

    LitBool _ x -> pure $ ValBool x

    Val _ x -> evaluate x

typecheck :: Expr -> CheckM Typ
typecheck = \case
    Abs _ (Ident _ param) typAnn body -> do
        paramTyp <- liftEither $ fromTypAnn typAnn
        bodyTyp <- local (M.insert param paramTyp) (typecheck body)
        pure $ TypArr paramTyp bodyTyp

    App _ f x -> do
        fTyp <- typecheck f
        case fTyp of
            TypArr yTyp zTyp -> do
                xTyp <- typecheck x
                if yTyp == xTyp
                    then pure zTyp
                    else throwError . mkErr (getSpan x) $
                        "expected type " <> prettyType yTyp <> " got type " <> prettyType xTyp
            _ -> throwError . mkErr (getSpan f) $ "expected function type got type " <> prettyType fTyp

    Branch _ p q r -> do
        pTyp <- typecheck p
        case pTyp of
            TypBool -> do
                qTyp <- typecheck q
                rTyp <- typecheck r
                if qTyp == rTyp
                    then pure qTyp
                    else throwError . mkErr (getSpan r) $
                        "expected type " <> prettyType qTyp <> " got type " <> prettyType rTyp
            _ -> throwError . mkErr (getSpan p) $ "expected type Bool got type " <> prettyType pTyp

    Var sp (Ident _ name) -> do
        entry <- asks (M.lookup name)
        case entry of
            Nothing -> throwError . mkErr sp $ "variable " <> T.unpack name <> " not defined"
            Just x -> pure x

    LitInt _ _ -> pure TypInt

    LitStr _ _ -> pure TypStr

    LitBool _ _ -> pure TypBool

    Val _ x -> typecheck x

fromTypAnn :: TypAnn -> Either Error Typ
fromTypAnn = \case
    TypAnnArr _ a b -> TypArr <$> fromTypAnn a <*> fromTypAnn b

    TypAnnVar sp (Ident _ name) -> case name of
        "Int" -> pure TypInt
        "Str" -> pure TypStr
        "Bool" -> pure TypBool
        _ -> throwError . mkErr sp $ "unknown type " <> T.unpack name

    TypAnnVal _ x -> fromTypAnn x

prettyType :: Typ -> String
prettyType = \case
    TypArr a@(TypArr _ _) b -> "(" <> prettyType a <> ")" <> " -> " <> prettyType b
    TypArr a b -> prettyType a <> " -> " <> prettyType b
    TypInt -> "Int"
    TypStr -> "Str"
    TypBool -> "Bool"

pretty :: Val -> String
pretty = \case
    ValInt n -> show n
    ValStr x -> show x
    ValBool x -> map toLower $ show x
    ValFun _ _ _ -> "<function>"
    ValFunPrim _ -> "<internal function>"
