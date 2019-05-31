{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, OverloadedStrings, TypeApplications #-}

module Ewe.Evaluator
    ( Error
    , Env
    , prelude
    , mkEnv
    , evaluateNormal
    , pretty
    ) where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Ewe.Types
import Ewe.Parser
import Ewe.Error

type Env = Map Text (Maybe Tree)

prelude :: Env
prelude = M.fromList . map (fmap Just) $
    [ ("S", "f" .: "g" .: "x" .: var "f" $: var "x" $: (var "g" $: var "x"))
    , ("K", "x" .: "y" .: var "x")
    , ("I", "x" .: var "x")
    , ("B", "f" .: "g" .: "x" .: var "f" $: (var "g" $: var "x"))
    , ("C", "f" .: "x" .: "y" .: var "f" $: var "y" $: var "x")
    , ("W", "f" .: "x" .: var "f" $: var "x" $: var "x")
    , ("Y", "f" .: ("x" .: var "f" $: (var "x" $: var "x")) $: ("x" .: var "f" $: (var "x" $: var "x")))
    ]
    where
        infixr 2 .:
        p .: b = Abstraction p Unknown b Unknown

        infixl 3 $:
        f $: x = Application f x Unknown

        var name = Variable name Unknown Unknown

mkEnv :: [Definition] -> Either Error Env
mkEnv = fmap fst . foldM define (prelude, M.map (const Unknown) prelude)
    where
        define (env, spans) (Definition { defIdent = Identifier { idName, idSpan }, defBody })
            | Just conflict@(Known _ _) <- M.lookup idName spans = Left $ mkErrs
                [ ("the symbol " <> T.unpack idName <> " has already been defined", idSpan)
                , ("it was defined here", conflict)
                ]
            | Just Unknown <- M.lookup idName spans = Left $ mkErr ("the symbol " <> T.unpack idName <> " has already been defined in the prelude") idSpan
            | otherwise = Right $ (M.insert idName (Just defBody) env, M.insert idName idSpan spans)

evaluateNormal :: Tree -> ReaderT Env (Except Error) Tree
evaluateNormal tree = do
    tree' <- evaluate tree
    case tree' of
        Abstraction param paramSpan body treeSpan -> do
            body' <- local (M.insert param Nothing) $ evaluateNormal body
            pure $ Abstraction param paramSpan body' treeSpan

        Application f x treeSpan -> do
            f' <- evaluateNormal f
            x' <- evaluateNormal x
            pure $ Application f' x' treeSpan

        var@(Variable _ _ _) -> pure var

        Value inner _ -> evaluateNormal inner

        _ -> error "impossible"

evaluate :: Tree -> ReaderT Env (Except Error) Tree
evaluate = \case
    app@(Application f x _) -> do
        f' <- evaluate f
        case f' of
            Abstraction param _ body _ -> betaReduce param x body >>= evaluate
            _ -> pure app

    var@(Variable name _ treeSpan) -> do
        mval <- asks $ M.lookup name
        case mval of
            Nothing -> throwError $ mkErr ("the symbol " <> T.unpack name <> " could not be found") treeSpan
            Just Nothing -> pure var
            Just (Just val) -> evaluate val

    Value inner _ -> evaluate inner

    tree -> pure tree

betaReduce :: Monad m => Text -> Tree -> Tree -> ReaderT Env m Tree
betaReduce param arg = \case
    lam@(Abstraction param2 param2Span body2 treeSpan) -> do
        if param == param2
            then pure lam
            else do
                fvs <- freeVars arg
                if param2 `S.member` fvs
                    then do
                        fvs2 <- freeVars body2
                        let param2' = newName param2 (fvs `S.union` fvs2)
                        body2' <- rec $ alphaConvert param2 param2' body2
                        pure $ Abstraction param2' param2Span body2' treeSpan
                    else do
                        body2' <- rec body2
                        pure $ Abstraction param2 param2Span body2' treeSpan

    Application f x treeSpan -> do
        f' <- rec f
        x' <- rec x
        pure $ Application f' x' treeSpan

    var@(Variable name _ _)
        | param == name -> pure arg
        | otherwise -> pure var

    Value inner _ -> rec inner

    _ -> error "impossible"
    where
        rec = betaReduce param arg

freeVars :: Monad m => Tree -> ReaderT Env m (Set Text)
freeVars = go S.empty
    where
        go vs = \case
            Abstraction param _ body _ -> go (S.insert param vs) body

            Application f x _ -> S.union <$> go vs f <*> go vs x

            Variable name _ _ -> do
                mval <- asks $ M.lookup name
                case join mval of
                    Nothing
                        | name `S.member` vs -> pure S.empty
                        | otherwise          -> pure $ S.singleton name
                    Just val -> go (S.insert name vs) val

            Value inner _ -> go vs inner

            _ -> error "impossible"

alphaConvert :: Text -> Text -> Tree -> Tree
alphaConvert name1 name2 tree = case tree of
    Abstraction param paramSpan body treeSpan
        | param == name1 -> tree
        | otherwise      -> Abstraction param paramSpan (rec body) treeSpan

    Application f x treeSpan -> Application (rec f) (rec x) treeSpan

    Variable name nameSpan treeSpan
        | name == name1 -> Variable name2 nameSpan treeSpan
        | otherwise     -> tree

    Value inner treeSpan -> Value (rec inner) treeSpan

    _ -> error "impossible"
    where
        rec = alphaConvert name1 name2

newName :: Text -> Set Text -> Text
newName old used =
    let pre = T.dropWhileEnd isDigit old
    in head . filter (`S.notMember` used) $ (pre <>) . T.pack . (show @Integer) <$> [1..]

pretty :: Tree -> String
pretty = \case
    (Abstraction param _ body _) -> "λ" <> T.unpack param <> ". " <> pretty body

    (Application f x _) -> go True f <> " " <> go False x
        where
            go _ lam@(Abstraction _ _ _ _) = "(" <> pretty lam <> ")"
            go l app@(Application _ _ _) = if l then pretty app else "(" <> pretty app <> ")"
            go l (Value inner _) = go l inner
            go _ tree = pretty tree

    (Variable name _ _) -> T.unpack name

    (Value inner _) -> pretty inner

    _ -> error "impossible"
