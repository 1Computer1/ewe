module Ewe.Language.Untyped.Evaluator
    ( Error
    , Env
    , prelude
    , mkEnv
    , evaluateNormal
    , pretty
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Char
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text (Text)
import           Ewe.Language.Common.Error
import           Ewe.Language.Common.Types
import           Ewe.Language.Untyped.Parser

type Env = Map Text (Maybe Expr)

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
        p .: b = Abs Unknown (Ident Unknown p) b

        infixl 3 $:
        f $: x = App Unknown f x

        var name = Var Unknown (Ident Unknown name)

mkEnv :: [Defn] -> Either Error Env
mkEnv = fmap fst . foldM define (prelude, M.map (const Unknown) prelude)
    where
        define (env, spans) (Defn _ (Ident nameSp name) body)
            | Just conflict@(Known _ _) <- M.lookup name spans = Left $ mkErrs
                [ (nameSp, "the symbol " <> T.unpack name <> " has already been defined")
                , (conflict, "it was defined here")
                ]
            | Just Unknown <- M.lookup name spans = Left $
                mkErr nameSp $ "the symbol " <> T.unpack name <> " has already been defined in the prelude"
            | otherwise = Right (M.insert name (Just body) env, M.insert name nameSp spans)

evaluateNormal :: Expr -> ReaderT Env (Except Error) Expr
evaluateNormal expr = do
    expr' <- evaluate expr
    case expr' of
        Abs sp (Ident paramSp param) body -> do
            body' <- local (M.insert param Nothing) $ evaluateNormal body
            pure $ Abs sp (Ident paramSp param) body'

        App sp f x -> do
            f' <- evaluateNormal f
            x' <- evaluateNormal x
            pure $ App sp f' x'

        var@(Var _ _) -> pure var

        Val _ inner -> evaluateNormal inner

evaluate :: Expr -> ReaderT Env (Except Error) Expr
evaluate = \case
    app@(App _ f x) -> do
        f' <- evaluate f
        case f' of
            Abs _ (Ident _ param) body -> betaReduce param x body >>= evaluate
            _ -> pure app

    var@(Var sp (Ident _ name)) -> do
        mval <- asks $ M.lookup name
        case mval of
            Nothing -> throwError $ mkErr sp ("the symbol " <> T.unpack name <> " could not be found")
            Just Nothing -> pure var
            Just (Just val) -> evaluate val

    Val _ inner -> evaluate inner

    tree -> pure tree

betaReduce :: Monad m => Text -> Expr -> Expr -> ReaderT Env m Expr
betaReduce toSub subExpr = \case
    lam@(Abs sp (Ident paramSp param) body) -> do
        if param == toSub
            then pure lam
            else do
                fvs <- freeVars subExpr
                if param `S.member` fvs
                    then do
                        fvs2 <- freeVars body
                        let param' = generateName param (fvs `S.union` fvs2)
                        body' <- go $ alphaConvert param param' body
                        pure $ Abs sp (Ident paramSp param') body'
                    else do
                        body' <- go body
                        pure $ Abs sp (Ident paramSp param) body'

    App sp f x -> do
        f' <- go f
        x' <- go x
        pure $ App sp f' x'

    var@(Var _ (Ident _ name))
        | name == toSub -> pure subExpr
        | otherwise -> pure var

    Val _ inner -> go inner
    where
        go = betaReduce toSub subExpr

freeVars :: Monad m => Expr -> ReaderT Env m (Set Text)
freeVars = go S.empty
    where
        go vs = \case
            Abs _ (Ident _ param) body -> go (S.insert param vs) body

            App _ f x -> S.union <$> go vs f <*> go vs x

            Var _ (Ident _ name) -> do
                mval <- asks $ M.lookup name
                case join mval of
                    Nothing
                        | name `S.member` vs -> pure S.empty
                        | otherwise -> pure $ S.singleton name
                    Just val -> go (S.insert name vs) val

            Val _ inner -> go vs inner

alphaConvert :: Text -> Text -> Expr -> Expr
alphaConvert oldName newName = \case
    lam@(Abs sp (Ident paramSp param) body)
        | param == oldName -> lam
        | otherwise -> Abs sp (Ident paramSp param) (go body)

    App sp f x -> App sp (go f) (go x)

    var@(Var sp (Ident identSp identText))
        | identText == oldName -> Var sp (Ident identSp newName)
        | otherwise -> var

    Val sp inner -> Val sp (go inner)
    where
        go = alphaConvert oldName newName

generateName :: Text -> Set Text -> Text
generateName old used =
    let pre = T.dropWhileEnd isDigit old
    in head . filter (`S.notMember` used) $ (pre <>) . T.pack . (show @Integer) <$> [1..]

pretty :: Expr -> String
pretty = \case
    Abs _ (Ident _ param) body -> "λ" <> T.unpack param <> ". " <> pretty body

    App _ f x -> go True f <> " " <> go False x
        where
            go _ lam@(Abs _ _ _) = "(" <> pretty lam <> ")"
            go l app@(App _ _ _) = if l then pretty app else "(" <> pretty app <> ")"
            go l (Val _ inner) = go l inner
            go _ tree = pretty tree

    Var _ (Ident _ name) -> T.unpack name

    Val _ inner -> pretty inner
