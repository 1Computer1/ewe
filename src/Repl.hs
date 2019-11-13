module Repl (repl) where

import           Control.Monad.State.Strict
import           Data.List

import qualified Data.Map as M
import qualified Data.Text as T
import           Ewe

import           Ewe.Evaluator (Env, prelude)
import           Ewe.Parser (Defn(..), Ident(..))
import           System.Console.Repline

type Repl = HaskelineT (StateT Env IO)

handleInput :: String -> Repl ()
handleInput input = do
    let src = T.pack input
        path = "<repl>"
    case parseDefinition path src of
        Left err1 -> case parseExpression path src of
            Left err2 -> if any (`isInfixOf` input) ["=", ":=", "≔", "≝", "≡"]
                then liftIO $ putStrLn err1
                else liftIO $ putStrLn err2
            Right res -> do
                env <- get
                case evaluateExpression' path src env res of
                    Left err -> liftIO $ putStrLn err
                    Right x  -> liftIO $ putStrLn x
        Right (Defn _ (Ident _ name) body) -> modify' (M.insert name (Just body))

commands :: [(String, [String] -> Repl ())]
commands =
    [ ("help", help)
    , ("quit", quit)
    , ("q", quit)
    ]
    where
        help _ = liftIO . putStr . unlines $
            [ "Input a valid top-level definition to define a symbol or an expression to evaluate it."
            , "Press <TAB> for autocomplete."
            , "Type :quit or :q to exit."
            ]
        quit _ = abort

completer :: (Monad m, MonadState Env m) => WordCompleter m
completer x = do
    xs <- gets $ map T.unpack . M.keys
    pure $ filter (isPrefixOf x) xs

initial :: Repl ()
initial = liftIO . putStr . unlines $
    [ "Welcome to the ewe REPL!"
    , "Type :help for help."
    ]

repl :: IO ()
repl = evalStateT (evalRepl (pure "λ> ") handleInput commands (Just ':') (Word completer) initial) prelude
