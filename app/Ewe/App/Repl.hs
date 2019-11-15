module Ewe.App.Repl
    ( repl
    ) where

import           Control.Monad.State.Strict
import           Data.List
import qualified Data.Text as T
import           Ewe.Language
import qualified Prelude
import           Prelude hiding (putStr, putStrLn)
import           System.Console.Repline

type Repl env = HaskelineT (StateT env IO)

handleInput :: Language lang => lang -> String -> Repl (Env lang) ()
handleInput lang input = do
    let source = ("<repl>", T.pack input)
    case parseDefn lang source of
        Left errDefn -> case parseExpr lang source of
            Left errExpr -> putStrLn . prettyParserError $
                if "=" `isInfixOf` input then errDefn else errExpr
            Right res -> do
                env <- get
                case evalExpr lang env res of
                    Left err -> putStrLn $ prettyError source err
                    Right x -> putStrLn $ pretty lang x
        Right defn -> do
            env <- get
            case evalDefn lang env defn of
                Left err -> putStrLn $ prettyError source err
                Right (_, env') -> put env'

commands :: Language lang => lang -> [(String, [String] -> Repl (Env lang) ())]
commands lang =
    [ ("help", help)
    , ("type", showType)
    , ("t", showType)
    , ("quit", quit)
    , ("q", quit)
    ]
    where
        help _ = putStr . unlines $
            [ "Input a valid top-level definition to define a symbol or an expression to evaluate it."
            , "Press <TAB> for autocomplete."
            , "Type :type or :t to get the type of an expression."
            , "Type :quit or :q to exit."
            ]
        showType xs =
            let source = ("<repl>", T.pack $ unwords xs)
            in case parseExpr lang source of
                Left errExpr -> putStrLn $ prettyParserError errExpr
                Right expr -> do
                    env <- get
                    case typeof lang env expr of
                        Left errTyp -> putStrLn $ prettyError source errTyp
                        Right typ -> putStrLn typ
        quit _ = abort

completer :: (Language lang, Monad m, MonadState (Env lang) m) => lang -> WordCompleter m
completer lang x = do
    xs <- gets $ map T.unpack . envKeys lang
    pure $ filter (isPrefixOf x) xs

initial :: Language lang => lang -> Repl (Env lang) ()
initial lang = putStr . unlines $
    [ "Welcome to the ewe REPL!"
    , "Language in use: " <> langName lang
    , "Type :help for help."
    ]

repl :: Language lang => lang -> IO ()
repl lang = evalStateT (evalRepl prompt handle cmds cmdPrefix compl splash) env
    where
        prompt = pure $ langName lang <> "> "
        handle = handleInput lang
        cmds = commands lang
        cmdPrefix = Just ':'
        compl = Word (completer lang)
        splash = initial lang
        env = prelude lang

putStr :: MonadIO m => String -> m ()
putStr = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn
