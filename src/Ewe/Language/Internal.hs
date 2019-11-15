module Ewe.Language.Internal
    ( defaultParse
    , runUsual
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Ewe.Language.Common.Types
import Text.Megaparsec

defaultParse :: Parser a -> lang -> Source -> Either ParserError a
defaultParse p _ (path, src) = runParser p path src

runUsual :: r -> ReaderT r (Except e) a -> Either e a
runUsual env = runExcept . flip runReaderT env
