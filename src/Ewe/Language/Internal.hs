module Ewe.Language.Internal
    ( defaultParse
    ) where

import Ewe.Language.Common.Types
import Text.Megaparsec

defaultParse :: Parser a -> lang -> Source -> Either ParserError a
defaultParse p _ (path, src) = runParser p path src
