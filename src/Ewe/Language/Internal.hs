module Ewe.Language.Internal where

import Ewe.Language.Types
import Text.Megaparsec

defaultParse :: Parser a -> lang -> Source -> Either ParserError a
defaultParse p _ (path, src) = runParser p path src
