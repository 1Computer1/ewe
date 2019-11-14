{-# LANGUAGE ExistentialQuantification #-}

module Ewe.App.Types
    ( ILang(..)
    , Command(..)
    ) where

import Data.Text (Text)
import Ewe.Language

data ILang = forall a. Language a => ILang a

data Command
    = RunProgramFile ILang FilePath
    | RunProgram ILang Text
    | RunExpr ILang Text
    | RunRepl ILang
