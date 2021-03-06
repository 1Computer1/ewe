module Ewe.Language.Common.Error
    ( mkErr
    , mkErrs
    , prettyError
    , prettyParserError
    ) where

import           Data.List (intercalate)
import qualified Data.Text as T
import           Ewe.Language.Common.Types
import           Text.Megaparsec

mkErr :: Span -> String -> Error
mkErr sp msg = Error [ErrorData sp msg]

mkErrs :: [(Span, String)] -> Error
mkErrs = Error . map (uncurry ErrorData)

formatLine
    :: SourcePos
    -> String
    -> Int
    -> String
    -> String
formatLine spos offendingLine pointerLength msg = formatted
    where
        formatted = intercalate "\n"
            [ name <> ":" <> show line <> ":" <> show col <> ":"
            , padding <> " |"
            , show line <> " | " <> offendingLine
            , padding <> " | " <> pointer
            , msg
            ]
        padding = replicate (length (show line)) ' '
        pointer = replicate (col - 1) ' ' <> replicate pointerLength '^'
        (name, line, col) = extractSourcePos spos

formatLines
    :: (SourcePos, SourcePos)
    -> [String]
    -> String
    -> String
formatLines (spos1, spos2) errLines msg = formatted
    where
        formatted = intercalate "\n"
            [ name <> ":" <> show line1 <> ":" <> show col1 <> "-" <> show line2 <> ":" <> show col2 <> ":"
            , padding <> " | " <> pointer1
            , lineN1 <> " | " <> head errLines <> case midLines of { "" -> ""; x -> "\n" <> x; }
            , lineN2 <> " | " <> last errLines
            , padding <> " | " <> pointer2
            , msg
            ]
        midLines = intercalate "\n" . map makeLine . init . tail $ errLines
        makeLine line = padding <> " | " <> line
        lineNPadded x = if length x /= padSize
            then replicate (padSize - length x) ' ' <> x
            else x
        lineN1 = lineNPadded $ show line1
        lineN2 = lineNPadded $ show line2
        padding = replicate padSize ' '
        padSize = max (length . show $ line1) (length . show $ line2)
        pointer1Length = maximum . map length . init $ errLines
        pointer1 = replicate (col1 - 1) ' ' <> replicate (pointer1Length - col1 + 1) 'v'
        pointer2 = replicate (col2 - 1) '^'
        (name, line1, col1) = extractSourcePos spos1
        (_,    line2, col2) = extractSourcePos spos2

initialPosState :: FilePath -> T.Text -> PosState T.Text
initialPosState file input = PosState
    { pstateInput = input
    , pstateOffset = 0
    , pstateSourcePos = initialPos file
    , pstateTabWidth = mkPos 0
    , pstateLinePrefix = ""
    }

extractSourcePos :: SourcePos -> (FilePath, Int, Int)
extractSourcePos (SourcePos { sourceName, sourceLine, sourceColumn }) = (sourceName, unPos sourceLine, unPos sourceColumn)

prettyError :: Source -> Error -> String
prettyError (path, src) (Error xs) = intercalate "\n\n" (map format xs)
    where
        format (ErrorData (Known so eo) msg) =
            if length offendingLines == 1
                then formatLine spos1 (head offendingLines) pointerLength msg
                else formatLines (spos1, spos2) offendingLines msg
            where
                (spos1@SourcePos { sourceLine = (unPos -> line1) }, state) = reachOffsetNoLine so posState
                (spos2@SourcePos { sourceLine = (unPos -> line2) }, _)     = reachOffsetNoLine eo state
                posState = initialPosState path src
                offendingLines = map T.unpack . take (line2 - line1 + 1) . drop (line1 - 1) $ T.lines src
                pointerLength = eo - so
        format (ErrorData Unknown msg) = path <> ":\n" <> msg

prettyParserError :: ParserError -> String
prettyParserError = errorBundlePretty
