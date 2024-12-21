module Internal where

import Flow
import HCat

import Data.Text (Text)

import Control.Exception.Extra qualified as Exception
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Tuple.Extra qualified as Tuple
import System.Environment qualified as Env
import System.IO.Extra qualified as IO

runDo :: IO ()
runDo = do
    content <-
        Env.getArgs
            >>= handleArgs .> toIOError
            >>= flip IO.openFile IO.ReadMode
            >>= TextIO.hGetContents
    termSize <- getTermSize
    Internal.paginates termSize content
        |> showPages
        |> Exception.handle printError

run2 :: IO ()
run2 = do
    putStrLn
        "do you want to continue (space) or quit (q)?"
    getUserInput >>= \case
        Continue -> putStrLn "Ok, continuing..." >> run2
        Quit -> putStrLn "Good bye..."
        Noop -> run2

paginates
    :: ScreenDimensions -> Text -> [Text]
paginates (ScreenDimensions rows cols) =
    Text.lines
        .> concatMap (wordWrap cols)
        .> groupsOf rows
        .> map Text.unlines

paginates2
    :: ScreenDimensions
    -> FileInfo
    -> Text
    -> [Text]
paginates2 (ScreenDimensions rows cols) finfo text =
    let
        wrappedLine =
            concatMap
                (wordWrap cols)
                (Text.lines text)
        pages =
            wrappedLine
                |> groupsOf rows
                |> map
                    ( (<> repeat "")
                        .> take rows
                        .> Text.unlines
                    )
        pageCount = length pages
        statusLn =
            map
                (fmtFileInfo finfo cols pageCount)
                [1 .. pageCount]
     in
        zipWith (<>) pages statusLn

groupsOf' :: Int -> [a] -> [[a]]
groupsOf' n =
    splitAt n
        .> Tuple.second (groupsOf n)
        .> uncurry (:)

wordWrap' :: Int -> Text -> [Text]
wordWrap' maxCol text =
    Text.splitAt maxCol text
        |> Tuple.first
            (\x -> softWrap x <| Text.length x - 1)
        |> uncurry recurse
  where
    softWrap hdwrpdTxt txtIdx
        | txtIdx <= 0 = (hdwrpdTxt, Text.empty)
        | Text.index hdwrpdTxt txtIdx == ' ' =
            Text.splitAt txtIdx hdwrpdTxt
                |> Tuple.second Text.tail
        | otherwise =
            softWrap hdwrpdTxt (txtIdx - 1)
    recurse (firstLn, ovrflw) nextLn =
        firstLn
            : wordWrap' maxCol (ovrflw <> nextLn)
