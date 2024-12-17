module HCat where

import Flow

import Data.Functor ((<&>))
import Data.Text (Text)

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Tuple.Extra qualified as Tuple
import System.Environment qualified as Env
import System.IO qualified as IO
import System.IO.Error qualified as IOError
import System.Info qualified as SysInfo
import System.Process qualified as Proc

data Result a where
    Err :: String -> Result a
    Ok :: a -> Result a

toIOError :: Result a -> IO a
toIOError (Ok a) = return a
toIOError (Err e) =
    IOError.userError e |> Exception.throwIO

data ScreenDimensions where
    ScreenDimensions
        :: {screenRows :: Int, screenCols :: Int}
        -> ScreenDimensions
    deriving (Show)

data UserInput where
    Quit :: UserInput
    Continue :: UserInput
    Noop :: UserInput
    deriving (Show)

handleArgs
    :: [String] -> Result FilePath
handleArgs [filepath] = Ok filepath
handleArgs [] = Err "ERROR: no arguments provided!"
handleArgs _ =
    Err
        "ERROR: too much arguments provided!"

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf 0 lst = [lst]
groupsOf n lst =
    let (x, xs) = splitAt n lst
     in x : groupsOf n xs

groupsOf' :: Int -> [a] -> [[a]]
groupsOf' n =
    splitAt n
        .> Tuple.second (groupsOf n)
        .> uncurry (:)

wordWrap :: Int -> Text -> [Text]
wordWrap maxColumn text
    | Text.length text <= maxColumn = [text]
    | otherwise =
        let
            (candidate, nextLines) = Text.splitAt maxColumn text
            (firstLine, overflow) =
                softWrap
                    candidate
                    (Text.length candidate - 1)
         in
            firstLine
                : wordWrap
                    maxColumn
                    (overflow <> nextLines)
  where
    softWrap hardwrappedText textIdx
        | textIdx <= 0 =
            (hardwrappedText, Text.empty)
        | Text.index hardwrappedText textIdx
            == ' ' =
            let (wrappedLine, rest) = Text.splitAt textIdx hardwrappedText
             in (wrappedLine, Text.tail rest)
        | otherwise =
            softWrap hardwrappedText (textIdx - 1)

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

paginates
    :: ScreenDimensions -> Text -> [Text]
paginates (ScreenDimensions rows cols) =
    Text.lines
        .> concatMap (wordWrap cols)
        .> groupsOf rows
        .> map Text.unlines

getTermSize :: IO ScreenDimensions
getTermSize = case SysInfo.os of
    unix
        | unix `elem` ["darwin", "linux"] ->
            tputScrDim
    _ -> return $ ScreenDimensions 25 80
  where
    tputScrDim :: IO ScreenDimensions
    tputScrDim = do
        rows <- getFromTput "lines"
        cols <- getFromTput "cols"
        return $
            ScreenDimensions (rows - 2) cols
      where
        getFromTput prop =
            read . init
                <$> Proc.readProcess "tput" [prop] mempty

getUserInput :: IO UserInput
getUserInput =
    IO.hSetBuffering IO.stdin IO.NoBuffering
        >> IO.hSetEcho IO.stdin False
        >> getChar
        <&> \case
            ' ' -> Continue
            'q' -> Quit
            _ -> Noop

run2 :: IO ()
run2 =
    putStrLn
        "do you want to continue (space) or quit (q)?"
        >> getUserInput
        >>= \case
            Continue -> putStrLn "Ok, continuing..." >> run2
            Quit -> putStrLn "Good bye..."
            Noop -> run2

showPages :: [Text] -> IO ()
showPages [] = return ()
showPages p@(page : pages) =
    clearScreen
        >> TextIO.putStrLn page
        >> getUserInput
        >>= \case
            Continue -> showPages pages
            Quit -> return ()
            Noop -> showPages p

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H" -- terminal magic, wow

run :: IO ()
run =
    withErrHandling <| Env.getArgs
        >>= handleArgs .> toIOError
        >>= flip IO.openFile IO.ReadMode
        >>= TextIO.hGetContents
        >>= \contents ->
            getTermSize
                >>= \size ->
                    paginates size contents |> showPages
  where
    withErrHandling :: IO () -> IO ()
    withErrHandling =
        Exception.handle <| \e ->
            putStr "HCAT: ERROR: "
                >> print @IOError e

runDo :: IO ()
runDo = do
    content <-
        Env.getArgs
            >>= handleArgs .> toIOError
            >>= flip IO.openFile IO.ReadMode
            >>= TextIO.hGetContents
    termSize <- getTermSize
    paginates termSize content
        |> showPages
        |> Exception.handle printError

printError
    :: Exception.IOException -> IO ()
printError e =
    putStr "HCAT: ERROR: "
        >> print @IOError e
