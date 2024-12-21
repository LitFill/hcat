module HCat where

import Flow

import Data.Functor ((<&>))
import Data.Text (Text)
import Text.Printf (printf)

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock qualified as Clock
import Data.Time.Format qualified as TimeFmt
import System.Directory qualified as Dir
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

data FileInfo where
    FileInfo
        :: { filePath :: FilePath
           , fileSize :: Int
           , fileMTime :: Clock.UTCTime
           , fileReadable :: Bool
           , fileWritable :: Bool
           , fileExecutable :: Bool
           }
        -> FileInfo
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

paginates
    :: ScreenDimensions
    -> FileInfo
    -> Text
    -> [Text]
paginates (ScreenDimensions rows cols) finfo text =
    let pages =
            text
                |> Text.lines
                |> concatMap (wordWrap cols)
                |> groupsOf rows
                |> map
                    ( (<> repeat "") -- padding the status line
                        .> take rows
                        .> Text.unlines
                    )
     in [1 .. length pages]
            |> map
                (fmtFileInfo finfo cols (length pages))
            |> zipWith (<>) pages

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
            {-here (-2) is 1 for my terminal offset, and 1 for status line-}
            ScreenDimensions (rows - 2) cols
      where
        getFromTput prop =
            read . init
                <$> Proc.readProcess "tput" [prop] mempty

getUserInput :: IO UserInput
getUserInput = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False
    getChar <&> \case
        ' ' -> Continue
        'q' -> Quit
        _ -> Noop

showPages :: [Text] -> IO ()
showPages [] = return ()
showPages p@(page : pages) = do
    clearScreen
    TextIO.putStrLn page
    getUserInput >>= \case
        Continue -> showPages pages
        Quit -> return ()
        Noop -> showPages p

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H" -- terminal magic, wow

fileInfo :: FilePath -> IO FileInfo
fileInfo fpath = do
    perms <- Dir.getPermissions fpath
    mtime <- Dir.getModificationTime fpath
    size <- BS.length <$> BS.readFile fpath
    return
        FileInfo
            { filePath = fpath
            , fileSize = size
            , fileMTime =
                mtime |> Clock.addUTCTime (7 * 60 * 60)
            , fileReadable = Dir.readable perms
            , fileWritable = Dir.writable perms
            , fileExecutable = Dir.executable perms
            }

fmtFileInfo
    :: FileInfo -> Int -> Int -> Int -> Text
fmtFileInfo
    ( FileInfo
            fpath
            fsize
            fmtime
            frdable
            fwrable
            fexeable
        )
    maxWidth
    ttlPages
    crntPage =
        printf
            "%s | perm: %s | %d bytes | modified: %s | page %d of %d"
            fpath
            permString
            fsize
            timestamp
            crntPage
            ttlPages
            |> truncateStatusLn
            |> Text.pack
            |> ("\^[[7m" <>) -- invert color
            |> (<> "\^[[0m") -- reset color
      where
        truncateStatusLn statusLn
            | maxWidth <= 3 = ""
            | length statusLn > maxWidth =
                take (maxWidth - 3) statusLn
                    <> "..."
            | otherwise =
                statusLn
                    |> (<> repeat ' ')
                    |> take maxWidth

        permString =
            [ if frdable then 'r' else '-'
            , if fwrable then 'w' else '-'
            , if fexeable then 'x' else '-'
            ]
        timestamp =
            TimeFmt.formatTime
                TimeFmt.defaultTimeLocale
                "%F %T"
                fmtime

run :: IO ()
run = Exception.handle printError $ do
    fpath <-
        toIOError <. handleArgs =<< Env.getArgs
    contents <-
        TextIO.hGetContents
            =<< IO.openFile fpath IO.ReadMode
    finfo <- fileInfo fpath
    size <- getTermSize
    showPages
        <| paginates size finfo contents

printError
    :: Exception.IOException -> IO ()
printError e = putStr "HCAT: ERROR: " >> print e
