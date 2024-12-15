module HCat where

import Control.Exception qualified as Exception
import Data.Text.IO qualified as TextIO
import System.Environment qualified as Env
import System.IO.Error qualified as IOError

import Flow

data Result a = Err String | Ok a

toIOError :: Result a -> IO a
toIOError (Ok a) = return a
toIOError (Err e) =
    IOError.userError e |> Exception.throwIO

handleArgs
    :: [String] -> Result FilePath
handleArgs [filepath] = Ok filepath
handleArgs [] = Err "ERROR: no arguments provided!"
handleArgs _ =
    Err
        "ERROR: too much arguments provided!"

run :: IO ()
run =
    withErrHandling
        <| Env.getArgs
        >>= handleArgs .> toIOError
        >>= TextIO.readFile
        >>= TextIO.putStrLn
  where
    withErrHandling :: IO () -> IO ()
    withErrHandling =
        Exception.handle <| \e ->
            putStr "HCAT: ERROR: "
                >> print @IOError e
