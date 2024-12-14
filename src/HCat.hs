{-# LANGUAGE LambdaCase #-}

module HCat where

import Flow
import System.Environment qualified as Env

data Result a = Err String | Ok a

toEither :: Result a -> Either String a
toEither (Ok a) = Right a
toEither (Err e) = Left e

handleArgs
    :: [String] -> Result FilePath
handleArgs [filepath] = Ok filepath
handleArgs [] = Err "ERROR: no arguments provided!"
handleArgs _ =
    Err
        "ERROR: too much arguments provided!"

run :: IO ()
run =
    Env.getArgs
        >>= handleArgs
            .> toEither
            .> either ("HCAT: " <>) ("input: " <>)
            .> print
