module Main where

import System.Environment
import System.Exit

import Text.ParserCombinators.Parsec

import Deturgenchry

main = do
    [fileName] <- getArgs
    result <- parseFromFile (program) fileName
    case (result) of
        Left err -> do
            print err
            exitWith $ ExitFailure 1
        Right prog -> do
            interpret prog
            exitWith $ ExitFailure 0
