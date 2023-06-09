module Main where

import Lexer.Parsec qualified
import Parser.Parsec qualified
import Token (formatErrorKind)

compile :: String -> String
compile input = do
    case Lexer.Parsec.tokenize input of
        Left errs -> unlines $ map (formatErrorKind "" input) errs
        Right tis -> do
            case Parser.Parsec.ast tis of
                Left errs -> unlines $ map (formatErrorKind "" input) errs
                Right ast -> show ast

main :: IO ()
main = interact compile
