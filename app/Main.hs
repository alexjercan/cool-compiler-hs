module Main where

import Lexer.Parsec qualified
import Parser.Parsec qualified
import Semantic.Definition qualified
import Token (formatErrorKind)

compile :: String -> String -> String
compile fn input = do
    case Lexer.Parsec.tokenize input of
        Left errs -> unlines $ map (formatErrorKind fn input) errs
        Right tis -> do
            case Parser.Parsec.ast tis of
                Left errs -> unlines $ map (formatErrorKind fn input) errs
                Right ast -> do
                    case Semantic.Definition.semantic ast of
                        Left errs -> unlines $ map (formatErrorKind fn input) errs
                        Right program -> show program

main :: IO ()
main = interact $ compile ""
