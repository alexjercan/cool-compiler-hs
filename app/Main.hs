module Main where

import Lexer.Parsec qualified
import Parser.Parsec qualified

compile :: String -> String
compile input = do
    case Lexer.Parsec.tokenize input of
        Left errs -> unlines errs
        Right tis -> do
            case Parser.Parsec.ast tis of
                Left errs -> unlines errs
                Right ast -> show ast

main :: IO ()
main = interact compile
