module Main where

import Lexer.Parsec qualified
import Parser.Parsec qualified
import Token

compile :: String -> String
compile input = do
    case Lexer.Parsec.tokenize input of
        Left errs -> unlines errs
        Right tis -> do
            case Parser.Parsec.ast (map token tis) of
                Left errs -> unlines errs
                Right ast -> show ast

main :: IO ()
main = interact compile
