module Main where

import Lexer.Parsec qualified

compile :: String -> String
compile input = do
    case Lexer.Parsec.tokenize input of
        Left errs -> unlines errs
        Right ts -> unlines $ map show ts

main :: IO ()
main = interact compile
