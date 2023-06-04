module Main where

import Lexer.Parsec qualified
import LexerTest (lexerTests)
import Parser.Parsec qualified
import ParserTest (parserTests)
import Test.Hspec (Spec, describe, hspec)

tests :: Spec
tests = do
    describe "Lexer.Parsec" $ lexerTests Lexer.Parsec.tokenize
    describe "Parser.Parsec" $ parserTests Parser.Parsec.ast

main :: IO ()
main = hspec $ do
    tests
