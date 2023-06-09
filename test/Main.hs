module Main where

import Lexer.Parsec qualified
import LexerTest (lexerTests)
import Parser.Parsec qualified
import ParserTest (parserTests)
import Semantic.Definition qualified
import SemanticTest (semanticTests)
import Test.Hspec (Spec, describe, hspec)

tests :: Spec
tests = do
    describe "Lexer.Parsec" $ lexerTests Lexer.Parsec.tokenize
    describe "Parser.Parsec" $ parserTests Parser.Parsec.ast
    describe "Semantic.Definition" $ semanticTests Semantic.Definition.semantic

main :: IO ()
main = hspec $ do
    tests
