module Main where

import Lexer.Parsec qualified
import LexerTest (lexerTests)
import Test.Hspec (Spec, describe, hspec)

tests :: Spec
tests = do
    describe "Lexer.Parsec" $ lexerTests Lexer.Parsec.tokenize

main :: IO ()
main = hspec $ do
    tests
