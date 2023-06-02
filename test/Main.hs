module Main (main) where

import Lexer qualified
import LexerTest (lexerTests)
import Test.Hspec (Spec, describe, hspec)

tests :: Spec
tests = do
    describe "Lexer" $ lexerTests Lexer.tokenize

main :: IO ()
main = hspec $ do
    tests
