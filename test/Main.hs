module Main (main) where

import LexerTest (tests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    tests
