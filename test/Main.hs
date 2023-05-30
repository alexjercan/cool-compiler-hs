module Main (main) where

import LexerTest (testClass)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    testClass
