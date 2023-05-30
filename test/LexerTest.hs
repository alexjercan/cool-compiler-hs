{-# LANGUAGE OverloadedStrings #-}

module LexerTest where

import Lexer (Token (..), tokenize)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)

testClass :: Spec
testClass = do
    describe "Lexer" $ do
        it "should tokenize class" $ do
            tokenize (unlines ["class A {};", "", "class B inherits A {};"]) `shouldParse` [Class, Type "A", LeftSquirly, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, RightSquirly, SemiColon]
