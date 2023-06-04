module ParserTest (parserTests) where

import AST
import Test.Hspec (Spec, describe, it, shouldBe)
import Token

parserTests :: Ast -> Spec
parserTests ast = do
    testClass ast

testClass :: Ast -> Spec
testClass ast = do
    describe "Lexer" $ do
        it "should ast class" $
            do
                ast [Class, Type "A", LeftSquirly, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [], ClassDefinition "B" (Just "A") []])
