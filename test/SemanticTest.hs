module SemanticTest (semanticTests) where

import AST
import Scope
import Test.Hspec (Spec, describe, it, shouldBe)
import Token

import Data.Either (fromLeft, fromRight)
import Lexer.Parsec qualified
import Parser.Parsec qualified

semanticTests :: Semantic -> Spec
semanticTests semantic = do
    testDefineClass semantic
    testDefineAttribute semantic

testDefineClass :: Semantic -> Spec
testDefineClass semantic = do
    describe "Semantic" $ do
        it "should check class" $
            do
                let fn = "01-define-class.cl"
                let input = unlines ["class BB__ inherits A {};", "", "class Int {};", "", "class C inherits Int {};", "", "class SELF_TYPE {};", "", "class D inherits SELF_TYPE {};", "", "class D {};", "", "class E inherits F {};", "class F inherits G {};", "class G inherits E {};"]
                map (formatErrorKind fn input) $ fromLeft [] $ semantic $ fromRight (Program []) $ Parser.Parsec.ast $ fromRight [] $ Lexer.Parsec.tokenize input
                `shouldBe` [ "\"01-define-class.cl\", line 3:7, Semantic error: Class Int is redefined"
                           , "\"01-define-class.cl\", line 7:7, Semantic error: Class has illegal name SELF_TYPE"
                           , "\"01-define-class.cl\", line 11:7, Semantic error: Class D is redefined"
                           , "\"01-define-class.cl\", line 1:21, Semantic error: Class BB__ has undefined parent A"
                           , "\"01-define-class.cl\", line 5:18, Semantic error: Class C has illegal parent Int"
                           , "\"01-define-class.cl\", line 9:18, Semantic error: Class D has illegal parent SELF_TYPE"
                           , "\"01-define-class.cl\", line 13:7, Semantic error: Inheritance cycle for class E"
                           , "\"01-define-class.cl\", line 14:7, Semantic error: Inheritance cycle for class F"
                           , "\"01-define-class.cl\", line 15:7, Semantic error: Inheritance cycle for class G"
                           ]

testDefineAttribute :: Semantic -> Spec
testDefineAttribute semantic = do
    describe "Semantic" $ do
        it "should check attribute" $
            do
                let fn = "02-define-attribute.cl"
                let input = unlines ["class B inherits A {", "    x : Int;", "};", "", "class A {", "    self : Int;", "    x    : Bool;", "    x    : Int;", "    y    : C;", "    x    : C;", "};"]
                map (formatErrorKind fn input) $ fromLeft [] $ semantic $ fromRight (Program []) $ Parser.Parsec.ast $ fromRight [] $ Lexer.Parsec.tokenize input
                `shouldBe` [ "\"02-define-attribute.cl\", line 6:5, Semantic error: Class A has attribute with illegal name self"
                           , "\"02-define-attribute.cl\", line 8:5, Semantic error: Class A redefines attribute x"
                           , "\"02-define-attribute.cl\", line 9:12, Semantic error: Class A has attribute y with undefined type C"
                           , "\"02-define-attribute.cl\", line 10:5, Semantic error: Class A redefines attribute x"
                           , "\"02-define-attribute.cl\", line 2:5, Semantic error: Class B redefines inherited attribute x"
                           ]
