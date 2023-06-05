module ParserTest (parserTests) where

import AST
import Test.Hspec (Spec, describe, it, shouldBe)
import Token

parserTests :: Ast -> Spec
parserTests ast = do
    testClass ast
    testAttributeNoInit ast
    testAttributeInit ast
    testMethod ast
    testLiteralId ast
    testArithmetic ast
    testRelational ast
    testAssignment ast
    testNewIsVoid ast
    testDispatch ast

testClass :: Ast -> Spec
testClass ast = do
    describe "Parser" $ do
        it "should ast class" $
            do
                ast [Class, Type "A", LeftSquirly, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [], ClassDefinition "B" (Just "A") []])

testAttributeNoInit :: Ast -> Spec
testAttributeNoInit ast = do
    describe "Parser" $ do
        it "should ast attribute no init" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [AttributeDefinition "x" "SELF_TYPE" Nothing], ClassDefinition "B" (Just "A") [AttributeDefinition "y" "Int" Nothing]])

testAttributeInit :: Ast -> Spec
testAttributeInit ast = do
    describe "Parser" $ do
        it "should ast attribute init" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [AttributeDefinition "x" "SELF_TYPE" Nothing], ClassDefinition "B" (Just "A") [AttributeDefinition "y" "Int" (Just (IntegerLiteral 0))]])

testMethod :: Ast -> Spec
testMethod ast = do
    describe "Parser" $ do
        it "should ast method" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, Ident "f", LeftParen, RightParen, Colon, Type "Object", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Bool", RightParen, Colon, Type "Int", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [AttributeDefinition "x" "SELF_TYPE" Nothing, MethodDefinition "f" [] "Object" (IntegerLiteral 0)], ClassDefinition "B" (Just "A") [AttributeDefinition "y" "Int" (Just (IntegerLiteral 0)), MethodDefinition "g" [Formal "x" "Int", Formal "y" "Bool"] "Int" (IntegerLiteral 0)]])

testLiteralId :: Ast -> Spec
testLiteralId ast = do
    describe "Parser" $ do
        it "should ast literal" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "s", Colon, Type "String", Assign, String "abc", SemiColon, Ident "b", Colon, Type "Bool", Assign, Boolean False, SemiColon, Ident "c", Colon, Type "Bool", Assign, Ident "b", SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [AttributeDefinition "n" "Int" (Just (IntegerLiteral 0)), AttributeDefinition "s" "String" (Just (StringLiteral "abc")), AttributeDefinition "b" "Bool" (Just (BoolLiteral False)), AttributeDefinition "c" "Bool" (Just (IdentStatement "b"))]])

testArithmetic :: Ast -> Spec
testArithmetic ast = do
    describe "Parser" $ do
        it "should ast arithmetic" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 1, Plus, Integer 2, Asterisk, LeftParen, Integer 3, Plus, Integer 4, RightParen, Slash, LeftParen, Integer 5, Minus, Tilde, Integer 6, RightParen, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right (Program [ClassDefinition "A" Nothing [AttributeDefinition "n" "Int" (Just $ AddStatement (IntegerLiteral 1) (DivStatement (MulStatement (IntegerLiteral 2) (ParenStatement (AddStatement (IntegerLiteral 3) (IntegerLiteral 4)))) (ParenStatement (SubStatement (IntegerLiteral 5) (NegationStatement (IntegerLiteral 6))))))]])

testRelational :: Ast -> Spec
testRelational ast = do
    describe "Parser" $ do
        it "should ast relational" $
            do
                ast [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", SemiColon, Ident "b", Colon, Type "Bool", Assign, Not, Integer 2, LessEqual, Ident "n", SemiColon, Ident "c", Colon, Type "Bool", Assign, Integer 2, LessThan, Ident "n", Plus, Integer 1, Equal, Boolean False, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ AttributeDefinition "n" "Int" Nothing
                            , AttributeDefinition
                                "b"
                                "Bool"
                                (Just $ NotStatement (LessThanOrEqualStatement (IntegerLiteral 2) (IdentStatement "n")))
                            , AttributeDefinition
                                "c"
                                "Bool"
                                ( Just $
                                    EqualStatement
                                        (LessThanStatement (IntegerLiteral 2) (AddStatement (IdentStatement "n") (IntegerLiteral 1)))
                                        (BoolLiteral False)
                                )
                            ]
                        ]
                    )

testAssignment :: Ast -> Spec
testAssignment ast = do
    describe "Parser" $ do
        it "should ast assignment" $ do
                ast [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "Int", SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", RightSquirly, SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", Comma, Ident "z", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", Assign, Ident "z", RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ AttributeDefinition "x" "Int" Nothing
                            , MethodDefinition
                                "f"
                                [Formal "y" "Int"]
                                "Int"
                                (AssignStatement "x" (IdentStatement "y"))
                            , MethodDefinition
                                "f"
                                [Formal "y" "Int", Formal "z" "Int"]
                                "Int"
                                (AssignStatement "x" (AssignStatement "y" (IdentStatement "z")))
                            ]
                        ]
                    )

testNewIsVoid :: Ast -> Spec
testNewIsVoid ast = do
    describe "Parser" $ do
        it "should ast new isvoid" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, RightParen, Colon, Type "Bool", LeftSquirly, Not, IsVoid, New, Type "A", RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                []
                                "Bool"
                                (NotStatement (IsVoidStatement (NewStatement "A")))
                            ]
                        ]
                    )

testDispatch :: Ast -> Spec
testDispatch ast = do
    describe "Parser" $ do
        it "should ast dispatch" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "f", LeftParen, Ident "x", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "self", Dot, Ident "g", LeftParen, Ident "x", Plus, Integer 1, Comma, Ident "y", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "h", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "A", LeftSquirly, New, Type "A", At, Type "A", Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 1, RightParen, Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 2, RightParen, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Int"]
                                "Bool"
                                (MethodCall Nothing Nothing "f" [AddStatement (IdentStatement "x") (IntegerLiteral 1)])
                            , MethodDefinition
                                "g"
                                [Formal "x" "Int", Formal "y" "Int"]
                                "Bool"
                                (MethodCall (Just (IdentStatement "self")) Nothing "g" [AddStatement (IdentStatement "x") (IntegerLiteral 1), AddStatement (IdentStatement "y") (IntegerLiteral 1)])
                            , MethodDefinition
                                "h"
                                [Formal "x" "Int"]
                                "A"
                                ( MethodCall
                                    ( Just
                                        ( MethodCall
                                            (Just (NewStatement "A"))
                                            (Just "A")
                                            "h"
                                            [AddStatement (IdentStatement "x") (IntegerLiteral 1)]
                                        )
                                    )
                                    Nothing
                                    "h"
                                    [AddStatement (IdentStatement "x") (IntegerLiteral 2)]
                                )
                            ]
                        ]
                    )
