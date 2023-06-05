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
    testIf ast
    testWhile ast
    testLet ast
    testCase ast
    testBlock ast
    testStringSpecialChars ast
    testBig ast

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

testIf :: Ast -> Spec
testIf ast = do
    describe "Parser" $ do
        it "should ast if" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, If, Ident "x", LessEqual, Integer 5, Then, Ident "x", Else, Ident "x", Plus, Integer 1, Fi, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Int"]
                                "Int"
                                (IfStatement (LessThanOrEqualStatement (IdentStatement "x") (IntegerLiteral 5)) (IdentStatement "x") (AddStatement (IdentStatement "x") (IntegerLiteral 1)))
                            ]
                        ]
                    )

testWhile :: Ast -> Spec
testWhile ast = do
    describe "Parser" $ do
        it "should ast while" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Object", LeftSquirly, While, Integer 0, LessThan, Ident "x", Loop, Ident "x", Assign, Ident "x", Minus, Integer 1, Pool, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Int"]
                                "Object"
                                (WhileStatement (LessThanStatement (IntegerLiteral 0) (IdentStatement "x")) (AssignStatement "x" (SubStatement (IdentStatement "x") (IntegerLiteral 1))))
                            ]
                        ]
                    )

testLet :: Ast -> Spec
testLet ast = do
    describe "Parser" $ do
        it "should ast let" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Let, Ident "x", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, Comma, Ident "y", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, In, Ident "x", RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Int"]
                                "Int"
                                (LetStatement
                                    [ VariableDefinition "x" "Int" $ Just (AddStatement (IdentStatement "x") (IntegerLiteral 1))
                                    , VariableDefinition "y" "Int" $ Just (AddStatement (IdentStatement "x") (IntegerLiteral 1))
                                    ]
                                    (IdentStatement "x")
                                )
                            ]
                        ]
                    )

testCase :: Ast -> Spec
testCase ast = do
    describe "Parser" $ do
        it "should ast case" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Object", RightParen, Colon, Type "String", LeftSquirly, Case, Ident "x", Of, Ident "s", Colon, Type "String", Results, String "String", SemiColon, Ident "i", Colon, Type "Int", Results, String "Int", SemiColon, Ident "o", Colon, Type "Object", Results, String "Oops", SemiColon, Esac, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Object"]
                                "String"
                                (CaseStatement
                                    (IdentStatement "x")
                                    [ CaseOfDefinition "s" "String" (StringLiteral "String")
                                    , CaseOfDefinition "i" "Int" (StringLiteral "Int")
                                    , CaseOfDefinition "o" "Object" (StringLiteral "Oops")
                                    ]
                                )
                            ]
                        ]
                    )

testBlock :: Ast -> Spec
testBlock ast = do
    describe "Parser" $ do
        it "should ast block" $ do
                ast [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "String", LeftSquirly, LeftSquirly, Ident "x", Assign, Ident "x", Plus, Integer 1, SemiColon, String "Done!", SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ MethodDefinition
                                "f"
                                [Formal "x" "Int"]
                                "String"
                                (BlockStatement
                                    [ AssignStatement "x" (AddStatement (IdentStatement "x") (IntegerLiteral 1))
                                    , StringLiteral "Done!"
                                    ]
                                )
                            ]
                        ]
                    )

testStringSpecialChars :: Ast -> Spec
testStringSpecialChars ast = do
    describe "Parser" $ do
        it "should ast string with special chars" $ do
                ast  [Class, Type "A", LeftSquirly, Ident "s1", Colon, Type "String", Assign, String "ab\tcd", SemiColon, Ident "s2", Colon, Type "String", Assign, String "ab\ncd", SemiColon, Ident "s3", Colon, Type "String", Assign, String "ab\ncd", SemiColon, Ident "s4", Colon, Type "String", Assign, String "ab\\cd", SemiColon, Ident "s5", Colon, Type "String", Assign, String "abzcd", SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program
                        [ ClassDefinition
                            "A"
                            Nothing
                            [ AttributeDefinition "s1" "String" (Just (StringLiteral "ab\tcd"))
                            , AttributeDefinition "s2" "String" (Just (StringLiteral "ab\ncd"))
                            , AttributeDefinition "s3" "String" (Just (StringLiteral "ab\ncd"))
                            , AttributeDefinition "s4" "String" (Just (StringLiteral "ab\\cd"))
                            , AttributeDefinition "s5" "String" (Just (StringLiteral "abzcd"))
                            ]
                        ]
                    )

testBig :: Ast -> Spec
testBig ast = do
    describe "Parser" $ do
        it "should ast big text" $ do
                ast  [Class, Type "List", Inherits, Type "IO", LeftSquirly, Ident "elem", Colon, Type "Object", SemiColon, Ident "next", Colon, Type "List", SemiColon, Ident "init", LeftParen, Ident "e", Colon, Type "Object", Comma, Ident "n", Colon, Type "List", RightParen, Colon, Type "List", LeftSquirly, LeftSquirly, Ident "elem", Assign, Ident "e", SemiColon, Ident "next", Assign, Ident "n", SemiColon, Ident "self", SemiColon, RightSquirly, RightSquirly, SemiColon, Ident "print", LeftParen, RightParen, Colon, Type "IO", LeftSquirly, Let, Ident "str", Colon, Type "String", Assign, Case, Ident "elem", Of, Ident "s", Colon, Type "String", Results, Ident "s", SemiColon, Ident "n", Colon, Type "Int", Results, New, Type "A2I", Dot, Ident "i2a", LeftParen, Ident "n", RightParen, SemiColon, Ident "o", Colon, Type "Object", Results, LeftSquirly, Ident "abort", LeftParen, RightParen, SemiColon, String "", SemiColon, RightSquirly, SemiColon, Esac, In, LeftSquirly, Ident "out_string", LeftParen, Ident "str", Dot, Ident "concat", LeftParen, String " ", RightParen, RightParen, SemiColon, If, LeftParen, IsVoid, Ident "next", RightParen, Then, Ident "out_string", LeftParen, String "\n", RightParen, Else, Ident "next", Dot, Ident "print", LeftParen, RightParen, Fi, SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type "Main", Inherits, Type "IO", LeftSquirly, Ident "main", LeftParen, RightParen, Colon, Type "Object", LeftSquirly, LeftSquirly, Let, Ident "x", Colon, Type "Int", Assign, Integer 0, Comma, Ident "y", Colon, Type "String", Assign, String "!", Comma, Ident "z", Colon, Type "Int", Assign, Ident "x", Plus, Integer 2, Comma, Ident "empty", Colon, Type "List", Comma, Ident "list", Colon, Type "List", Assign, New, Type "List", Dot, Ident "init", LeftParen, Ident "x", Comma, New, Type "List", Dot, Ident "init", LeftParen, Ident "y", Comma, New, Type "List", Dot, Ident "init", LeftParen, Ident "z", Comma, Ident "empty", RightParen, RightParen, RightParen, In, Ident "list", Dot, Ident "print", LeftParen, RightParen, SemiColon, Let, Ident "n", Colon, Type "Int", Assign, Ident "out_string", LeftParen, String "Calcul\259m factorial pentru: ", RightParen, Dot, Ident "in_int", LeftParen, RightParen, In, LeftSquirly, Ident "out_string", LeftParen, String "Factorial recursiv: ", RightParen, Dot, Ident "out_int", LeftParen, Ident "fact_rec", LeftParen, Ident "n", RightParen, RightParen, Dot, Ident "out_string", LeftParen, String "\n", RightParen, SemiColon, Ident "out_string", LeftParen, String "Factorial iterativ: ", RightParen, Dot, Ident "out_int", LeftParen, Ident "fact_iter", LeftParen, Ident "n", RightParen, RightParen, Dot, Ident "out_string", LeftParen, String "\n", RightParen, SemiColon, RightSquirly, SemiColon, RightSquirly, RightSquirly, SemiColon, Ident "fact_rec", LeftParen, Ident "n", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, If, Ident "n", Equal, Integer 0, Then, Integer 1, Else, Ident "n", Asterisk, Ident "fact_rec", LeftParen, Ident "n", Minus, Integer 1, RightParen, Fi, RightSquirly, SemiColon, Ident "fact_iter", LeftParen, Ident "n", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Let, Ident "res", Colon, Type "Int", Assign, Integer 1, In, LeftSquirly, While, LeftParen, Not, LeftParen, Ident "n", Equal, Integer 0, RightParen, RightParen, Loop, LeftSquirly, Ident "res", Assign, Ident "res", Asterisk, Ident "n", SemiColon, Ident "n", Assign, Ident "n", Minus, Integer 1, SemiColon, RightSquirly, Pool, SemiColon, Ident "res", SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon]
                `shouldBe` Right
                    ( Program [ ClassDefinition "List" (Just "IO") [ AttributeDefinition "elem" "Object" Nothing , AttributeDefinition "next" "List" Nothing , MethodDefinition "init" [Formal "e" "Object",Formal "n" "List"] "List" ( BlockStatement [ AssignStatement "elem" (IdentStatement "e") ,AssignStatement "next" (IdentStatement "n") ,IdentStatement "self" ]) ,MethodDefinition "print" [] "IO" (LetStatement [VariableDefinition "str" "String" (Just (CaseStatement (IdentStatement "elem") [CaseOfDefinition "s" "String" (IdentStatement "s") ,CaseOfDefinition "n" "Int" (MethodCall (Just (NewStatement "A2I")) Nothing "i2a" [IdentStatement "n"]),CaseOfDefinition "o" "Object" (BlockStatement [MethodCall Nothing Nothing "abort" [],StringLiteral ""])]))] (BlockStatement [MethodCall Nothing Nothing "out_string" [MethodCall (Just (IdentStatement "str")) Nothing "concat" [StringLiteral " "]],IfStatement (ParenStatement (IsVoidStatement (IdentStatement "next"))) (MethodCall Nothing Nothing "out_string" [StringLiteral "\n"]) (MethodCall (Just (IdentStatement "next")) Nothing "print" [])]))],ClassDefinition "Main" (Just "IO") [MethodDefinition "main" [] "Object" (BlockStatement [LetStatement [VariableDefinition "x" "Int" (Just (IntegerLiteral 0)),VariableDefinition "y" "String" (Just (StringLiteral "!")),VariableDefinition "z" "Int" (Just (AddStatement (IdentStatement "x") (IntegerLiteral 2))),VariableDefinition "empty" "List" Nothing,VariableDefinition "list" "List" (Just (MethodCall (Just (NewStatement "List")) Nothing "init" [IdentStatement "x",MethodCall (Just (NewStatement "List")) Nothing "init" [IdentStatement "y",MethodCall (Just (NewStatement "List")) Nothing "init" [IdentStatement "z",IdentStatement "empty"]]]))] (MethodCall (Just (IdentStatement "list")) Nothing "print" []),LetStatement [VariableDefinition "n" "Int" (Just (MethodCall (Just (MethodCall Nothing Nothing "out_string" [StringLiteral "Calcul\259m factorial pentru: "])) Nothing "in_int" []))] (BlockStatement [MethodCall (Just (MethodCall (Just (MethodCall Nothing Nothing "out_string" [StringLiteral "Factorial recursiv: "])) Nothing "out_int" [MethodCall Nothing Nothing "fact_rec" [IdentStatement "n"]])) Nothing "out_string" [StringLiteral "\n"],MethodCall (Just (MethodCall (Just (MethodCall Nothing Nothing "out_string" [StringLiteral "Factorial iterativ: "])) Nothing "out_int" [MethodCall Nothing Nothing "fact_iter" [IdentStatement "n"]])) Nothing "out_string" [StringLiteral "\n"]])]),MethodDefinition "fact_rec" [Formal "n" "Int"] "Int" (IfStatement (EqualStatement (IdentStatement "n") (IntegerLiteral 0)) (IntegerLiteral 1) (MulStatement (IdentStatement "n") (MethodCall Nothing Nothing "fact_rec" [SubStatement (IdentStatement "n") (IntegerLiteral 1)]))),MethodDefinition "fact_iter" [Formal "n" "Int"] "Int" (LetStatement [VariableDefinition "res" "Int" (Just (IntegerLiteral 1))] (BlockStatement [WhileStatement (ParenStatement (NotStatement (ParenStatement (EqualStatement (IdentStatement "n") (IntegerLiteral 0))))) (BlockStatement [AssignStatement "res" (MulStatement (IdentStatement "res") (IdentStatement "n")),AssignStatement "n" (SubStatement (IdentStatement "n") (IntegerLiteral 1))]),IdentStatement "res"]))]])
