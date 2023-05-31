{-# LANGUAGE OverloadedStrings #-}

module LexerTest where

import Lexer (Token (..), tokenize)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)

tests :: Spec
tests = do
    testClass
    testAttributeNoInit
    testAttributeInit
    testMethod
    testLiteralId
    testArithmetic
    testRelational
    testAssignment
    testNewIsVoid
    testDispatch
    testIf
    testWhile
    testLet
    testCase
    testBlock

testClass :: Spec
testClass = do
    describe "Lexer" $ do
        it "should tokenize class" $ do
            tokenize (unlines ["class A {};", "", "class B inherits A {};"]) `shouldParse` [Class, Type "A", LeftSquirly, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, RightSquirly, SemiColon]

testAttributeNoInit :: Spec
testAttributeNoInit = do
    describe "Lexer" $ do
        it "should tokenize attribute no init" $ do
            tokenize
                (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int;", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", SemiColon, RightSquirly, SemiColon]

testAttributeInit :: Spec
testAttributeInit = do
    describe "Lexer" $ do
        it "should tokenize attribute init" $ do
            tokenize
                (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int <- 0;", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, RightSquirly, SemiColon]

testMethod :: Spec
testMethod = do
    describe "Lexer" $ do
        it "should tokenize method" $ do
            tokenize
                (unlines ["class A {", "    x : SELF_TYPE;", "", "    f() : Object { 0 };", "};", "", "class B inherits A {", "    y : Int <- 0;", "", "    g(x : Int, y : Bool) : Int { 0 };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, Ident "f", LeftParen, RightParen, Colon, Type "Object", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Bool", RightParen, Colon, Type "Int", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon]

testLiteralId :: Spec
testLiteralId = do
    describe "Lexer" $ do
        it "should tokenize literal" $ do
            tokenize
                (unlines ["class A {", "    n : Int <- 0;", "    s : String <- \"abc\";", "    b : Bool <- false;", "    c : Bool <- b;", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "s", Colon, Type "String", Assign, String "abc", SemiColon, Ident "b", Colon, Type "Bool", Assign, Boolean False, SemiColon, Ident "c", Colon, Type "Bool", Assign, Ident "b", SemiColon, RightSquirly, SemiColon]

testArithmetic :: Spec
testArithmetic = do
    describe "Lexer" $ do
        it "should tokenize arithmetic" $ do
            tokenize
                (unlines ["class A {", "    n : Int <- 1 + 2 * (3 + 4) / (5 - ~6);", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 1, Plus, Integer 2, Asterisk, LeftParen, Integer 3, Plus, Integer 4, RightParen, Slash, LeftParen, Integer 5, Minus, Tilde, Integer 6, RightParen, SemiColon, RightSquirly, SemiColon]

testRelational :: Spec
testRelational = do
    describe "Lexer" $ do
        it "should tokenize relational" $ do
            tokenize
                (unlines ["class A {", "    n : Int;", "    b : Bool <- not 2 <= n;", "    c : Bool <- 2 < n + 1 = false;", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", SemiColon, Ident "b", Colon, Type "Bool", Assign, Not, Integer 2, LessEqual, Ident "n", SemiColon, Ident "c", Colon, Type "Bool", Assign, Integer 2, LessThan, Ident "n", Plus, Integer 1, Equal, Boolean False, SemiColon, RightSquirly, SemiColon]

testAssignment :: Spec
testAssignment = do
    describe "Lexer" $ do
        it "should tokenize assignment" $ do
            tokenize
                (unlines ["class A {", "    x : Int;", "", "    f(y : Int) : Int {", "        x <- y", "    };", "", "    f(y : Int, z : Int) : Int {", "        x <- y <- z", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "Int", SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", RightSquirly, SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", Comma, Ident "z", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", Assign, Ident "z", RightSquirly, SemiColon, RightSquirly, SemiColon]

testNewIsVoid :: Spec
testNewIsVoid = do
    describe "Lexer" $ do
        it "should tokenize new isvoid" $ do
            tokenize
                (unlines ["class A {", "    f() : Bool {", "        not isvoid new A", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, RightParen, Colon, Type "Bool", LeftSquirly, Not, IsVoid, New, Type "A", RightSquirly, SemiColon, RightSquirly, SemiColon]

testDispatch :: Spec
testDispatch = do
    describe "Lexer" $ do
        it "should tokenize dispatch" $ do
            tokenize
                (unlines ["class A {", "    f(x : Int) : Bool {", "        f(x + 1)", "    };", "", "    g(x : Int, y : Int) : Bool {", "        self.g(x + 1, y + 1)", "    };", "", "    h(x : Int) : A {", "        new A@A.h(x + 1).h(x + 2)", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "f", LeftParen, Ident "x", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "self", Dot, Ident "g", LeftParen, Ident "x", Plus, Integer 1, Comma, Ident "y", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "h", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "A", LeftSquirly, New, Type "A", At, Type "A", Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 1, RightParen, Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 2, RightParen, RightSquirly, SemiColon, RightSquirly, SemiColon]

testIf :: Spec
testIf = do
    describe "Lexer" $ do
        it "should tokenize if" $ do
            tokenize
                (unlines ["class A {", "    f(x : Int) : Int {", "        if x <= 5 then x else x + 1 fi", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, If, Ident "x", LessEqual, Integer 5, Then, Ident "x", Else, Ident "x", Plus, Integer 1, Fi, RightSquirly, SemiColon, RightSquirly, SemiColon]

testWhile :: Spec
testWhile = do
    describe "Lexer" $ do
        it "should tokenize while" $ do
            tokenize
                (unlines ["class A {", "    f(x : Int) : Object {", "        while 0 < x loop", "            x <- x - 1", "        pool", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Object", LeftSquirly, While, Integer 0, LessThan, Ident "x", Loop, Ident "x", Assign, Ident "x", Minus, Integer 1, Pool, RightSquirly, SemiColon, RightSquirly, SemiColon]

testLet :: Spec
testLet = do
    describe "Lexer" $ do
        it "should tokenize let" $ do
            tokenize
                (unlines ["class A {", "    f(x : Int) : Int {", "        let x : Int <- x + 1,", "            y : Int <- x + 1", "        in", "            x", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Let, Ident "x", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, Comma, Ident "y", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, In, Ident "x", RightSquirly, SemiColon, RightSquirly, SemiColon]

testCase :: Spec
testCase = do
    describe "Lexer" $ do
        it "should tokenize case" $ do
            tokenize
                (unlines ["class A {", "    f(x : Object) : String {", "        case x of", "            s : String => \"String\";", "            i : Int    => \"Int\";", "            o : Object => \"Oops\";", "        esac", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Object", RightParen, Colon, Type "String", LeftSquirly, Case, Ident "x", Of, Ident "s", Colon, Type "String", Results, String "String", SemiColon, Ident "i", Colon, Type "Int", Results, String "Int", SemiColon, Ident "o", Colon, Type "Object", Results, String "Oops", SemiColon, Esac, RightSquirly, SemiColon, RightSquirly, SemiColon]

testBlock :: Spec
testBlock = do
    describe "Lexer" $ do
        it "should tokenize block" $ do
            tokenize
                (unlines ["class A {", "    f(x : Int) : String {", "        {", "            x <- x + 1;", "            \"Done!\";", "        }", "    };", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "String", LeftSquirly, LeftSquirly, Ident "x", Assign, Ident "x", Plus, Integer 1, SemiColon, String "Done!", SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon]

testStringSpecialChars :: Spec
testStringSpecialChars = do
    describe "Lexer" $ do
        it "should tokenize string with special chars" $ do
            tokenize
                (unlines ["class A {", "    s1 : String <- \"ab\\tcd\";", "    s2 : String <- \"ab\\ncd\";", "    s3 : String <- \"ab\\", "cd\";", "    s4 : String <- \"ab\\\\cd\";", "    s5 : String <- \"ab\\zcd\";", "};"])
                `shouldParse` [Class, Type "A", LeftSquirly, Ident "s1", Colon, Type "String", Assign, String "ab\tcd", SemiColon, Ident "s2", Colon, Type "String", Assign, String "ab\ncd", SemiColon, Ident "s3", Colon, Type "String", Assign, String "ab\\", String "cd", SemiColon, Ident "s4", Colon, Type "String", Assign, String "ab\\cd", SemiColon, Ident "s5", Colon, Type "String", Assign, String "abzcd", SemiColon, RightSquirly, SemiColon]
