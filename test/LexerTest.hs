{-# LANGUAGE OverloadedStrings #-}

module LexerTest where

import Data.Either (fromRight)
import Test.Hspec (Spec, describe, it, shouldBe)
import Token (LexicalError (..), Token (..), TokenInfo (..), Tokenizer)

lexerTests :: Tokenizer -> Spec
lexerTests tokenize = do
    testClass tokenize
    testAttributeNoInit tokenize
    testAttributeInit tokenize
    testMethod tokenize
    testLiteralId tokenize
    testArithmetic tokenize
    testRelational tokenize
    testAssignment tokenize
    testNewIsVoid tokenize
    testDispatch tokenize
    testIf tokenize
    testWhile tokenize
    testLet tokenize
    testCase tokenize
    testBlock tokenize
    testStringSpecialChars tokenize
    testBig tokenize
    testErrorString tokenize
    testErrorComment tokenize
    testInvalidChar tokenize

testClass :: Tokenizer -> Spec
testClass tokenize = do
    describe "Lexer" $ do
        it "should tokenize class" $
            do
                fromRight [] (tokenize (unlines ["class A {};", "", "class B inherits A {};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, RightSquirly, SemiColon, Class, Type (TokenInfo{offset = 19, value = "B"}), Inherits, Type (TokenInfo{offset = 30, value = "A"}), LeftSquirly, RightSquirly, SemiColon, Eof]

testAttributeNoInit :: Tokenizer -> Spec
testAttributeNoInit tokenize = do
    describe "Lexer" $ do
        it "should tokenize attribute no init" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int;", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "x"}), Colon, Type (TokenInfo{offset = 18, value = "SELF_TYPE"}), SemiColon, RightSquirly, SemiColon, Class, Type (TokenInfo{offset = 39, value = "B"}), Inherits, Type (TokenInfo{offset = 50, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 58, value = "y"}), Colon, Type (TokenInfo{offset = 62, value = "Int"}), SemiColon, RightSquirly, SemiColon, Eof]

testAttributeInit :: Tokenizer -> Spec
testAttributeInit tokenize = do
    describe "Lexer" $ do
        it "should tokenize attribute init" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int <- 0;", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "x"}), Colon, Type (TokenInfo{offset = 18, value = "SELF_TYPE"}), SemiColon, RightSquirly, SemiColon, Class, Type (TokenInfo{offset = 39, value = "B"}), Inherits, Type (TokenInfo{offset = 50, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 58, value = "y"}), Colon, Type (TokenInfo{offset = 62, value = "Int"}), Assign, Integer (TokenInfo{offset = 69, value = 0}), SemiColon, RightSquirly, SemiColon, Eof]

testMethod :: Tokenizer -> Spec
testMethod tokenize = do
    describe "Lexer" $ do
        it "should tokenize method" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "", "    f() : Object { 0 };", "};", "", "class B inherits A {", "    y : Int <- 0;", "", "    g(x : Int, y : Bool) : Int { 0 };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "x"}), Colon, Type (TokenInfo{offset = 18, value = "SELF_TYPE"}), SemiColon, Ident (TokenInfo{offset = 34, value = "f"}), LeftParen, RightParen, Colon, Type (TokenInfo{offset = 40, value = "Object"}), LeftSquirly, Integer (TokenInfo{offset = 49, value = 0}), RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type (TokenInfo{offset = 64, value = "B"}), Inherits, Type (TokenInfo{offset = 75, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 83, value = "y"}), Colon, Type (TokenInfo{offset = 87, value = "Int"}), Assign, Integer (TokenInfo{offset = 94, value = 0}), SemiColon, Ident (TokenInfo{offset = 102, value = "g"}), LeftParen, Ident (TokenInfo{offset = 104, value = "x"}), Colon, Type (TokenInfo{offset = 108, value = "Int"}), Comma, Ident (TokenInfo{offset = 113, value = "y"}), Colon, Type (TokenInfo{offset = 117, value = "Bool"}), RightParen, Colon, Type (TokenInfo{offset = 125, value = "Int"}), LeftSquirly, Integer (TokenInfo{offset = 131, value = 0}), RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testLiteralId :: Tokenizer -> Spec
testLiteralId tokenize = do
    describe "Lexer" $ do
        it "should tokenize literal" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    n : Int <- 0;", "    s : String <- \"abc\";", "    b : Bool <- false;", "    c : Bool <- b;", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "n"}), Colon, Type (TokenInfo{offset = 18, value = "Int"}), Assign, Integer (TokenInfo{offset = 25, value = 0}), SemiColon, Ident (TokenInfo{offset = 32, value = "s"}), Colon, Type (TokenInfo{offset = 36, value = "String"}), Assign, String (TokenInfo{offset = 46, value = "abc"}), SemiColon, Ident (TokenInfo{offset = 57, value = "b"}), Colon, Type (TokenInfo{offset = 61, value = "Bool"}), Assign, Boolean (TokenInfo{offset = 69, value = False}), SemiColon, Ident (TokenInfo{offset = 80, value = "c"}), Colon, Type (TokenInfo{offset = 84, value = "Bool"}), Assign, Ident (TokenInfo{offset = 92, value = "b"}), SemiColon, RightSquirly, SemiColon, Eof]

testArithmetic :: Tokenizer -> Spec
testArithmetic tokenize = do
    describe "Lexer" $ do
        it "should tokenize arithmetic" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    n : Int <- 1 + 2 * (3 + 4) / (5 - ~6);", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "n"}), Colon, Type (TokenInfo{offset = 18, value = "Int"}), Assign, Integer (TokenInfo{offset = 25, value = 1}), Plus, Integer (TokenInfo{offset = 29, value = 2}), Asterisk, LeftParen, Integer (TokenInfo{offset = 34, value = 3}), Plus, Integer (TokenInfo{offset = 38, value = 4}), RightParen, Slash, LeftParen, Integer (TokenInfo{offset = 44, value = 5}), Minus, Tilde, Integer (TokenInfo{offset = 49, value = 6}), RightParen, SemiColon, RightSquirly, SemiColon, Eof]

testRelational :: Tokenizer -> Spec
testRelational tokenize = do
    describe "Lexer" $ do
        it "should tokenize relational" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    n : Int;", "    b : Bool <- not 2 <= n;", "    c : Bool <- 2 < n + 1 = false;", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "n"}), Colon, Type (TokenInfo{offset = 18, value = "Int"}), SemiColon, Ident (TokenInfo{offset = 27, value = "b"}), Colon, Type (TokenInfo{offset = 31, value = "Bool"}), Assign, Not, Integer (TokenInfo{offset = 43, value = 2}), LessEqual, Ident (TokenInfo{offset = 48, value = "n"}), SemiColon, Ident (TokenInfo{offset = 55, value = "c"}), Colon, Type (TokenInfo{offset = 59, value = "Bool"}), Assign, Integer (TokenInfo{offset = 67, value = 2}), LessThan, Ident (TokenInfo{offset = 71, value = "n"}), Plus, Integer (TokenInfo{offset = 75, value = 1}), Equal, Boolean (TokenInfo{offset = 79, value = False}), SemiColon, RightSquirly, SemiColon, Eof]

testAssignment :: Tokenizer -> Spec
testAssignment tokenize = do
    describe "Lexer" $ do
        it "should tokenize assignment" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    x : Int;", "", "    f(y : Int) : Int {", "        x <- y", "    };", "", "    f(y : Int, z : Int) : Int {", "        x <- y <- z", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "x"}), Colon, Type (TokenInfo{offset = 18, value = "Int"}), SemiColon, Ident (TokenInfo{offset = 28, value = "f"}), LeftParen, Ident (TokenInfo{offset = 30, value = "y"}), Colon, Type (TokenInfo{offset = 34, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 41, value = "Int"}), LeftSquirly, Ident (TokenInfo{offset = 55, value = "x"}), Assign, Ident (TokenInfo{offset = 60, value = "y"}), RightSquirly, SemiColon, Ident (TokenInfo{offset = 74, value = "f"}), LeftParen, Ident (TokenInfo{offset = 76, value = "y"}), Colon, Type (TokenInfo{offset = 80, value = "Int"}), Comma, Ident (TokenInfo{offset = 85, value = "z"}), Colon, Type (TokenInfo{offset = 89, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 96, value = "Int"}), LeftSquirly, Ident (TokenInfo{offset = 110, value = "x"}), Assign, Ident (TokenInfo{offset = 115, value = "y"}), Assign, Ident (TokenInfo{offset = 120, value = "z"}), RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testNewIsVoid :: Tokenizer -> Spec
testNewIsVoid tokenize = do
    describe "Lexer" $ do
        it "should tokenize new isvoid" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f() : Bool {", "        not isvoid new A", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, RightParen, Colon, Type (TokenInfo{offset = 20, value = "Bool"}), LeftSquirly, Not, IsVoid, New, Type (TokenInfo{offset = 50, value = "A"}), RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testDispatch :: Tokenizer -> Spec
testDispatch tokenize = do
    describe "Lexer" $ do
        it "should tokenize dispatch" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Int) : Bool {", "        f(x + 1)", "    };", "", "    g(x : Int, y : Int) : Bool {", "        self.g(x + 1, y + 1)", "    };", "", "    h(x : Int) : A {", "        new A@A.h(x + 1).h(x + 2)", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 27, value = "Bool"}), LeftSquirly, Ident (TokenInfo{offset = 42, value = "f"}), LeftParen, Ident (TokenInfo{offset = 44, value = "x"}), Plus, Integer (TokenInfo{offset = 48, value = 1}), RightParen, RightSquirly, SemiColon, Ident (TokenInfo{offset = 63, value = "g"}), LeftParen, Ident (TokenInfo{offset = 65, value = "x"}), Colon, Type (TokenInfo{offset = 69, value = "Int"}), Comma, Ident (TokenInfo{offset = 74, value = "y"}), Colon, Type (TokenInfo{offset = 78, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 85, value = "Bool"}), LeftSquirly, Ident (TokenInfo{offset = 100, value = "self"}), Dot, Ident (TokenInfo{offset = 105, value = "g"}), LeftParen, Ident (TokenInfo{offset = 107, value = "x"}), Plus, Integer (TokenInfo{offset = 111, value = 1}), Comma, Ident (TokenInfo{offset = 114, value = "y"}), Plus, Integer (TokenInfo{offset = 118, value = 1}), RightParen, RightSquirly, SemiColon, Ident (TokenInfo{offset = 133, value = "h"}), LeftParen, Ident (TokenInfo{offset = 135, value = "x"}), Colon, Type (TokenInfo{offset = 139, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 146, value = "A"}), LeftSquirly, New, Type (TokenInfo{offset = 162, value = "A"}), At, Type (TokenInfo{offset = 164, value = "A"}), Dot, Ident (TokenInfo{offset = 166, value = "h"}), LeftParen, Ident (TokenInfo{offset = 168, value = "x"}), Plus, Integer (TokenInfo{offset = 172, value = 1}), RightParen, Dot, Ident (TokenInfo{offset = 175, value = "h"}), LeftParen, Ident (TokenInfo{offset = 177, value = "x"}), Plus, Integer (TokenInfo{offset = 181, value = 2}), RightParen, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testIf :: Tokenizer -> Spec
testIf tokenize = do
    describe "Lexer" $ do
        it "should tokenize if" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Int) : Int {", "        if x <= 5 then x else x + 1 fi", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 27, value = "Int"}), LeftSquirly, If, Ident (TokenInfo{offset = 44, value = "x"}), LessEqual, Integer (TokenInfo{offset = 49, value = 5}), Then, Ident (TokenInfo{offset = 56, value = "x"}), Else, Ident (TokenInfo{offset = 63, value = "x"}), Plus, Integer (TokenInfo{offset = 67, value = 1}), Fi, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testWhile :: Tokenizer -> Spec
testWhile tokenize = do
    describe "Lexer" $ do
        it "should tokenize while" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Int) : Object {", "        while 0 < x loop", "            x <- x - 1", "        pool", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 27, value = "Object"}), LeftSquirly, While, Integer (TokenInfo{offset = 50, value = 0}), LessThan, Ident (TokenInfo{offset = 54, value = "x"}), Loop, Ident (TokenInfo{offset = 73, value = "x"}), Assign, Ident (TokenInfo{offset = 78, value = "x"}), Minus, Integer (TokenInfo{offset = 82, value = 1}), Pool, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testLet :: Tokenizer -> Spec
testLet tokenize = do
    describe "Lexer" $ do
        it "should tokenize let" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Int) : Int {", "        let x : Int <- x + 1,", "            y : Int <- x + 1", "        in", "            x", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 27, value = "Int"}), LeftSquirly, Let, Ident (TokenInfo{offset = 45, value = "x"}), Colon, Type (TokenInfo{offset = 49, value = "Int"}), Assign, Ident (TokenInfo{offset = 56, value = "x"}), Plus, Integer (TokenInfo{offset = 60, value = 1}), Comma, Ident (TokenInfo{offset = 75, value = "y"}), Colon, Type (TokenInfo{offset = 79, value = "Int"}), Assign, Ident (TokenInfo{offset = 86, value = "x"}), Plus, Integer (TokenInfo{offset = 90, value = 1}), In, Ident (TokenInfo{offset = 115, value = "x"}), RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testCase :: Tokenizer -> Spec
testCase tokenize = do
    describe "Lexer" $ do
        it "should tokenize case" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Object) : String {", "        case x of", "            s : String => \"String\";", "            i : Int    => \"Int\";", "            o : Object => \"Oops\";", "        esac", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Object"}), RightParen, Colon, Type (TokenInfo{offset = 30, value = "String"}), LeftSquirly, Case, Ident (TokenInfo{offset = 52, value = "x"}), Of, Ident (TokenInfo{offset = 69, value = "s"}), Colon, Type (TokenInfo{offset = 73, value = "String"}), Results, String (TokenInfo{offset = 83, value = "String"}), SemiColon, Ident (TokenInfo{offset = 105, value = "i"}), Colon, Type (TokenInfo{offset = 109, value = "Int"}), Results, String (TokenInfo{offset = 119, value = "Int"}), SemiColon, Ident (TokenInfo{offset = 138, value = "o"}), Colon, Type (TokenInfo{offset = 142, value = "Object"}), Results, String (TokenInfo{offset = 152, value = "Oops"}), SemiColon, Esac, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testBlock :: Tokenizer -> Spec
testBlock tokenize = do
    describe "Lexer" $ do
        it "should tokenize block" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    f(x : Int) : String {", "        {", "            x <- x + 1;", "            \"Done!\";", "        }", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "f"}), LeftParen, Ident (TokenInfo{offset = 16, value = "x"}), Colon, Type (TokenInfo{offset = 20, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 27, value = "String"}), LeftSquirly, LeftSquirly, Ident (TokenInfo{offset = 58, value = "x"}), Assign, Ident (TokenInfo{offset = 63, value = "x"}), Plus, Integer (TokenInfo{offset = 67, value = 1}), SemiColon, String (TokenInfo{offset = 82, value = "Done!"}), SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testStringSpecialChars :: Tokenizer -> Spec
testStringSpecialChars tokenize = do
    describe "Lexer" $ do
        it "should tokenize string with special chars" $
            do
                fromRight [] (tokenize (unlines ["class A {", "    s1 : String <- \"ab\\tcd\";", "    s2 : String <- \"ab\\ncd\";", "    s3 : String <- \"ab\\", "cd\";", "    s4 : String <- \"ab\\\\cd\";", "    s5 : String <- \"ab\\zcd\";", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 6, value = "A"}), LeftSquirly, Ident (TokenInfo{offset = 14, value = "s1"}), Colon, Type (TokenInfo{offset = 19, value = "String"}), Assign, String (TokenInfo{offset = 29, value = "ab\tcd"}), SemiColon, Ident (TokenInfo{offset = 43, value = "s2"}), Colon, Type (TokenInfo{offset = 48, value = "String"}), Assign, String (TokenInfo{offset = 58, value = "ab\ncd"}), SemiColon, Ident (TokenInfo{offset = 72, value = "s3"}), Colon, Type (TokenInfo{offset = 77, value = "String"}), Assign, String (TokenInfo{offset = 87, value = "ab\ncd"}), SemiColon, Ident (TokenInfo{offset = 101, value = "s4"}), Colon, Type (TokenInfo{offset = 106, value = "String"}), Assign, String (TokenInfo{offset = 116, value = "ab\\cd"}), SemiColon, Ident (TokenInfo{offset = 130, value = "s5"}), Colon, Type (TokenInfo{offset = 135, value = "String"}), Assign, String (TokenInfo{offset = 145, value = "abzcd"}), SemiColon, RightSquirly, SemiColon, Eof]

testBig :: Tokenizer -> Spec
testBig tokenize = do
    describe "Lexer" $ do
        it "should tokenize big text" $
            do
                fromRight [] (tokenize (unlines ["(*", "    Listă nevidă eterogenă, cu elemente având tipul static Object și tipuri", "    dinamice amestecate. Sfârșitul liste este semnalat de next = void.", "", "    Adaptare după Alex Aiken.", "*)", "class List inherits IO {", "    elem : Object;", "    next : List;", "", "    init(e : Object, n : List) : List {", "        {", "            elem <- e;", "            next <- n;", "            self;", "        }", "    };", "", "    print() : IO {", "        let str : String <-", "                -- case permite ramificarea execuției în funcție de tipul", "                -- dinamic. abort() oprește execuția. Șirul vid care îi urmează", "                -- este necesar pentru verificarea statică a tipurilor.", "                case elem of", "                    s : String => s;", "                    n : Int => new A2I.i2a(n);  -- A2I este definită mai jos", "                    o : Object => { abort(); \"\"; };", "                esac", "        in", "            {", "                out_string(str.concat(\" \"));", "                if (isvoid next) then out_string(\"\\n\") else next.print() fi;", "            }", "    };", "};", "", "class Main inherits IO {", "    main() : Object {", "        {", "            let x : Int <- 0,", "                y : String <- \"!\",", "                z : Int <- x + 2,", "                empty : List,  -- inițializată implicit la void", "                list : List <-", "                    new List.init(x,", "                        new List.init(y,", "                            new List.init(z, empty)))", "            in", "                list.print();", "", "            -- out_string întoarce IO, și putem înlănțui mai multe operații.", "            let n : Int <- out_string(\"Calculăm factorial pentru: \").in_int()", "            in", "                {", "                    out_string(\"Factorial recursiv: \").out_int(fact_rec(n))", "                        .out_string(\"\\n\");", "                    out_string(\"Factorial iterativ: \").out_int(fact_iter(n))", "                        .out_string(\"\\n\");", "                };", "        }", "    };", "", "    -- factorial implementat recursiv", "    fact_rec(n : Int) : Int {", "        if n = 0 then 1 else n * fact_rec(n - 1) fi", "    };", "", "    -- factorial implementat iterativ", "    fact_iter(n : Int) : Int {", "        let res : Int <- 1", "        -- Blocurile {} sunt văzute ca expresii. Valoarea ultimei expresii", "        -- din bloc este valoarea blocului. ", "        in", "            {", "                while (not (n = 0)) loop", "                    {", "                        res <- res * n;", "                        n <- n - 1;", "                    }", "                pool;", "                res;", "            }", "    };", "};"]))
                `shouldBe` [Class, Type (TokenInfo{offset = 190, value = "List"}), Inherits, Type (TokenInfo{offset = 204, value = "IO"}), LeftSquirly, Ident (TokenInfo{offset = 213, value = "elem"}), Colon, Type (TokenInfo{offset = 220, value = "Object"}), SemiColon, Ident (TokenInfo{offset = 232, value = "next"}), Colon, Type (TokenInfo{offset = 239, value = "List"}), SemiColon, Ident (TokenInfo{offset = 250, value = "init"}), LeftParen, Ident (TokenInfo{offset = 255, value = "e"}), Colon, Type (TokenInfo{offset = 259, value = "Object"}), Comma, Ident (TokenInfo{offset = 267, value = "n"}), Colon, Type (TokenInfo{offset = 271, value = "List"}), RightParen, Colon, Type (TokenInfo{offset = 279, value = "List"}), LeftSquirly, LeftSquirly, Ident (TokenInfo{offset = 308, value = "elem"}), Assign, Ident (TokenInfo{offset = 316, value = "e"}), SemiColon, Ident (TokenInfo{offset = 331, value = "next"}), Assign, Ident (TokenInfo{offset = 339, value = "n"}), SemiColon, Ident (TokenInfo{offset = 354, value = "self"}), SemiColon, RightSquirly, RightSquirly, SemiColon, Ident (TokenInfo{offset = 382, value = "print"}), LeftParen, RightParen, Colon, Type (TokenInfo{offset = 392, value = "IO"}), LeftSquirly, Let, Ident (TokenInfo{offset = 409, value = "str"}), Colon, Type (TokenInfo{offset = 415, value = "String"}), Assign, Case, Ident (TokenInfo{offset = 672, value = "elem"}), Of, Ident (TokenInfo{offset = 700, value = "s"}), Colon, Type (TokenInfo{offset = 704, value = "String"}), Results, Ident (TokenInfo{offset = 714, value = "s"}), SemiColon, Ident (TokenInfo{offset = 737, value = "n"}), Colon, Type (TokenInfo{offset = 741, value = "Int"}), Results, New, Type (TokenInfo{offset = 752, value = "A2I"}), Dot, Ident (TokenInfo{offset = 756, value = "i2a"}), LeftParen, Ident (TokenInfo{offset = 760, value = "n"}), RightParen, SemiColon, Ident (TokenInfo{offset = 814, value = "o"}), Colon, Type (TokenInfo{offset = 818, value = "Object"}), Results, LeftSquirly, Ident (TokenInfo{offset = 830, value = "abort"}), LeftParen, RightParen, SemiColon, String (TokenInfo{offset = 839, value = ""}), SemiColon, RightSquirly, SemiColon, Esac, In, LeftSquirly, Ident (TokenInfo{offset = 908, value = "out_string"}), LeftParen, Ident (TokenInfo{offset = 919, value = "str"}), Dot, Ident (TokenInfo{offset = 923, value = "concat"}), LeftParen, String (TokenInfo{offset = 930, value = " "}), RightParen, RightParen, SemiColon, If, LeftParen, IsVoid, Ident (TokenInfo{offset = 964, value = "next"}), RightParen, Then, Ident (TokenInfo{offset = 975, value = "out_string"}), LeftParen, String (TokenInfo{offset = 986, value = "\n"}), RightParen, Else, Ident (TokenInfo{offset = 997, value = "next"}), Dot, Ident (TokenInfo{offset = 1002, value = "print"}), LeftParen, RightParen, Fi, SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type (TokenInfo{offset = 1045, value = "Main"}), Inherits, Type (TokenInfo{offset = 1059, value = "IO"}), LeftSquirly, Ident (TokenInfo{offset = 1068, value = "main"}), LeftParen, RightParen, Colon, Type (TokenInfo{offset = 1077, value = "Object"}), LeftSquirly, LeftSquirly, Let, Ident (TokenInfo{offset = 1112, value = "x"}), Colon, Type (TokenInfo{offset = 1116, value = "Int"}), Assign, Integer (TokenInfo{offset = 1123, value = 0}), Comma, Ident (TokenInfo{offset = 1142, value = "y"}), Colon, Type (TokenInfo{offset = 1146, value = "String"}), Assign, String (TokenInfo{offset = 1156, value = "!"}), Comma, Ident (TokenInfo{offset = 1177, value = "z"}), Colon, Type (TokenInfo{offset = 1181, value = "Int"}), Assign, Ident (TokenInfo{offset = 1188, value = "x"}), Plus, Integer (TokenInfo{offset = 1192, value = 2}), Comma, Ident (TokenInfo{offset = 1211, value = "empty"}), Colon, Type (TokenInfo{offset = 1219, value = "List"}), Comma, Ident (TokenInfo{offset = 1275, value = "list"}), Colon, Type (TokenInfo{offset = 1282, value = "List"}), Assign, New, Type (TokenInfo{offset = 1314, value = "List"}), Dot, Ident (TokenInfo{offset = 1319, value = "init"}), LeftParen, Ident (TokenInfo{offset = 1324, value = "x"}), Comma, New, Type (TokenInfo{offset = 1355, value = "List"}), Dot, Ident (TokenInfo{offset = 1360, value = "init"}), LeftParen, Ident (TokenInfo{offset = 1365, value = "y"}), Comma, New, Type (TokenInfo{offset = 1400, value = "List"}), Dot, Ident (TokenInfo{offset = 1405, value = "init"}), LeftParen, Ident (TokenInfo{offset = 1410, value = "z"}), Comma, Ident (TokenInfo{offset = 1413, value = "empty"}), RightParen, RightParen, RightParen, In, Ident (TokenInfo{offset = 1453, value = "list"}), Dot, Ident (TokenInfo{offset = 1458, value = "print"}), LeftParen, RightParen, SemiColon, Let, Ident (TokenInfo{offset = 1561, value = "n"}), Colon, Type (TokenInfo{offset = 1565, value = "Int"}), Assign, Ident (TokenInfo{offset = 1572, value = "out_string"}), LeftParen, String (TokenInfo{offset = 1583, value = "Calcul\259m factorial pentru: "}), RightParen, Dot, Ident (TokenInfo{offset = 1614, value = "in_int"}), LeftParen, RightParen, In, LeftSquirly, Ident (TokenInfo{offset = 1676, value = "out_string"}), LeftParen, String (TokenInfo{offset = 1687, value = "Factorial recursiv: "}), RightParen, Dot, Ident (TokenInfo{offset = 1711, value = "out_int"}), LeftParen, Ident (TokenInfo{offset = 1719, value = "fact_rec"}), LeftParen, Ident (TokenInfo{offset = 1728, value = "n"}), RightParen, RightParen, Dot, Ident (TokenInfo{offset = 1757, value = "out_string"}), LeftParen, String (TokenInfo{offset = 1768, value = "\n"}), RightParen, SemiColon, Ident (TokenInfo{offset = 1795, value = "out_string"}), LeftParen, String (TokenInfo{offset = 1806, value = "Factorial iterativ: "}), RightParen, Dot, Ident (TokenInfo{offset = 1830, value = "out_int"}), LeftParen, Ident (TokenInfo{offset = 1838, value = "fact_iter"}), LeftParen, Ident (TokenInfo{offset = 1848, value = "n"}), RightParen, RightParen, Dot, Ident (TokenInfo{offset = 1877, value = "out_string"}), LeftParen, String (TokenInfo{offset = 1888, value = "\n"}), RightParen, SemiColon, RightSquirly, SemiColon, RightSquirly, RightSquirly, SemiColon, Ident (TokenInfo{offset = 1974, value = "fact_rec"}), LeftParen, Ident (TokenInfo{offset = 1983, value = "n"}), Colon, Type (TokenInfo{offset = 1987, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 1994, value = "Int"}), LeftSquirly, If, Ident (TokenInfo{offset = 2011, value = "n"}), Equal, Integer (TokenInfo{offset = 2015, value = 0}), Then, Integer (TokenInfo{offset = 2022, value = 1}), Else, Ident (TokenInfo{offset = 2029, value = "n"}), Asterisk, Ident (TokenInfo{offset = 2033, value = "fact_rec"}), LeftParen, Ident (TokenInfo{offset = 2042, value = "n"}), Minus, Integer (TokenInfo{offset = 2046, value = 1}), RightParen, Fi, RightSquirly, SemiColon, Ident (TokenInfo{offset = 2102, value = "fact_iter"}), LeftParen, Ident (TokenInfo{offset = 2112, value = "n"}), Colon, Type (TokenInfo{offset = 2116, value = "Int"}), RightParen, Colon, Type (TokenInfo{offset = 2123, value = "Int"}), LeftSquirly, Let, Ident (TokenInfo{offset = 2141, value = "res"}), Colon, Type (TokenInfo{offset = 2147, value = "Int"}), Assign, Integer (TokenInfo{offset = 2154, value = 1}), In, LeftSquirly, While, LeftParen, Not, LeftParen, Ident (TokenInfo{offset = 2329, value = "n"}), Equal, Integer (TokenInfo{offset = 2333, value = 0}), RightParen, RightParen, Loop, LeftSquirly, Ident (TokenInfo{offset = 2388, value = "res"}), Assign, Ident (TokenInfo{offset = 2395, value = "res"}), Asterisk, Ident (TokenInfo{offset = 2401, value = "n"}), SemiColon, Ident (TokenInfo{offset = 2428, value = "n"}), Assign, Ident (TokenInfo{offset = 2433, value = "n"}), Minus, Integer (TokenInfo{offset = 2437, value = 1}), SemiColon, RightSquirly, Pool, SemiColon, Ident (TokenInfo{offset = 2500, value = "res"}), SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon, Eof]

testErrorString :: Tokenizer -> Spec
testErrorString tokenize = do
    describe "Lexer" $ do
        it "should handle error string" $
            do
                tokenize (unlines ["class A {", "    long : String <- \"lRLRZEEyGcL1vXxQIke5GzPdnalgabCDWDkohxom3vy2B3RfieCvgfqjMEkP9nxcQE0CdSFpZnlohvhDwr7aHRBlXbovc7YpPEKCLUxprxMsUIP4oerysNX6NNvv4OznWgi90lQPTqH1T3flaLUEdOo96RVsikngIJklXvPItGHYza6vkdUWtbahqi8Pewfq3uV1dKfEAnyuHc5XGqFEsAmEfbcYOLr4ADNpOqxWdRwISb14OWzZUA7MHLqtcv4zpkqN6sf4JaTtIMsu7tfYe6UfVkGjD2nQdE8kpq5OmpfmD0Znwh2Pab4EfAffN9LabBbRqEFlTNkhHpKKOPA7MenY1761UGsq4ia0wsKnDIFnCumudFUFjigEiTob96sjn66eDVK0ETzwGR4acif2T8QsFwCv8zNQDdPABbVzUI5qtiYZR2i2f3eVgyzU6N3NCHg6dwDDHfTN8hlaGdRZdTM7kHHYXvNGiNtvagJwTLml2arYYGnkn02ci4vm6BkRdiog6OCbTr3Ysqvbee5CjsWXjflSuiMyNiw1ZNvWJtmU0bObYAj5YTPwVPH5IjejXN8c3eTRfMpOYhTkTpY9nQoi2S5FRZKTFPK99X4l5AqaR7Fx3IfC7WOn5tJnhkbmFFoUnyPlrXGltNTodBYTwNCgWTTdaRIV7ulg6ttxTYd5VJQyVBk9kJei5NIjuIrwQ4xovu0ODIDVMZNIfO1KiaN81XwHx4JPchHBkGMIn5cHsfLMdXdV8AfzLV1lPveSg1MuBZEvWUG9prYFLcQIJcWqDfnZgHw1Yx4Iy5rA4kGi9EaPEMpJxGARpRSm2TawQfRnenZw8FpMYDCUri2RTB20fSfkGHDzET8EBFSU7lbAiBOluWSSyHkr3eU6JJuld83Nbp5iNFQV7VNL4MOReVJkHJPvoEbsQdxYAJQG3EBC8uNf6YXVCbDRBdEXXGcOXNLtBTS7t07SKc6iScxOTduV5K9v38vs3YmqpOFVObZ5TfMnoUngX69IzCMMsL9Ad\";", "    null : String <- \"a\0b\";", "    unterminated : String <- \"where to?", "    ;"] ++ "    eof : String <- \"nirvana")
                `shouldBe` Left [TokenInfo{offset = 31, value = StringConstantTooLong}, TokenInfo{offset = 1081, value = StringContainsNull}, TokenInfo{offset = 1117, value = StringUnterminated}, TokenInfo{offset = 1154, value = EofInString}]

testErrorComment :: Tokenizer -> Spec
testErrorComment tokenize = do
    describe "Lexer" $ do
        it "should handle error comment" $
            do
                tokenize (unlines ["class A {", "    *)", "", "    x : Int;", "", "    (* some (* comment *)", "};"])
                `shouldBe` Left [TokenInfo{offset = 14, value = UnmatchedComment}, TokenInfo{offset = 36, value = EofInComment}]

testInvalidChar :: Tokenizer -> Spec
testInvalidChar tokenize = do
    describe "Lexer" $ do
        it "should handle error invalid char" $
            do
                tokenize (unlines ["class A {", "    x : Int; #", "};"])
                `shouldBe` Left [TokenInfo{offset = 23, value = InvalidChar '#'}]
