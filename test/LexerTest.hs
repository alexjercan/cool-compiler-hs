{-# LANGUAGE OverloadedStrings #-}

module LexerTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Token (Token (..), TokenInfo (..), Tokenizer, Error (..))

tokens :: [TokenInfo] -> [Token]
tokens = map token

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
        it "should tokenize class" $ do
            tokens (tokenize (unlines ["class A {};", "", "class B inherits A {};"])) `shouldBe` [Class, Type "A", LeftSquirly, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, RightSquirly, SemiColon]

testAttributeNoInit :: Tokenizer -> Spec
testAttributeNoInit tokenize = do
    describe "Lexer" $ do
        it "should tokenize attribute no init" $
            do
                tokens (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int;", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", SemiColon, RightSquirly, SemiColon]

testAttributeInit :: Tokenizer -> Spec
testAttributeInit tokenize = do
    describe "Lexer" $ do
        it "should tokenize attribute init" $ do
            tokens (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "};", "", "class B inherits A {", "    y : Int <- 0;", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, RightSquirly, SemiColon]

testMethod :: Tokenizer -> Spec
testMethod tokenize = do
    describe "Lexer" $ do
        it "should tokenize method" $ do
            tokens (tokenize (unlines ["class A {", "    x : SELF_TYPE;", "", "    f() : Object { 0 };", "};", "", "class B inherits A {", "    y : Int <- 0;", "", "    g(x : Int, y : Bool) : Int { 0 };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "SELF_TYPE", SemiColon, Ident "f", LeftParen, RightParen, Colon, Type "Object", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type "B", Inherits, Type "A", LeftSquirly, Ident "y", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Bool", RightParen, Colon, Type "Int", LeftSquirly, Integer 0, RightSquirly, SemiColon, RightSquirly, SemiColon]

testLiteralId :: Tokenizer -> Spec
testLiteralId tokenize = do
    describe "Lexer" $ do
        it "should tokenize literal" $ do
            tokens (tokenize (unlines ["class A {", "    n : Int <- 0;", "    s : String <- \"abc\";", "    b : Bool <- false;", "    c : Bool <- b;", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 0, SemiColon, Ident "s", Colon, Type "String", Assign, String "abc", SemiColon, Ident "b", Colon, Type "Bool", Assign, Boolean False, SemiColon, Ident "c", Colon, Type "Bool", Assign, Ident "b", SemiColon, RightSquirly, SemiColon]

testArithmetic :: Tokenizer -> Spec
testArithmetic tokenize = do
    describe "Lexer" $ do
        it "should tokenize arithmetic" $ do
            tokens (tokenize (unlines ["class A {", "    n : Int <- 1 + 2 * (3 + 4) / (5 - ~6);", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", Assign, Integer 1, Plus, Integer 2, Asterisk, LeftParen, Integer 3, Plus, Integer 4, RightParen, Slash, LeftParen, Integer 5, Minus, Tilde, Integer 6, RightParen, SemiColon, RightSquirly, SemiColon]

testRelational :: Tokenizer -> Spec
testRelational tokenize = do
    describe "Lexer" $ do
        it "should tokenize relational" $ do
            tokens (tokenize (unlines ["class A {", "    n : Int;", "    b : Bool <- not 2 <= n;", "    c : Bool <- 2 < n + 1 = false;", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "n", Colon, Type "Int", SemiColon, Ident "b", Colon, Type "Bool", Assign, Not, Integer 2, LessEqual, Ident "n", SemiColon, Ident "c", Colon, Type "Bool", Assign, Integer 2, LessThan, Ident "n", Plus, Integer 1, Equal, Boolean False, SemiColon, RightSquirly, SemiColon]

testAssignment :: Tokenizer -> Spec
testAssignment tokenize = do
    describe "Lexer" $ do
        it "should tokenize assignment" $ do
            tokens (tokenize (unlines ["class A {", "    x : Int;", "", "    f(y : Int) : Int {", "        x <- y", "    };", "", "    f(y : Int, z : Int) : Int {", "        x <- y <- z", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "Int", SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", RightSquirly, SemiColon, Ident "f", LeftParen, Ident "y", Colon, Type "Int", Comma, Ident "z", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Ident "x", Assign, Ident "y", Assign, Ident "z", RightSquirly, SemiColon, RightSquirly, SemiColon]

testNewIsVoid :: Tokenizer -> Spec
testNewIsVoid tokenize = do
    describe "Lexer" $ do
        it "should tokenize new isvoid" $ do
            tokens (tokenize (unlines ["class A {", "    f() : Bool {", "        not isvoid new A", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, RightParen, Colon, Type "Bool", LeftSquirly, Not, IsVoid, New, Type "A", RightSquirly, SemiColon, RightSquirly, SemiColon]

testDispatch :: Tokenizer -> Spec
testDispatch tokenize = do
    describe "Lexer" $ do
        it "should tokenize dispatch" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Int) : Bool {", "        f(x + 1)", "    };", "", "    g(x : Int, y : Int) : Bool {", "        self.g(x + 1, y + 1)", "    };", "", "    h(x : Int) : A {", "        new A@A.h(x + 1).h(x + 2)", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "f", LeftParen, Ident "x", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "g", LeftParen, Ident "x", Colon, Type "Int", Comma, Ident "y", Colon, Type "Int", RightParen, Colon, Type "Bool", LeftSquirly, Ident "self", Dot, Ident "g", LeftParen, Ident "x", Plus, Integer 1, Comma, Ident "y", Plus, Integer 1, RightParen, RightSquirly, SemiColon, Ident "h", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "A", LeftSquirly, New, Type "A", At, Type "A", Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 1, RightParen, Dot, Ident "h", LeftParen, Ident "x", Plus, Integer 2, RightParen, RightSquirly, SemiColon, RightSquirly, SemiColon]

testIf :: Tokenizer -> Spec
testIf tokenize = do
    describe "Lexer" $ do
        it "should tokenize if" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Int) : Int {", "        if x <= 5 then x else x + 1 fi", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, If, Ident "x", LessEqual, Integer 5, Then, Ident "x", Else, Ident "x", Plus, Integer 1, Fi, RightSquirly, SemiColon, RightSquirly, SemiColon]

testWhile :: Tokenizer -> Spec
testWhile tokenize = do
    describe "Lexer" $ do
        it "should tokenize while" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Int) : Object {", "        while 0 < x loop", "            x <- x - 1", "        pool", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Object", LeftSquirly, While, Integer 0, LessThan, Ident "x", Loop, Ident "x", Assign, Ident "x", Minus, Integer 1, Pool, RightSquirly, SemiColon, RightSquirly, SemiColon]

testLet :: Tokenizer -> Spec
testLet tokenize = do
    describe "Lexer" $ do
        it "should tokenize let" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Int) : Int {", "        let x : Int <- x + 1,", "            y : Int <- x + 1", "        in", "            x", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Let, Ident "x", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, Comma, Ident "y", Colon, Type "Int", Assign, Ident "x", Plus, Integer 1, In, Ident "x", RightSquirly, SemiColon, RightSquirly, SemiColon]

testCase :: Tokenizer -> Spec
testCase tokenize = do
    describe "Lexer" $ do
        it "should tokenize case" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Object) : String {", "        case x of", "            s : String => \"String\";", "            i : Int    => \"Int\";", "            o : Object => \"Oops\";", "        esac", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Object", RightParen, Colon, Type "String", LeftSquirly, Case, Ident "x", Of, Ident "s", Colon, Type "String", Results, String "String", SemiColon, Ident "i", Colon, Type "Int", Results, String "Int", SemiColon, Ident "o", Colon, Type "Object", Results, String "Oops", SemiColon, Esac, RightSquirly, SemiColon, RightSquirly, SemiColon]

testBlock :: Tokenizer -> Spec
testBlock tokenize = do
    describe "Lexer" $ do
        it "should tokenize block" $ do
            tokens (tokenize (unlines ["class A {", "    f(x : Int) : String {", "        {", "            x <- x + 1;", "            \"Done!\";", "        }", "    };", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "f", LeftParen, Ident "x", Colon, Type "Int", RightParen, Colon, Type "String", LeftSquirly, LeftSquirly, Ident "x", Assign, Ident "x", Plus, Integer 1, SemiColon, String "Done!", SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon]

testStringSpecialChars :: Tokenizer -> Spec
testStringSpecialChars tokenize = do
    describe "Lexer" $ do
        it "should tokenize string with special chars" $ do
            tokens (tokenize (unlines ["class A {", "    s1 : String <- \"ab\\tcd\";", "    s2 : String <- \"ab\\ncd\";", "    s3 : String <- \"ab\\", "cd\";", "    s4 : String <- \"ab\\\\cd\";", "    s5 : String <- \"ab\\zcd\";", "};"]))
                `shouldBe` [Class, Type "A", LeftSquirly, Ident "s1", Colon, Type "String", Assign, String "ab\tcd", SemiColon, Ident "s2", Colon, Type "String", Assign, String "ab\ncd", SemiColon, Ident "s3", Colon, Type "String", Assign, String "ab\ncd", SemiColon, Ident "s4", Colon, Type "String", Assign, String "ab\\cd", SemiColon, Ident "s5", Colon, Type "String", Assign, String "abzcd", SemiColon, RightSquirly, SemiColon]

testBig :: Tokenizer -> Spec
testBig tokenize = do
    describe "Lexer" $ do
        it "should tokenize big text" $ do
            tokens (tokenize (unlines ["(*", "    Listă nevidă eterogenă, cu elemente având tipul static Object și tipuri", "    dinamice amestecate. Sfârșitul liste este semnalat de next = void.", "", "    Adaptare după Alex Aiken.", "*)", "class List inherits IO {", "    elem : Object;", "    next : List;", "", "    init(e : Object, n : List) : List {", "        {", "            elem <- e;", "            next <- n;", "            self;", "        }", "    };", "", "    print() : IO {", "        let str : String <-", "                -- case permite ramificarea execuției în funcție de tipul", "                -- dinamic. abort() oprește execuția. Șirul vid care îi urmează", "                -- este necesar pentru verificarea statică a tipurilor.", "                case elem of", "                    s : String => s;", "                    n : Int => new A2I.i2a(n);  -- A2I este definită mai jos", "                    o : Object => { abort(); \"\"; };", "                esac", "        in", "            {", "                out_string(str.concat(\" \"));", "                if (isvoid next) then out_string(\"\\n\") else next.print() fi;", "            }", "    };", "};", "", "class Main inherits IO {", "    main() : Object {", "        {", "            let x : Int <- 0,", "                y : String <- \"!\",", "                z : Int <- x + 2,", "                empty : List,  -- inițializată implicit la void", "                list : List <-", "                    new List.init(x,", "                        new List.init(y,", "                            new List.init(z, empty)))", "            in", "                list.print();", "", "            -- out_string întoarce IO, și putem înlănțui mai multe operații.", "            let n : Int <- out_string(\"Calculăm factorial pentru: \").in_int()", "            in", "                {", "                    out_string(\"Factorial recursiv: \").out_int(fact_rec(n))", "                        .out_string(\"\\n\");", "                    out_string(\"Factorial iterativ: \").out_int(fact_iter(n))", "                        .out_string(\"\\n\");", "                };", "        }", "    };", "", "    -- factorial implementat recursiv", "    fact_rec(n : Int) : Int {", "        if n = 0 then 1 else n * fact_rec(n - 1) fi", "    };", "", "    -- factorial implementat iterativ", "    fact_iter(n : Int) : Int {", "        let res : Int <- 1", "        -- Blocurile {} sunt văzute ca expresii. Valoarea ultimei expresii", "        -- din bloc este valoarea blocului. ", "        in", "            {", "                while (not (n = 0)) loop", "                    {", "                        res <- res * n;", "                        n <- n - 1;", "                    }", "                pool;", "                res;", "            }", "    };", "};"]))
                `shouldBe` [Class, Type "List", Inherits, Type "IO", LeftSquirly, Ident "elem", Colon, Type "Object", SemiColon, Ident "next", Colon, Type "List", SemiColon, Ident "init", LeftParen, Ident "e", Colon, Type "Object", Comma, Ident "n", Colon, Type "List", RightParen, Colon, Type "List", LeftSquirly, LeftSquirly, Ident "elem", Assign, Ident "e", SemiColon, Ident "next", Assign, Ident "n", SemiColon, Ident "self", SemiColon, RightSquirly, RightSquirly, SemiColon, Ident "print", LeftParen, RightParen, Colon, Type "IO", LeftSquirly, Let, Ident "str", Colon, Type "String", Assign, Case, Ident "elem", Of, Ident "s", Colon, Type "String", Results, Ident "s", SemiColon, Ident "n", Colon, Type "Int", Results, New, Type "A2I", Dot, Ident "i2a", LeftParen, Ident "n", RightParen, SemiColon, Ident "o", Colon, Type "Object", Results, LeftSquirly, Ident "abort", LeftParen, RightParen, SemiColon, String "", SemiColon, RightSquirly, SemiColon, Esac, In, LeftSquirly, Ident "out_string", LeftParen, Ident "str", Dot, Ident "concat", LeftParen, String " ", RightParen, RightParen, SemiColon, If, LeftParen, IsVoid, Ident "next", RightParen, Then, Ident "out_string", LeftParen, String "\n", RightParen, Else, Ident "next", Dot, Ident "print", LeftParen, RightParen, Fi, SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon, Class, Type "Main", Inherits, Type "IO", LeftSquirly, Ident "main", LeftParen, RightParen, Colon, Type "Object", LeftSquirly, LeftSquirly, Let, Ident "x", Colon, Type "Int", Assign, Integer 0, Comma, Ident "y", Colon, Type "String", Assign, String "!", Comma, Ident "z", Colon, Type "Int", Assign, Ident "x", Plus, Integer 2, Comma, Ident "empty", Colon, Type "List", Comma, Ident "list", Colon, Type "List", Assign, New, Type "List", Dot, Ident "init", LeftParen, Ident "x", Comma, New, Type "List", Dot, Ident "init", LeftParen, Ident "y", Comma, New, Type "List", Dot, Ident "init", LeftParen, Ident "z", Comma, Ident "empty", RightParen, RightParen, RightParen, In, Ident "list", Dot, Ident "print", LeftParen, RightParen, SemiColon, Let, Ident "n", Colon, Type "Int", Assign, Ident "out_string", LeftParen, String "Calcul\259m factorial pentru: ", RightParen, Dot, Ident "in_int", LeftParen, RightParen, In, LeftSquirly, Ident "out_string", LeftParen, String "Factorial recursiv: ", RightParen, Dot, Ident "out_int", LeftParen, Ident "fact_rec", LeftParen, Ident "n", RightParen, RightParen, Dot, Ident "out_string", LeftParen, String "\n", RightParen, SemiColon, Ident "out_string", LeftParen, String "Factorial iterativ: ", RightParen, Dot, Ident "out_int", LeftParen, Ident "fact_iter", LeftParen, Ident "n", RightParen, RightParen, Dot, Ident "out_string", LeftParen, String "\n", RightParen, SemiColon, RightSquirly, SemiColon, RightSquirly, RightSquirly, SemiColon, Ident "fact_rec", LeftParen, Ident "n", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, If, Ident "n", Equal, Integer 0, Then, Integer 1, Else, Ident "n", Asterisk, Ident "fact_rec", LeftParen, Ident "n", Minus, Integer 1, RightParen, Fi, RightSquirly, SemiColon, Ident "fact_iter", LeftParen, Ident "n", Colon, Type "Int", RightParen, Colon, Type "Int", LeftSquirly, Let, Ident "res", Colon, Type "Int", Assign, Integer 1, In, LeftSquirly, While, LeftParen, Not, LeftParen, Ident "n", Equal, Integer 0, RightParen, RightParen, Loop, LeftSquirly, Ident "res", Assign, Ident "res", Asterisk, Ident "n", SemiColon, Ident "n", Assign, Ident "n", Minus, Integer 1, SemiColon, RightSquirly, Pool, SemiColon, Ident "res", SemiColon, RightSquirly, RightSquirly, SemiColon, RightSquirly, SemiColon]

testErrorString :: Tokenizer -> Spec
testErrorString tokenize = do
    describe "Lexer" $ do
        it "should handle error string" $ do
            let tis = tokenize (unlines ["class A {", "    long : String <- \"lRLRZEEyGcL1vXxQIke5GzPdnalgabCDWDkohxom3vy2B3RfieCvgfqjMEkP9nxcQE0CdSFpZnlohvhDwr7aHRBlXbovc7YpPEKCLUxprxMsUIP4oerysNX6NNvv4OznWgi90lQPTqH1T3flaLUEdOo96RVsikngIJklXvPItGHYza6vkdUWtbahqi8Pewfq3uV1dKfEAnyuHc5XGqFEsAmEfbcYOLr4ADNpOqxWdRwISb14OWzZUA7MHLqtcv4zpkqN6sf4JaTtIMsu7tfYe6UfVkGjD2nQdE8kpq5OmpfmD0Znwh2Pab4EfAffN9LabBbRqEFlTNkhHpKKOPA7MenY1761UGsq4ia0wsKnDIFnCumudFUFjigEiTob96sjn66eDVK0ETzwGR4acif2T8QsFwCv8zNQDdPABbVzUI5qtiYZR2i2f3eVgyzU6N3NCHg6dwDDHfTN8hlaGdRZdTM7kHHYXvNGiNtvagJwTLml2arYYGnkn02ci4vm6BkRdiog6OCbTr3Ysqvbee5CjsWXjflSuiMyNiw1ZNvWJtmU0bObYAj5YTPwVPH5IjejXN8c3eTRfMpOYhTkTpY9nQoi2S5FRZKTFPK99X4l5AqaR7Fx3IfC7WOn5tJnhkbmFFoUnyPlrXGltNTodBYTwNCgWTTdaRIV7ulg6ttxTYd5VJQyVBk9kJei5NIjuIrwQ4xovu0ODIDVMZNIfO1KiaN81XwHx4JPchHBkGMIn5cHsfLMdXdV8AfzLV1lPveSg1MuBZEvWUG9prYFLcQIJcWqDfnZgHw1Yx4Iy5rA4kGi9EaPEMpJxGARpRSm2TawQfRnenZw8FpMYDCUri2RTB20fSfkGHDzET8EBFSU7lbAiBOluWSSyHkr3eU6JJuld83Nbp5iNFQV7VNL4MOReVJkHJPvoEbsQdxYAJQG3EBC8uNf6YXVCbDRBdEXXGcOXNLtBTS7t07SKc6iScxOTduV5K9v38vs3YmqpOFVObZ5TfMnoUngX69IzCMMsL9Ad\";", "    null : String <- \"a\0b\";", "    unterminated : String <- \"where to?", "    ;"] ++ "    eof : String <- \"nirvana")
            let ts = tokens tis
            ts `shouldBe` [Class, Type "A", LeftSquirly, Ident "long", Colon, Type "String", Assign, Illegal StringConstantTooLong, SemiColon, Ident "null", Colon, Type "String", Assign, Illegal StringContainsNull, SemiColon, Ident "unterminated", Colon, Type "String", Assign, Illegal StringUnterminated, SemiColon, Ident "eof", Colon, Type "String", Assign, Illegal EofInString]
            tis !! 7 `shouldBe` TokenInfo (Illegal StringConstantTooLong) 31
            tis !! 13 `shouldBe` TokenInfo (Illegal StringContainsNull) 1081
            tis !! 19 `shouldBe` TokenInfo (Illegal StringUnterminated) 1117
            tis !! 25 `shouldBe` TokenInfo (Illegal EofInString) 1154

testErrorComment :: Tokenizer -> Spec
testErrorComment tokenize = do
    describe "Lexer" $ do
        it "should handle error comment" $ do
            let tis = tokenize (unlines ["class A {", "    *)", "", "    x : Int;", "", "    (* some (* comment *)", "};"])
            let ts = tokens tis
            ts `shouldBe` [Class, Type "A", LeftSquirly, Illegal UnmatchedComment, Ident "x", Colon, Type "Int", SemiColon, Illegal EofInComment]
            tis !! 3 `shouldBe` TokenInfo (Illegal UnmatchedComment) 14
            tis !! 8 `shouldBe` TokenInfo (Illegal EofInComment) 36



testInvalidChar :: Tokenizer -> Spec
testInvalidChar tokenize = do
    describe "Lexer" $ do
        it "should handle error invalid char" $ do
            let tis = tokenize (unlines ["class A {", "    x : Int; #", "};"])
            let ts = tokens tis
            ts `shouldBe` [Class, Type "A", LeftSquirly, Ident "x", Colon, Type "Int", SemiColon, Illegal $ InvalidChar '#', RightSquirly, SemiColon]
            tis !! 7 `shouldBe` TokenInfo (Illegal $ InvalidChar '#') 23
