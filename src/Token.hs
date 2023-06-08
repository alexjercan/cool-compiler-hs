module Token where

import Data.Char (isAlphaNum)

type Tokenizer = String -> Either [String] [TokenInfo]

data TokenInfo = TokenInfo
    { token :: Token
    , offset :: Int
    }
    deriving (Show, Eq, Ord)

data Error
    = StringConstantTooLong
    | StringContainsNull
    | StringUnterminated
    | EofInString
    | UnmatchedComment
    | EofInComment
    | InvalidChar Char
    deriving (Eq, Ord)

instance Show Error where
    show :: Error -> String
    show StringConstantTooLong = "String constant too long"
    show StringContainsNull = "String contains null character"
    show StringUnterminated = "Unterminated string constant"
    show EofInString = "EOF in string constant"
    show UnmatchedComment = "Unmatched *)"
    show EofInComment = "EOF in comment"
    show (InvalidChar c) = "Invalid character: " ++ [c]

data Token
    = Eof
    | Class
    | Inherits
    | New
    | If
    | Then
    | Else
    | Fi
    | While
    | Loop
    | Pool
    | Case
    | Of
    | Esac
    | Let
    | In
    | Not
    | IsVoid
    | Tilde
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Equal
    | LessThan
    | LessEqual
    | Dot
    | At
    | Assign
    | Results
    | SemiColon
    | Colon
    | Comma
    | LeftParen
    | RightParen
    | LeftSquirly
    | RightSquirly
    | Integer Integer
    | Boolean Bool
    | String String
    | Type String
    | Ident String
    | Illegal Error
    | BlockComment
    deriving (Show, Eq, Ord)

isIdentChar :: Char -> Bool
isIdentChar = (||) <$> isAlphaNum <*> (== '_')

stringToToken :: String -> Token
stringToToken "class" = Class
stringToToken "inherits" = Inherits
stringToToken "new" = New
stringToToken "not" = Not
stringToToken "isvoid" = IsVoid
stringToToken "if" = If
stringToToken "then" = Then
stringToToken "else" = Else
stringToToken "fi" = Fi
stringToToken "while" = While
stringToToken "loop" = Loop
stringToToken "pool" = Pool
stringToToken "case" = Case
stringToToken "of" = Of
stringToToken "esac" = Esac
stringToToken "let" = Let
stringToToken "in" = In
stringToToken "true" = Boolean True
stringToToken "false" = Boolean False
stringToToken x = Ident x

isIllegal :: TokenInfo -> Bool
isIllegal (TokenInfo (Illegal _) _) = True
isIllegal _ = False

offsetToLineColumn :: String -> Int -> (Int, Int)
offsetToLineColumn str offset =
    let linesList = lines str
        (line, column) = go offset linesList 1
     in (line, column)
  where
    go _ [] _ = (0, 0)
    go n (l : ls) line
        | n < length l = (line, n + 1)
        | otherwise = go (n - length l - 1) ls (line + 1)

formatError :: String -> String -> Int -> String
formatError fn s pos =
    let (l, c) = offsetToLineColumn s pos
     in show fn ++ ", " ++ "line " ++ show l ++ ":" ++ show c

lexicalError :: String -> String -> TokenInfo -> String
lexicalError fn s (TokenInfo (Illegal err) pos) = formatError fn s pos ++ ", Lexical error: " ++ show err
lexicalError _ _ _ = error "formatError: impossible"
