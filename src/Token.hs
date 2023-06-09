module Token where

import Data.Char (isAlphaNum)

type Tokenizer = String -> Either [TokenInfo LexicalError] [Token]

data TokenInfo a = TokenInfo
    { offset :: Int
    , value :: a
    }
    deriving (Show, Eq, Ord)

data LexicalError
    = StringConstantTooLong
    | StringContainsNull
    | StringUnterminated
    | EofInString
    | UnmatchedComment
    | EofInComment
    | InvalidChar Char
    deriving (Eq, Ord)

instance Show LexicalError where
    show :: LexicalError -> String
    show StringConstantTooLong = "String constant too long"
    show StringContainsNull = "String contains null character"
    show StringUnterminated = "Unterminated string constant"
    show EofInString = "EOF in string constant"
    show UnmatchedComment = "Unmatched *)"
    show EofInComment = "EOF in comment"
    show (InvalidChar c) = "Invalid character: " ++ [c]

instance FormatErrorKind LexicalError where
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", Lexical error: " ++ show err

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
    | Integer (TokenInfo Integer)
    | Boolean (TokenInfo Bool)
    | String (TokenInfo String)
    | Type (TokenInfo String)
    | Ident (TokenInfo String)
    | Illegal (TokenInfo LexicalError)
    | BlockComment (TokenInfo ())
    deriving (Show, Eq, Ord)

isIdentChar :: Char -> Bool
isIdentChar = (||) <$> isAlphaNum <*> (== '_')

stringToToken :: TokenInfo String -> Token
stringToToken (TokenInfo _ "class") = Class
stringToToken (TokenInfo _ "inherits") = Inherits
stringToToken (TokenInfo _ "new") = New
stringToToken (TokenInfo _ "not") = Not
stringToToken (TokenInfo _ "isvoid") = IsVoid
stringToToken (TokenInfo _ "if") = If
stringToToken (TokenInfo _ "then") = Then
stringToToken (TokenInfo _ "else") = Else
stringToToken (TokenInfo _ "fi") = Fi
stringToToken (TokenInfo _ "while") = While
stringToToken (TokenInfo _ "loop") = Loop
stringToToken (TokenInfo _ "pool") = Pool
stringToToken (TokenInfo _ "case") = Case
stringToToken (TokenInfo _ "of") = Of
stringToToken (TokenInfo _ "esac") = Esac
stringToToken (TokenInfo _ "let") = Let
stringToToken (TokenInfo _ "in") = In
stringToToken (TokenInfo o "true") = Boolean $ TokenInfo o True
stringToToken (TokenInfo o "false") = Boolean $ TokenInfo o False
stringToToken (TokenInfo o x) = Ident $ TokenInfo o x

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

class (Show a) => FormatErrorKind a where
    formatErrorKind :: String -> String -> TokenInfo a -> String
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", " ++ show err
