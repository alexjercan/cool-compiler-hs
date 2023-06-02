module Token where

import Data.Char (isAlphaNum)

type Tokenizer = String -> [Token]

data Token
    = Class
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
    deriving (Show, Eq)

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
