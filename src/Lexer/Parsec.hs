{-# LANGUAGE OverloadedStrings #-}

module Lexer.Parsec where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, takeWhileP, choice, eof, getOffset, many, manyTill, parse, anySingle)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Token

type Parser = Parsec Void Text

tokenize :: Tokenizer
tokenize = fromRight [] . parse tokensP "" . T.pack
  where
    tokensP = sc *> many nextTokenItem <* eof

nextTokenItem :: Parser TokenInfo
nextTokenItem = do
    o <- getOffset
    tok <- nextToken
    return $ TokenInfo tok o

nextToken :: Parser Token
nextToken =
    choice
        [ Dot <$ symbol "."
        , At <$ symbol "@"
        , Results <$ symbol "=>"
        , Assign <$ symbol "<-"
        , Tilde <$ symbol "~"
        , Plus <$ symbol "+"
        , Minus <$ symbol "-"
        , Asterisk <$ symbol "*"
        , Slash <$ symbol "/"
        , Equal <$ symbol "="
        , LessEqual <$ symbol "<="
        , LessThan <$ symbol "<"
        , SemiColon <$ symbol ";"
        , Colon <$ symbol ":"
        , Comma <$ symbol ","
        , LeftParen <$ symbol "("
        , RightParen <$ symbol ")"
        , LeftSquirly <$ symbol "{"
        , RightSquirly <$ symbol "}"
        , Integer <$> numberP
        , lexeme stringP
        , Type <$> typeP
        , stringToToken <$> identP
        ]

numberP :: Parser Integer
numberP = lexeme L.decimal

stringToken :: Token -> Parser Token
stringToken tok@(Illegal _) = return tok
stringToken (String s)
    | length s > 1024 = Illegal StringConstantTooLong <$ consume
    | otherwise =
        choice
            [ String (reverse s) <$ char '\"'
            , Illegal EofInString <$ eof
            , Illegal StringUnterminated <$ char '\n'
            , Illegal StringContainsNull <$ char '\0' <* consume
            , charEscaped >>= stringToken . String . (:s)
            , anySingle >>= stringToken . String . (:s)
            ]
    where
        consume = manyTill anySingle (char '\"')
        charEscaped =
            char '\\'
                *> choice
                    [ char '\"'
                    , char '\\'
                    , char '\n'
                    , '\n' <$ char 'n'
                    , '\t' <$ char 't'
                    , '\b' <$ char 'b'
                    , '\f' <$ char 'f'
                    , alphaNumChar
                    ]
stringToken _ = error "stringToken: impossible"


stringP :: Parser Token
stringP = char '\"' *> stringToken (String "")

typeP :: Parser String
typeP = lexeme (T.unpack <$> (T.cons <$> upperChar <*> takeWhileP Nothing isIdentChar))

identP :: Parser String
identP = lexeme (T.unpack <$> (T.cons <$> lowerChar <*> takeWhileP Nothing isIdentChar))

sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment "--")
        (L.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
