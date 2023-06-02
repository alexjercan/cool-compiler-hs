{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, anySingle, choice, eof, many, manyTill, parse, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Token

type Parser = Parsec Void Text

tokenize :: Tokenizer
tokenize = fromRight [] . parse tokensP "" . T.pack
  where
    tokensP = sc *> many nextToken <* eof

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
        , String <$> stringP
        , Type <$> typeP
        , stringToToken <$> identP
        ]

numberP :: Parser Integer
numberP = lexeme L.decimal

stringP :: Parser String
stringP = lexeme (char '\"' *> manyTill charLiteral (char '\"'))
  where
    charLiteral = charEscaped <|> anySingle
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
