{-# LANGUAGE OverloadedStrings #-}

module Lexer.Parsec where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, empty, eof, getOffset, many, manyTill, parse, takeWhileP)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Token

type Parser = Parsec Void Text

tokenize :: Tokenizer
tokenize s = case parse (sc *> many nextToken <* eof) "" $ T.pack s of
    Left e -> error $ "Lexer should never fail" ++ show e
    Right tis -> case foldr go ([], []) tis of
        ([], tokens) -> Right $ tokens ++ [Eof]
        (errs, _) -> Left errs
  where
    go (Illegal ti) (errs, tokens) = (ti : errs, tokens)
    go (BlockComment _) (errs, tokens) = (errs, tokens)
    go ti (errs, tokens) = (errs, ti : tokens)

nextToken :: Parser Token
nextToken = do
    o <- getOffset
    choice
        [ Dot <$ symbol "."
        , At <$ symbol "@"
        , Results <$ symbol "=>"
        , Assign <$ symbol "<-"
        , Tilde <$ symbol "~"
        , Plus <$ symbol "+"
        , Minus <$ symbol "-"
        , Illegal (TokenInfo o UnmatchedComment) <$ symbol "*)"
        , Asterisk <$ symbol "*"
        , Slash <$ symbol "/"
        , Equal <$ symbol "="
        , LessEqual <$ symbol "<="
        , LessThan <$ symbol "<"
        , SemiColon <$ symbol ";"
        , Colon <$ symbol ":"
        , Comma <$ symbol ","
        , symbol "(*" *> lexeme (commentBlockToken 0 $ BlockComment $ TokenInfo o ())
        , LeftParen <$ symbol "("
        , RightParen <$ symbol ")"
        , LeftSquirly <$ symbol "{"
        , RightSquirly <$ symbol "}"
        , Integer . TokenInfo o <$> numberP
        , char '\"' *> lexeme (stringToken $ String $ TokenInfo o "")
        , Type . TokenInfo o <$> typeP
        , stringToToken . TokenInfo o <$> identP
        , Illegal . TokenInfo o . InvalidChar <$> anySingle <* sc
        ]

numberP :: Parser Integer
numberP = lexeme L.decimal

stringToken :: Token -> Parser Token
stringToken tok@(Illegal _) = return tok
stringToken (String (TokenInfo o s))
    | length s > 1024 = Illegal (TokenInfo o StringConstantTooLong) <$ consume
    | otherwise =
        choice
            [ String (TokenInfo o (reverse s)) <$ char '\"'
            , Illegal (TokenInfo o EofInString) <$ eof
            , Illegal (TokenInfo o StringUnterminated) <$ char '\n'
            , Illegal (TokenInfo o StringContainsNull) <$ char '\0' <* consume
            , charEscaped >>= stringToken . String . TokenInfo o . (: s)
            , anySingle >>= stringToken . String . TokenInfo o . (: s)
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

typeP :: Parser String
typeP = lexeme (T.unpack <$> (T.cons <$> upperChar <*> takeWhileP Nothing isIdentChar))

identP :: Parser String
identP = lexeme (T.unpack <$> (T.cons <$> lowerChar <*> takeWhileP Nothing isIdentChar))

commentBlockToken :: Int -> Token -> Parser Token
commentBlockToken _ tok@(Illegal _) = return tok
commentBlockToken i b@(BlockComment (TokenInfo o _)) =
    choice
        [ symbol "*)" *> if i == 0 then return b else commentBlockToken (i - 1) b
        , Illegal (TokenInfo o EofInComment) <$ eof
        , symbol "(*" >> commentBlockToken (i + 1) b
        , anySingle >> commentBlockToken i b
        ]
commentBlockToken _ _ = error "commentBlockToken: impossible"

sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment "--")
        empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
