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
tokenize = tokenizeFromFile ""

tokenizeFromFile :: String -> Tokenizer
tokenizeFromFile fn s = case parse (sc *> many nextTokenItem <* eof) fn $ T.pack s of
    Left e -> error $ "Lexer should never fail" ++ show e
    Right tis ->
        let illegalTokens = filter isIllegal tis
         in if null illegalTokens
                then Right $ filter ((/= BlockComment) . token) tis
                else Left $ map (formatError fn s) illegalTokens

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
        , Illegal UnmatchedComment <$ symbol "*)"
        , Asterisk <$ symbol "*"
        , Slash <$ symbol "/"
        , Equal <$ symbol "="
        , LessEqual <$ symbol "<="
        , LessThan <$ symbol "<"
        , SemiColon <$ symbol ";"
        , Colon <$ symbol ":"
        , Comma <$ symbol ","
        , symbol "(*" *> commentP
        , LeftParen <$ symbol "("
        , RightParen <$ symbol ")"
        , LeftSquirly <$ symbol "{"
        , RightSquirly <$ symbol "}"
        , Integer <$> numberP
        , char '\"' *> stringP
        , Type <$> typeP
        , stringToToken <$> identP
        , Illegal . InvalidChar <$> anySingle <* sc
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
            , charEscaped >>= stringToken . String . (: s)
            , anySingle >>= stringToken . String . (: s)
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
stringP = lexeme $ stringToken (String "")

typeP :: Parser String
typeP = lexeme (T.unpack <$> (T.cons <$> upperChar <*> takeWhileP Nothing isIdentChar))

identP :: Parser String
identP = lexeme (T.unpack <$> (T.cons <$> lowerChar <*> takeWhileP Nothing isIdentChar))

commentBlockToken :: Int -> Token -> Parser Token
commentBlockToken _ tok@(Illegal _) = return tok
commentBlockToken i BlockComment =
    choice
        [ symbol "*)" *> if i == 0 then return BlockComment else commentBlockToken (i - 1) BlockComment
        , Illegal EofInComment <$ eof
        , symbol "(*" >> commentBlockToken (i + 1) BlockComment
        , anySingle >> commentBlockToken i BlockComment
        ]
commentBlockToken _ _ = error "commentBlockToken: impossible"

commentP :: Parser Token
commentP = lexeme $ commentBlockToken 0 BlockComment

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
