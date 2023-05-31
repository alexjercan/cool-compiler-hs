{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Char (isAlphaNum)
import Data.Text (Text, cons, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, takeWhileP), Parsec, anySingle, choice, eof, many, manyTill, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space1, string, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ParseErrorBundle)

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
    | String Text
    | Type Text
    | Ident Text
    deriving (Show, Eq)

isIdentChar :: Char -> Bool
isIdentChar = (||) <$> isAlphaNum <*> (== '_')

type Parser = Parsec Void Text

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

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme $ string keyword <* notFollowedBy (satisfy isIdentChar)

{- classes -}
classP :: Parser Token
classP = Class <$ pKeyword "class"

inheritsP :: Parser Token
inheritsP = Inherits <$ pKeyword "inherits"

newP :: Parser Token
newP = New <$ pKeyword "new"

notP :: Parser Token
notP = Not <$ pKeyword "not"

isvoidP :: Parser Token
isvoidP = IsVoid <$ pKeyword "isvoid"

dotP :: Parser Token
dotP = Dot <$ symbol "."

atP :: Parser Token
atP = At <$ symbol "@"

{- if -}

ifP :: Parser Token
ifP = If <$ pKeyword "if"

thenP :: Parser Token
thenP = Then <$ pKeyword "then"

elseP :: Parser Token
elseP = Else <$ pKeyword "else"

fiP :: Parser Token
fiP = Fi <$ pKeyword "fi"

{- while -}

whileP :: Parser Token
whileP = While <$ pKeyword "while"

loopP :: Parser Token
loopP = Loop <$ pKeyword "loop"

poolP :: Parser Token
poolP = Pool <$ pKeyword "pool"

{- case -}

caseP :: Parser Token
caseP = Case <$ pKeyword "case"

ofP :: Parser Token
ofP = Of <$ pKeyword "of"

esacP :: Parser Token
esacP = Esac <$ pKeyword "esac"

resultsP :: Parser Token
resultsP = Results <$ symbol "=>"

{- let -}

letP :: Parser Token
letP = Let <$ pKeyword "let"

assignP :: Parser Token
assignP = Assign <$ symbol "<-"

inP :: Parser Token
inP = In <$ pKeyword "in"

{- operators - unary -}

tildeP :: Parser Token
tildeP = Tilde <$ symbol "~"

{- operators - binary -}

plusP :: Parser Token
plusP = Plus <$ symbol "+"

minusP :: Parser Token
minusP = Minus <$ symbol "-"

asteriskP :: Parser Token
asteriskP = Asterisk <$ symbol "*"

slashP :: Parser Token
slashP = Slash <$ symbol "/"

equalP :: Parser Token
equalP = Equal <$ symbol "="

lessEqualP :: Parser Token
lessEqualP = LessEqual <$ symbol "<="

lessThanP :: Parser Token
lessThanP = LessThan <$ symbol "<"

{- code - separators -}

semiColonP :: Parser Token
semiColonP = SemiColon <$ symbol ";"

colonP :: Parser Token
colonP = Colon <$ symbol ":"

commaP :: Parser Token
commaP = Comma <$ symbol ","

{- code - blocks -}

leftParenP :: Parser Token
leftParenP = LeftParen <$ symbol "("

rightParenP :: Parser Token
rightParenP = RightParen <$ symbol ")"

leftSquirlyP :: Parser Token
leftSquirlyP = LeftSquirly <$ symbol "{"

rightSquirlyP :: Parser Token
rightSquirlyP = RightSquirly <$ symbol "}"

{- values -}

integerP :: Parser Token
integerP = Integer <$> lexeme L.decimal

booleanP :: Parser Token
booleanP = Boolean <$> (True <$ pKeyword "true" <|> False <$ pKeyword "false")

stringP :: Parser Token
stringP = String . pack <$> lexeme (char '\"' *> manyTill charLiteral (char '\"'))
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

{- types -}

typeP :: Parser Token
typeP = Type <$> lexeme (cons <$> upperChar <*> takeWhileP Nothing isIdentChar)

identP :: Parser Token
identP = Ident <$> lexeme (cons <$> lowerChar <*> takeWhileP Nothing isIdentChar)

{- lexer -}

tokenP :: Parser Token
tokenP =
    choice
        [ try classP
        , try inheritsP
        , try newP
        , try notP
        , try isvoidP
        , dotP
        , atP
        , try ifP
        , try thenP
        , try elseP
        , try fiP
        , try whileP
        , try loopP
        , try poolP
        , try caseP
        , try ofP
        , try esacP
        , resultsP
        , try letP
        , assignP
        , try inP
        , tildeP
        , plusP
        , minusP
        , asteriskP
        , slashP
        , equalP
        , lessEqualP
        , lessThanP
        , semiColonP
        , colonP
        , commaP
        , leftParenP
        , rightParenP
        , leftSquirlyP
        , rightSquirlyP
        , integerP
        , booleanP
        , stringP
        , typeP
        , identP
        ]

tokensP :: Parser [Token]
tokensP = sc *> many tokenP <* eof

tokenize :: String -> Either (ParseErrorBundle Text Void) [Token]
tokenize = parse tokensP "" . pack

tokenize' :: Text -> Either (ParseErrorBundle Text Void) [Token]
tokenize' = parse tokensP ""
