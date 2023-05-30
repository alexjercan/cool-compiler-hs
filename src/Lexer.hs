{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Char (isAlphaNum)
import Data.Text (Text, cons, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, takeWhileP), Parsec, choice, eof, many, manyTill, parse, satisfy, (<|>))
import Text.Megaparsec.Char (char, lowerChar, space1, string, upperChar)
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
    | Isvoid
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

{- key words - classes -}
classP :: Parser Token
classP = Class <$ pKeyword "class"

inheritsP :: Parser Token
inheritsP = Inherits <$ pKeyword "inherits"

newP :: Parser Token
newP = New <$ pKeyword "new"

{- key words - instructions - if -}

ifP :: Parser Token
ifP = If <$ pKeyword "if"

thenP :: Parser Token
thenP = Then <$ pKeyword "then"

elseP :: Parser Token
elseP = Else <$ pKeyword "else"

fiP :: Parser Token
fiP = Fi <$ pKeyword "fi"

{- key words - instructions - while -}

whileP :: Parser Token
whileP = While <$ pKeyword "while"

loopP :: Parser Token
loopP = Loop <$ pKeyword "loop"

poolP :: Parser Token
poolP = Pool <$ pKeyword "pool"

{- key words - instructions - case -}

caseP :: Parser Token
caseP = Case <$ pKeyword "case"

ofP :: Parser Token
ofP = Of <$ pKeyword "of"

esacP :: Parser Token
esacP = Esac <$ pKeyword "esac"

{- key words - instructions - let -}

letP :: Parser Token
letP = Let <$ pKeyword "let"

inP :: Parser Token
inP = In <$ pKeyword "in"

{- key words - instructions - others -}

notP :: Parser Token
notP = Not <$ pKeyword "not"

isvoidP :: Parser Token
isvoidP = Isvoid <$ pKeyword "isvoid"

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

{- operators - relational -}

equalP :: Parser Token
equalP = Equal <$ symbol "="

lessThanP :: Parser Token
lessThanP = LessThan <$ symbol "<"

lessEqualP :: Parser Token
lessEqualP = LessEqual <$ symbol "<="

{- operators - classes -}

dotP :: Parser Token
dotP = Dot <$ symbol "."

atP :: Parser Token
atP = At <$ symbol "@"

{- operators - assign -}

assignP :: Parser Token
assignP = Assign <$ symbol "<-"

{- operators - case results -}

resultsP :: Parser Token
resultsP = Results <$ symbol "=>"

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
stringP = String . pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

{- types -}

typeP :: Parser Token
typeP = Type <$> lexeme (cons <$> upperChar <*> takeWhileP Nothing isIdentChar)

identP :: Parser Token
identP = Ident <$> lexeme (cons <$> lowerChar <*> takeWhileP Nothing isIdentChar)

{- lexer -}

tokenP :: Parser Token
tokenP =
    choice
        [ classP
        , inheritsP
        , newP
        , ifP
        , thenP
        , elseP
        , fiP
        , whileP
        , loopP
        , poolP
        , caseP
        , ofP
        , esacP
        , letP
        , inP
        , notP
        , isvoidP
        , tildeP
        , plusP
        , minusP
        , asteriskP
        , slashP
        , equalP
        , lessThanP
        , lessEqualP
        , dotP
        , atP
        , assignP
        , resultsP
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
tokensP = many tokenP <* eof

tokenize :: String -> Either (ParseErrorBundle Text Void) [Token]
tokenize = parse tokensP "" . pack

tokenize' :: Text -> Either (ParseErrorBundle Text Void) [Token]
tokenize' = parse tokensP ""
