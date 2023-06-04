{-# LANGUAGE LambdaCase #-}

module Parser.Parsec where

import AST
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, optional, parse, satisfy, sepBy, sepEndBy, try)
import Token

type Parser = Parsec Void [Token]

ast :: Ast
ast s = case parse programP "" s of
    Left e -> Left $ lines $ show e
    Right a -> Right a

programP :: Parser Program
programP = do
    classes <- sepEndBy classDefinitionP (tokenP SemiColon)
    return $ Program classes

classDefinitionP :: Parser ClassDefinition
classDefinitionP = do
    typ <- tokenP Class >> typeP
    inherits <- optional $ tokenP Inherits >> typeP
    fields <- between (tokenP LeftSquirly) (tokenP RightSquirly) (fieldP `sepEndBy` tokenP SemiColon)
    return $ ClassDefinition typ inherits fields

fieldP :: Parser FieldDefinition
fieldP = do
    ident <- identP
    choice
        [ do
            formals <- between (tokenP LeftParen) (tokenP RightParen) $ formalP `sepBy` tokenP Comma
            typ <- tokenP Colon >> typeP
            expr <- between (tokenP LeftSquirly) (tokenP RightSquirly) expressionP
            return $ MethodDefinition ident formals typ expr
        , do
            typ <- tokenP Colon >> typeP
            expr <- optional $ tokenP Assign >> expressionP
            return $ AttributeDefinition ident typ expr
        ]

formalP :: Parser Formal
formalP = do
    ident <- identP
    typ <- tokenP Colon >> typeP
    return $ Formal ident typ

expressionP :: Parser Expression
expressionP = undefined

tokenP :: Token -> Parser Token
tokenP t = satisfy (== t)

identP :: Parser String
identP = try $ do
    anySingle >>= \case
        Ident ident -> return ident
        _ -> fail "Expected identifier"

typeP :: Parser String
typeP = try $ do
    anySingle >>= \case
        Type typ -> return typ
        _ -> fail "Expected type"
