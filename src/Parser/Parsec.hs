{-# LANGUAGE LambdaCase #-}

module Parser.Parsec where

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, optional, parse, satisfy, sepBy, sepBy1, sepEndBy, sepEndBy1, try)
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
            expr <- optional (tokenP Assign >> expressionP)
            return $ AttributeDefinition ident typ expr
        ]

formalP :: Parser Formal
formalP = do
    ident <- identP
    typ <- tokenP Colon >> typeP
    return $ Formal ident typ

variableDefinitionP :: Parser VariableDefinition
variableDefinitionP = do
    ident <- identP
    typ <- tokenP Colon >> typeP
    expr <- optional $ tokenP Assign >> expressionP
    return $ VariableDefinition ident typ expr

caseOfDefinitionP :: Parser CaseOfDefinition
caseOfDefinitionP = do
    ident <- identP
    typ <- tokenP Colon >> typeP
    expr <- tokenP Results >> expressionP
    return $ CaseOfDefinition ident typ expr

tokenP :: Token -> Parser Token
tokenP t = satisfy (== t)

numberP :: Parser Integer
numberP = try $ do
    anySingle >>= \case
        Integer i -> return i
        _ -> fail "Expected integer"

stringP :: Parser String
stringP = try $ do
    anySingle >>= \case
        String s -> return s
        _ -> fail "Expected string"

booleanP :: Parser Bool
booleanP = try $ do
    anySingle >>= \case
        Boolean b -> return b
        _ -> fail "Expected boolean"

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

termP :: Parser Expression
termP =
    choice
        [ try $ do
            ident <- identP
            formals <- between (tokenP LeftParen) (tokenP RightParen) $ expressionP `sepBy` tokenP Comma
            return $ MethodCall Nothing Nothing ident formals
        , do
            iff <- tokenP If >> expressionP
            thenn <- tokenP Then >> expressionP
            elsee <- tokenP Else >> expressionP <* tokenP Fi
            return $ IfStatement iff thenn elsee
        , do
            while <- tokenP While >> expressionP
            loop <- tokenP Loop >> expressionP <* tokenP Pool
            return $ WhileStatement while loop
        , do
            exprs <- between (tokenP LeftSquirly) (tokenP RightSquirly) $ expressionP `sepEndBy` tokenP SemiColon
            return $ BlockStatement exprs
        , do
            vars <- tokenP Let >> variableDefinitionP `sepBy1` tokenP Comma
            expr <- tokenP In >> expressionP
            return $ LetStatement vars expr
        , do
            expr <- tokenP Case >> expressionP
            defs <- tokenP Of >> caseOfDefinitionP `sepEndBy1` tokenP SemiColon <* tokenP Esac
            return $ CaseStatement expr defs
        , do
            typ <- tokenP New >> typeP
            return $ NewStatement typ
        , do
            expr <- tokenP Tilde >> expressionP
            return $ NegationStatement expr
        , do
            expr <- tokenP IsVoid >> expressionP
            return $ IsVoidStatement expr
        , do
            expr <- tokenP Not >> expressionP
            return $ NotStatement expr
        , try $ do
            ident <- identP
            expr <- tokenP Assign >> expressionP
            return $ AssignStatement ident expr
        , do
            expr <- between (tokenP LeftParen) (tokenP RightParen) expressionP
            return $ ParenStatement expr
        , do
            IdentStatement <$> identP
        , do
            IntegerLiteral <$> numberP
        , do
            StringLiteral <$> stringP
        , do
            BoolLiteral <$> booleanP
        , do
            TypeStatement <$> typeP
        , do
            IllegalStatement <$ anySingle
        ]

staticDispatch :: Expression -> Expression -> Expression
staticDispatch expr (TypeStatement typ) = StaticDispatch expr typ
staticDispatch _ _ = IllegalStatement

dispatchStatement :: Expression -> Expression -> Expression
dispatchStatement (StaticDispatch expr ident) (MethodCall _ _ ident' formals) = MethodCall (Just expr) (Just ident) ident' formals
dispatchStatement expr (MethodCall _ _ ident formals) = MethodCall (Just expr) Nothing ident formals
dispatchStatement _ _ = IllegalStatement

expressionP :: Parser Expression
expressionP = makeExprParser termP table
  where
    table =
        [
            [ InfixL (tokenP At >> return staticDispatch)
            ]
        ,   [ InfixL (tokenP Dot >> return dispatchStatement)
            ]
        ,   [ InfixL (tokenP Asterisk >> return MulStatement)
            , InfixL (tokenP Slash >> return DivStatement)
            ]
        , [ InfixL (tokenP Plus >> return AddStatement)
            , InfixL (tokenP Minus >> return SubStatement)
            ]
        ,
            [ InfixL (tokenP Equal >> return EqualStatement)
            , InfixL (tokenP LessThan >> return LessThanStatement)
            , InfixL (tokenP LessEqual >> return LessThanOrEqualStatement)
            ]
        ]
