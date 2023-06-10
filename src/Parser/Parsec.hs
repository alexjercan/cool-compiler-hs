{-# LANGUAGE LambdaCase #-}

module Parser.Parsec where

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (State, modify, runState)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, anySingle, between, choice, lookAhead, manyTill, optional, runParserT, satisfy, sepBy, sepBy1, sepEndBy, sepEndBy1, try, withRecovery, (<|>))
import Token

type Parser = ParsecT Void [Token] (State [IllegalStatement])

ast :: Ast
ast s = case runState (runParserT programP "" s) [] of
    (Left e, _) -> error $ "Parser should never fail" ++ show e
    (Right p, []) -> Right p
    (_, errs) -> Left errs

programP :: Parser Program
programP = Program <$> manyTill classDefinitionP' (tokenP Eof)

classDefinitionP' :: Parser ClassDefinition
classDefinitionP' = withRecovery recover (classDefinitionP <* tokenP SemiColon)
  where
    recover _ = do
        modify (TokenInfo 0 InvalidParse :)
        manyTill anySingle (tokenP SemiColon <* lookAhead (tokenP Class <|> tokenP Eof)) >> try classDefinitionP'

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
            exprs <- between (tokenP LeftSquirly) (tokenP RightSquirly) $ expressionP `sepEndBy1` tokenP SemiColon
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
            between (tokenP LeftParen) (tokenP RightParen) expressionP
        , do
            IdentStatement <$> identP
        , do
            IntegerLiteral <$> numberP
        , do
            StringLiteral <$> stringP
        , do
            BoolLiteral <$> booleanP
        ]

staticDispatchP :: Parser (Expression -> Expression -> Expression)
staticDispatchP = do
    typ <- optional $ tokenP At >> typeP
    tokenP Dot >> lookAhead termP >>= \case
        (MethodCall _ _ ident formals) -> return $ \lhs _ -> MethodCall (Just lhs) typ ident formals
        _ -> fail "Expected method call"

expressionP :: Parser Expression
expressionP = makeExprParser termP table
  where
    table =
        [
            [ InfixL staticDispatchP
            ]
        ,
            [ InfixL (tokenP Asterisk >> return MulStatement)
            , InfixL (tokenP Slash >> return DivStatement)
            ]
        ,
            [ InfixL (tokenP Plus >> return AddStatement)
            , InfixL (tokenP Minus >> return SubStatement)
            ]
        ,
            [ InfixL (tokenP Equal >> return EqualStatement)
            , InfixL (tokenP LessThan >> return LessThanStatement)
            , InfixL (tokenP LessEqual >> return LessThanOrEqualStatement)
            ]
        ]

tokenP :: Token -> Parser Token
tokenP t = satisfy (== t)

numberP :: Parser (TokenInfo Integer)
numberP =
    satisfy isNumber >>= \case
        (Integer t) -> return t
        _ -> fail "Expected number"
  where
    isNumber (Integer _) = True
    isNumber _ = False

stringP :: Parser (TokenInfo String)
stringP =
    satisfy isString >>= \case
        (String t) -> return t
        _ -> fail "Expected string"
  where
    isString (String _) = True
    isString _ = False

booleanP :: Parser (TokenInfo Bool)
booleanP =
    satisfy isBoolean >>= \case
        (Boolean t) -> return t
        _ -> fail "Expected boolean"
  where
    isBoolean (Boolean _) = True
    isBoolean _ = False

identP :: Parser (TokenInfo String)
identP =
    satisfy isIdent >>= \case
        (Ident t) -> return t
        _ -> fail "Expected identifier"
  where
    isIdent (Ident _) = True
    isIdent _ = False

typeP :: Parser (TokenInfo String)
typeP =
    satisfy isType >>= \case
        (Type t) -> return t
        _ -> fail "Expected type"
  where
    isType (Type _) = True
    isType _ = False
