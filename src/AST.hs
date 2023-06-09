module AST where

import Token

type Ast = [Token] -> Either [TokenInfo ParserError] Program

data ParserError = InvalidParse
    deriving (Show, Eq)

instance FormatErrorKind ParserError where
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", Parser error: " ++ show err

newtype Program = Program [ClassDefinition]
    deriving (Show, Eq)

data ClassDefinition
    = ClassDefinition Token (Maybe Token) [FieldDefinition]
    | IllegalStatement (TokenInfo ParserError)
    deriving (Show, Eq)

data FieldDefinition
    = MethodDefinition Token [Formal] Token Expression
    | AttributeDefinition Token Token (Maybe Expression)
    deriving (Show, Eq)

data Formal = Formal Token Token
    deriving (Show, Eq)

data Expression
    = MethodCall (Maybe Expression) (Maybe Token) Token [Expression]
    | IfStatement Expression Expression Expression
    | WhileStatement Expression Expression
    | BlockStatement [Expression]
    | LetStatement [VariableDefinition] Expression
    | CaseStatement Expression [CaseOfDefinition]
    | NewStatement Token
    | NegationStatement Expression
    | IsVoidStatement Expression
    | MulStatement Expression Expression
    | DivStatement Expression Expression
    | AddStatement Expression Expression
    | SubStatement Expression Expression
    | LessThanStatement Expression Expression
    | LessThanOrEqualStatement Expression Expression
    | EqualStatement Expression Expression
    | NotStatement Expression
    | AssignStatement Token Expression
    | IdentStatement Token
    | IntegerLiteral Token
    | StringLiteral Token
    | BoolLiteral Token
    deriving (Show, Eq)

data VariableDefinition = VariableDefinition Token Token (Maybe Expression)
    deriving (Show, Eq)

data CaseOfDefinition = CaseOfDefinition Token Token Expression
    deriving (Show, Eq)

isIllegal :: ClassDefinition -> Bool
isIllegal (IllegalStatement _) = True
isIllegal _ = False
