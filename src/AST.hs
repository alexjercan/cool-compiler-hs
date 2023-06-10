module AST where

import Token

type Ast = [Token] -> Either [TokenInfo ParserError] Program

type IllegalStatement = TokenInfo ParserError

data ParserError = InvalidParse
    deriving (Show, Eq)

instance FormatErrorKind ParserError where
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", Parser error: " ++ show err

newtype Program = Program [ClassDefinition]
    deriving (Show, Eq)

data ClassDefinition
    = ClassDefinition Type (Maybe Type) [FieldDefinition]
    deriving (Show, Eq)

data FieldDefinition
    = MethodDefinition Name [Formal] Type Expression
    | AttributeDefinition Name Type (Maybe Expression)
    deriving (Show, Eq)

data Formal = Formal Name Type
    deriving (Show, Eq)

data Expression
    = MethodCall (Maybe Expression) (Maybe Type) Name [Expression]
    | IfStatement Expression Expression Expression
    | WhileStatement Expression Expression
    | BlockStatement [Expression]
    | LetStatement [VariableDefinition] Expression
    | CaseStatement Expression [CaseOfDefinition]
    | NewStatement Type
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
    | AssignStatement Name Expression
    | IdentStatement Name
    | IntegerLiteral (TokenInfo Integer)
    | StringLiteral (TokenInfo String)
    | BoolLiteral (TokenInfo Bool)
    deriving (Show, Eq)

data VariableDefinition = VariableDefinition Name Type (Maybe Expression)
    deriving (Show, Eq)

data CaseOfDefinition = CaseOfDefinition Name Type Expression
    deriving (Show, Eq)

type Type = TokenInfo String
type Name = TokenInfo String
