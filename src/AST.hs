module AST where

import Token

type Ast = [TokenInfo] -> Either [String] Program

newtype Program = Program [ClassDefinition]
    deriving (Show, Eq)

data ClassDefinition
    = ClassDefinition TokenInfo (Maybe TokenInfo) [FieldDefinition]
    | IllegalStatement
    deriving (Show, Eq)

data FieldDefinition
    = MethodDefinition TokenInfo [Formal] TokenInfo Expression
    | AttributeDefinition TokenInfo TokenInfo (Maybe Expression)
    deriving (Show, Eq)

data Formal = Formal TokenInfo TokenInfo
    deriving (Show, Eq)

data Expression
    = MethodCall (Maybe Expression) (Maybe TokenInfo) TokenInfo [Expression]
    | IfStatement Expression Expression Expression
    | WhileStatement Expression Expression
    | BlockStatement [Expression]
    | LetStatement [VariableDefinition] Expression
    | CaseStatement Expression [CaseOfDefinition]
    | NewStatement TokenInfo
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
    | AssignStatement TokenInfo Expression
    | IdentStatement TokenInfo
    | IntegerLiteral TokenInfo
    | StringLiteral TokenInfo
    | BoolLiteral TokenInfo
    deriving (Show, Eq)

data VariableDefinition = VariableDefinition TokenInfo TokenInfo (Maybe Expression)
    deriving (Show, Eq)

data CaseOfDefinition = CaseOfDefinition TokenInfo TokenInfo Expression
    deriving (Show, Eq)

isIllegal :: ClassDefinition -> Bool
isIllegal IllegalStatement = True
isIllegal _ = False
