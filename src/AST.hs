module AST where

import Token

type Ast = [Token] -> Either [String] Program

newtype Program = Program [ClassDefinition]
    deriving (Show, Eq)

data ClassDefinition = ClassDefinition String (Maybe String) [FieldDefinition]
    deriving (Show, Eq)

data FieldDefinition
    = MethodDefinition String [Formal] String Expression
    | AttributeDefinition String String (Maybe Expression)
    deriving (Show, Eq)

data Formal = Formal String String
    deriving (Show, Eq)

data Expression
    = Dispatch (Maybe String) String [Expression]
    | MethodCall String [Expression]
    | IfStatement Expression Expression Expression
    | WhileStatement Expression Expression
    | BlockStatement [Expression]
    | LetStatement [VariableDefinition] Expression
    | CaseStatement Expression [CaseOfDefinition]
    | NewStatement String
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
    | AssignStatement String Expression
    | ParenStatement Expression
    | IdentStatement String
    | IntStatement Int
    | StringStatement String
    | BoolStatement Bool
    deriving (Show, Eq)

data VariableDefinition = VariableDefinition String String (Maybe Expression)
    deriving (Show, Eq)

data CaseOfDefinition = CaseOfDefinition String String Expression
    deriving (Show, Eq)
