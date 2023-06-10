module Scope where

import AST
import Token

import Data.Map qualified as M

type Semantic = Program -> Either [TokenInfo SemanticError] ProgramScope

newtype ProgramScope = ProgramScope
    { programScopeClasses :: M.Map String ClassScope
    }
    deriving (Show, Eq)

data ClassScope = ClassScope
    { classScopeParent :: Maybe String
    , classScopeAttributes :: M.Map String IdentSymbol
    , classScopeMethods :: M.Map String MethodScope
    }
    deriving (Show, Eq)

data MethodScope = MethodScope
    { methodScopeParent :: ClassScope
    , methodScopeParameters :: M.Map String IdentSymbol
    , methodScopeType :: ClassScope
    }
    deriving (Show, Eq)

data IdentSymbol
    = AttributeSymbol
        { attributeSymbolParent :: ClassScope
        , attributeSymbolType :: ClassScope
        }
    | ParameterSymbol
        { parameterSymbolParent :: MethodScope
        , parameterSymbolType :: ClassScope
        }
    deriving (Show, Eq)

globalScope :: ProgramScope
globalScope =
    let objectClass =
            ClassScope
                Nothing
                M.empty
                ( M.fromList
                    [ ("abort", abortMethod)
                    , ("type_name", typeNameMethod)
                    , ("copy", copyMethod)
                    ]
                )
        ioClass =
            ClassScope
                (Just "Object")
                M.empty
                ( M.fromList
                    [ ("out_string", outStringMethod)
                    , ("out_int", outIntMethod)
                    , ("in_string", inStringMethod)
                    , ("in_int", inIntMethod)
                    ]
                )
        intClass = ClassScope (Just "Object") M.empty M.empty
        stringClass =
            ClassScope
                (Just "Object")
                M.empty
                ( M.fromList
                    [ ("length", lengthMethod)
                    , ("concat", concatMethod)
                    , ("substr", substrMethod)
                    ]
                )
        boolClass = ClassScope (Just "Object") M.empty M.empty
        selfTypeClass = ClassScope (Just "Object") M.empty M.empty

        abortMethod = MethodScope objectClass M.empty selfTypeClass
        typeNameMethod = MethodScope objectClass M.empty stringClass
        copyMethod = MethodScope objectClass M.empty selfTypeClass
        outStringMethod = MethodScope ioClass (M.singleton "x" (ParameterSymbol outStringMethod stringClass)) ioClass
        outIntMethod = MethodScope ioClass (M.singleton "x" (ParameterSymbol outIntMethod intClass)) ioClass
        inStringMethod = MethodScope ioClass M.empty stringClass
        inIntMethod = MethodScope ioClass M.empty intClass
        lengthMethod = MethodScope stringClass M.empty intClass
        concatMethod = MethodScope stringClass (M.singleton "s" (ParameterSymbol concatMethod stringClass)) stringClass
        substrMethod =
            MethodScope
                stringClass
                ( M.fromList
                    [ ("i", ParameterSymbol substrMethod intClass)
                    , ("l", ParameterSymbol substrMethod intClass)
                    ]
                )
                stringClass
     in ProgramScope $
            M.fromList
                [ ("Object", objectClass)
                , ("IO", ioClass)
                , ("Int", intClass)
                , ("String", stringClass)
                , ("Bool", boolClass)
                , ("SELF_TYPE", selfTypeClass)
                ]

programScopeInsertClass :: String -> ClassScope -> ProgramScope -> ProgramScope
programScopeInsertClass name classScope (ProgramScope classes) =
    ProgramScope $ M.insert name classScope classes

programScopeClassAddParent :: String -> String -> ProgramScope -> ProgramScope
programScopeClassAddParent name parent (ProgramScope classes) =
    ProgramScope $ M.adjust (\p -> p{classScopeParent = Just parent}) name classes

data SemanticError
    = ClassHasIllegalName String
    | ClassIsRedefined String
    | ClassHasIllegalParent String String
    | ClassUndefinedParent String String
    | ClassInheritanceCycle String
    deriving (Eq)

instance Show SemanticError where
    show (ClassHasIllegalName name) = "Class has illegal name " ++ name
    show (ClassIsRedefined name) = "Class " ++ name ++ " is redefined"
    show (ClassHasIllegalParent name parent) = "Class " ++ name ++ " has illegal parent " ++ parent
    show (ClassUndefinedParent name parent) = "Class " ++ name ++ " has undefined parent " ++ parent
    show (ClassInheritanceCycle name) = "Inheritance cycle for class " ++ name

instance FormatErrorKind SemanticError where
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", Semantic error: " ++ show err

isClassNameLegal :: String -> Bool
isClassNameLegal "SELF_TYPE" = False
isClassNameLegal _ = True

isClassDefined :: String -> ProgramScope -> Bool
isClassDefined name (ProgramScope classes) = M.member name classes

isClassNameValidParentName :: String -> Bool
isClassNameValidParentName "SELF_TYPE" = False
isClassNameValidParentName "Int" = False
isClassNameValidParentName "String" = False
isClassNameValidParentName "Bool" = False
isClassNameValidParentName _ = True

isClassNameInCycle :: String -> ProgramScope -> Bool
isClassNameInCycle name (ProgramScope classes) = go name
  where
    go current = case M.lookup current classes >>= classScopeParent of
        (Just parent) | parent == name -> True
        (Just parent) -> go parent
        Nothing -> False
