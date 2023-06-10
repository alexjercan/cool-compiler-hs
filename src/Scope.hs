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
    }
    deriving (Show, Eq)

data IdentSymbol
    = AttributeSymbol
        { attributeSymbolType :: String
        }
    deriving (Show, Eq)

globalScope :: ProgramScope
globalScope =
    let objectClass = ClassScope Nothing M.empty
        ioClass = ClassScope (Just "Object") M.empty
        intClass = ClassScope (Just "Object") M.empty
        stringClass = ClassScope (Just "Object") M.empty
        boolClass = ClassScope (Just "Object") M.empty
        selfTypeClass = ClassScope (Just "Object") M.empty
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

programScopeGetClass :: String -> ProgramScope -> Maybe ClassScope
programScopeGetClass name (ProgramScope classes) = M.lookup name classes

programScopeClassAddParent :: String -> String -> ProgramScope -> ProgramScope
programScopeClassAddParent name parent (ProgramScope classes) =
    ProgramScope $ M.adjust (\p -> p{classScopeParent = Just parent}) name classes

classScopeAddAttribute :: String -> IdentSymbol -> ClassScope -> ClassScope
classScopeAddAttribute name symbol (ClassScope parent attributes) =
    ClassScope parent $ M.insert name symbol attributes

data SemanticError
    = ClassHasIllegalName String
    | ClassIsRedefined String
    | ClassHasIllegalParent String String
    | ClassUndefinedParent String String
    | ClassInheritanceCycle String
    | AttributeWithIllegalName String String
    | AttributeIsRedefined String String
    deriving (Eq)

instance Show SemanticError where
    show (ClassHasIllegalName name) = "Class has illegal name " ++ name
    show (ClassIsRedefined name) = "Class " ++ name ++ " is redefined"
    show (ClassHasIllegalParent name parent) = "Class " ++ name ++ " has illegal parent " ++ parent
    show (ClassUndefinedParent name parent) = "Class " ++ name ++ " has undefined parent " ++ parent
    show (ClassInheritanceCycle name) = "Inheritance cycle for class " ++ name
    show (AttributeWithIllegalName cls name) = "Class " ++ cls ++ " has attribute with illegal name " ++ name
    show (AttributeIsRedefined cls name) = "Class " ++ cls ++ " redefines attribute " ++ name

instance FormatErrorKind SemanticError where
    formatErrorKind fn s (TokenInfo pos err) = formatError fn s pos ++ ", Semantic error: " ++ show err

isClassNameLegal :: String -> Bool
isClassNameLegal "SELF_TYPE" = False
isClassNameLegal _ = True

isClassDefined :: String -> ProgramScope -> Bool
isClassDefined name (ProgramScope classes) = M.member name classes

isAttributeDefined :: String -> ClassScope -> Bool
isAttributeDefined name (ClassScope _ attributes) = M.member name attributes

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

isAttributeNameLegal :: String -> Bool
isAttributeNameLegal "self" = False
isAttributeNameLegal _ = True
