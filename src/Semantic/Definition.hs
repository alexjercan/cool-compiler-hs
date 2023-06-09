module Semantic.Definition where

import AST
import Scope
import Token

import Data.Map qualified as M

semantic :: Semantic
semantic p = case s of
        ([], programScope) -> Right programScope
        (errs, _) -> Left $ reverse errs
    where s = resolutionPass p $ classResolutionPass p $ classDefinitionPass p ([], globalScope)

classDefinitionPass :: Program -> ([TokenInfo SemanticError], ProgramScope) -> ([TokenInfo SemanticError], ProgramScope)
classDefinitionPass (Program classes) initS = foldl go initS classes
  where
    go (errs, programScope) (ClassDefinition (Type (TokenInfo o name)) _ _)
        | not $ isClassNameLegal name = (TokenInfo o (ClassHasIllegalName name) : errs, programScope)
        | isClassDefined name programScope = (TokenInfo o (ClassIsRedefined name) : errs, programScope)
        | otherwise = (errs, programScopeInsertClass name (ClassScope Nothing M.empty M.empty) programScope)
    go _ _ = error "classDefinitionPass: impossible"

classResolutionPass :: Program -> ([TokenInfo SemanticError], ProgramScope) -> ([TokenInfo SemanticError], ProgramScope)
classResolutionPass (Program classes) initS = foldl go initS classes
  where
    go (errs, programScope) (ClassDefinition (Type (TokenInfo _ name)) (Just (Type (TokenInfo o' name'))) _)
        | not $ isClassNameValidParentName name' = (TokenInfo o' (ClassHasIllegalParent name name') : errs, programScope)
        | not $ isClassDefined name' programScope = (TokenInfo o' (ClassUndefinedParent name name') : errs, programScope)
        | otherwise = (errs, programScopeClassAddParent name name' programScope)
    go (errs, programScope) (ClassDefinition (Type (TokenInfo _ name)) Nothing _)
        = (errs, programScopeClassAddParent name "Object" programScope)
    go _ _ = error "classResolutionPass: impossible"

resolutionPass :: Program -> ([TokenInfo SemanticError], ProgramScope) -> ([TokenInfo SemanticError], ProgramScope)
resolutionPass (Program classes) initS = foldl go initS classes
    where
        go (errs, programScope) (ClassDefinition (Type (TokenInfo o name)) _ _)
            | isClassNameInCycle name programScope = (TokenInfo o (ClassInheritanceCycle name) : errs, programScope)
            | otherwise = (errs, programScope)
        go _ _ = error "resolutionPass: impossible"
