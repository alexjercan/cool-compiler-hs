module Semantic.Definition where

import AST
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as M
import Scope
import Token

type SemanticT = ReaderT Program (StateT ProgramScope (Writer [TokenInfo SemanticError]))

semantic :: Semantic
semantic p = case errs of
    [] -> Right scope
    _ -> Left errs
  where
    (scope, errs) = runWriter $ execStateT (runReaderT passes p) globalScope
    passes = classDefinitionPass
           >> classResolutionPass
           >> classCyclePass
           >> attributeDefinitionPass

classDefinitionPass :: SemanticT ()
classDefinitionPass = do
    Program classes <- ask
    forM_ classes $ (get >>=) . visitClass
  where
    visitClass (ClassDefinition (TokenInfo o name) _ _) programScope
        | not $ isClassNameLegal name = tell [TokenInfo o (ClassHasIllegalName name)]
        | isClassDefined name programScope = tell [TokenInfo o (ClassIsRedefined name)]
        | otherwise = modify $ programScopeInsertClass name (ClassScope Nothing M.empty)

classResolutionPass :: SemanticT ()
classResolutionPass = do
    Program classes <- ask
    forM_ classes $ (get >>=) . visitClass
  where
    visitClass (ClassDefinition (TokenInfo _ name) (Just (TokenInfo o' name')) _) programScope
        | not $ isClassNameValidParentName name' = tell [TokenInfo o' (ClassHasIllegalParent name name')]
        | not $ isClassDefined name' programScope = tell [TokenInfo o' (ClassUndefinedParent name name')]
        | otherwise = modify $ programScopeClassAddParent name name'
    visitClass (ClassDefinition (TokenInfo _ name) Nothing _) _ =
        modify $ programScopeClassAddParent name "Object"

classCyclePass :: SemanticT ()
classCyclePass = do
    Program classes <- ask
    forM_ classes $ (get >>=) . visitClass
  where
    visitClass (ClassDefinition (TokenInfo o name) _ _) programScope
        | isClassNameInCycle name programScope = tell [TokenInfo o (ClassInheritanceCycle name)]
        | otherwise = return ()

attributeDefinitionPass :: SemanticT ()
attributeDefinitionPass = do
    Program classes <- ask
    forM_ classes visitClass
  where
    visitClass (ClassDefinition (TokenInfo _ name) _ fields) =
        forM_ fields $ (gets (programScopeGetClass name) >>=) . visitField name
    visitField _ _ Nothing = return ()
    visitField cls (AttributeDefinition (TokenInfo o name) (TokenInfo _ typ) _) (Just classScope)
        | not $ isAttributeNameLegal name = tell [TokenInfo o (AttributeWithIllegalName cls name)]
        | isAttributeDefined name classScope = tell [TokenInfo o (AttributeIsRedefined cls name)]
        | otherwise = modify $ programScopeInsertClass cls (classScopeAddAttribute name (AttributeSymbol typ) classScope)
    visitField _ _ _ = return ()
