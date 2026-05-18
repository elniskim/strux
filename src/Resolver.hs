{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import AST

data Resolver = Resolver {
    scopeStack :: [Scope],
    nextId :: SymbolId,
    errors :: [T.Text]
}

type ResolverState = State Resolver
type Scope = Map.Map T.Text ResolvedInfo

resolveStrux :: Program Parsed -> (Program Resolved, [T.Text])
resolveStrux program = let (a, s) = runState (resolveProgram program) (Resolver [] 0 []) in (a, errors s)

resolveProgram :: Program Parsed -> ResolverState (Program Resolved)
resolveProgram program = do
    _ <- enterScope -- will never exit this scope. this is the global scope. should always be present
    ds <- mapM resolveDecl $ declList program
    return $ Program ds

resolveDecl :: Decl Parsed -> ResolverState (Decl Resolved)
resolveDecl (GlobalVarDecl name t) = do
    info <- insertInScope GlobalVar t name
    return $ GlobalVarDecl name t
resolveDecl (GlobalArrDecl name t) = do
    info <- insertInScope GlobalVar t name
    return $ GlobalVarDecl name t
resolveDecl (FuncDef name retType args body) = do
    _ <- enterScope
    mapM_ (\arg -> insertInScope LocalVar (argType arg) (argName arg)) args
    newBody <- mapM resolveStmt body
    _ <- exitScope
    return $ FuncDef name retType args newBody
resolveDecl (StructDef name attrs) = do
    let newAttrs = map makeField attrs
    return $ StructDef name newAttrs
    where
        makeField :: Decl Parsed -> StructField Resolved
        makeField (GlobalVarDecl name t) = Scalar name t
        makeField (GlobalArrDecl name t) = Vector name t
        makeField (StructDef name attrs) = Struct name (map makeField attrs)
        makeField _ = error "Unhandled declaration type in makeField."

resolveStmt :: Stmt Parsed -> ResolverState(Stmt Resolved)
resolveStmt (LocalVarDecl name t) = do
    _ <- insertInScope LocalVar t name
    return $ LocalVarDecl name t
resolveStmt (LocalArrDecl name t) = do
    _ <- insertInScope LocalVar t n
    return $ LocalArrDecl name t
resolveStmt (ExprStmt expr) = do 
    newExpr <- resolveExpr expr
    return $ ExprStmt newExpr
resolveStmt (IfStmt condition ifBlock elseBlock) = do
    newExpr <- resolveExpr condition
    _ <- enterScope
    newIfBlock <- mapM resolveStmt ifBlock
    _ <- exitScope
    _ <- enterScope
    newElseBlock <- mapM resolveStmt elseBlock
    _ <- exitScope
    return $ IfStmt newExpr newIfBlock newElseBlock
resolveStmt (ForStmt forInit forCond forIncr forBody) = do 
    newForInit <- mapM resolveExpr forInit
    newForCond <- mapM resolveExpr forCond
    newForIncr <- mapM resolveExpr forIncr
    _ <- enterScope
    newForBody <- mapM resolveStmt forBody
    _ <- exitScope
    return $ ForStmt newForInit newForCond newForIncr newForBody
resolveStmt (WhileStmt whileCond whileBody) = do 
    newWhileCond <- resolveExpr whileCond
    _ <- enterScope
    newWhileBody <- mapM resolveStmt whileBody
    _ <- exitScope
    return $ WhileStmt newWhileCond newWhileBody
resolveStmt (ReturnStmt val) = do
    newVal <- mapM resolveExpr val
    return $ ReturnStmt newVal
resolveStmt BreakStmt = return BreakStmt
resolveStmt ContinueStmt = return ContinueStmt

resolveExpr :: Expr Parsed -> ResolverState (Expr Resolved)







freshId :: ResolverState SymbolId
freshId = do
    s <- get
    let newId = nextId s
    _ <- put s { nextId = newId + 1}
    return newId

enterScope :: ResolverState ()
enterScope = do modify pushScope
    where
        pushScope :: Resolver -> Resolver
        pushScope old = old { scopeStack = Map.empty : scopeStack old }

exitScope :: ResolverState ()
exitScope = do modify popScope
    where
        popScope :: Resolver -> Resolver
        popScope old = old { scopeStack = tail $ scopeStack old }

findInScopes :: T.Text -> ResolverState (Maybe SymbolId)
findInScopes name = do
    s <- get
    let scopes = scopeStack s
    return $ searchScopes name scopes
        where
            searchScopes :: T.Text -> [Scope] -> Maybe SymbolId
            searchScopes _ [] = Nothing
            searchScopes symName (scope:rest) = case Map.lookup symName scope of
                Just info -> Just (symId info)
                Nothing -> searchScopes symName rest

insertInScope :: SymbolKind -> Type -> T.Text -> ResolverState ResolvedInfo
insertInScope kind t name = do
    s <- get
    let scope = head $ scopeStack s
    if Map.member name scope
        then do
            let err = "Double declaration of " <> name <> " in dummy location."
            let info = ResolvedInfo kind t (-1)
            modify $ \currState -> currState { errors = err : errors currState } -- Double declaration, new insert not required for correctness assuming we stop given the error (we should).
            return info
        else do
            newid <- freshId
            let info = ResolvedInfo kind t newid
            modify $ \currState -> currState { scopeStack = Map.insert name info scope: tail (scopeStack currState) }
            return info
