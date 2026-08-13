{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import AST

type StructEnv = Map.Map T.Text [StructField Resolved]

data Resolver = Resolver {
    scopeStack :: [Scope],
    nextId :: SymbolId,
    structEnv :: StructEnv,
    errors :: [T.Text]
}

type ResolverState = State Resolver
type Scope = Map.Map T.Text ResolvedInfo

initScope :: Scope
initScope = Map.fromList [("printInt", ResolvedInfo FunctionName (FuncType VoidType [IntType]) (-2)),
                          ("printFloat", ResolvedInfo FunctionName (FuncType VoidType [FloatType]) (-3)),
                          ("printBool", ResolvedInfo FunctionName (FuncType VoidType [BoolType]) (-4)),
                          ("printChar", ResolvedInfo FunctionName (FuncType VoidType [CharType]) (-5)),
                          ("printStr", ResolvedInfo FunctionName (FuncType VoidType [ArrayType 0 VoidType]) (-6))]

resolveStrux :: Program Parsed -> (Program Resolved, [T.Text], StructEnv)
resolveStrux program = let (a, s) = runState (resolveProgram program) (Resolver [initScope] 0 Map.empty []) in (a, errors s, structEnv s)

resolveProgram :: Program Parsed -> ResolverState (Program Resolved)
resolveProgram program = do
    _ <- enterScope -- will never exit this scope. this is the global scope. should always be present. underneath are builtins
    ds <- mapM resolveDecl $ declList program
    return $ Program ds

resolveDecl :: Decl Parsed -> ResolverState (Decl Resolved)
resolveDecl (GlobalVarDecl name t) = do
    _ <- insertInScope GlobalVar t name -- We don't need the information here. Storing it in the state to be retrieved for symbols when needed.
    return $ GlobalVarDecl name t
resolveDecl (GlobalArrDecl name t) = do
    _ <- insertInScope GlobalVar t name
    return $ GlobalVarDecl name t
resolveDecl (FuncDef name retType args body) = do
    _ <- enterScope
    mapM_ (\arg -> insertInScope LocalVar (argType arg) (argName arg)) args
    newBody <- mapM resolveStmt body
    _ <- exitScope
    _ <- insertInScope FunctionName (FuncType retType (map argType args)) name
    return $ FuncDef name retType args newBody
resolveDecl (StructDef name attrs) = do
    let newAttrs = map makeField attrs
    _ <- modify $ \currState -> currState {structEnv = Map.insert name newAttrs (structEnv currState)}
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
    _ <- insertInScope LocalVar t name
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
resolveExpr (BinaryExpr binOp leftExpr rightExpr _) = do
    newLeft <- resolveExpr leftExpr
    newRight <- resolveExpr rightExpr
    return $ BinaryExpr binOp newLeft newRight ()
resolveExpr (UnaryExpr unOp rightExpr _) = do
    newRight <- resolveExpr rightExpr
    return $ UnaryExpr unOp newRight ()
resolveExpr (FunctionCall name funcArgs _) = do
    newArgs <- mapM resolveExpr funcArgs
    newName <- resolveExpr name
    return $ FunctionCall newName newArgs ()
resolveExpr (ArrayIndex name idx _) = do
    newIdx <- resolveExpr idx
    newName <- resolveExpr name
    return $ ArrayIndex newName newIdx ()
resolveExpr (StructDeref name field _) = do
    newName <- resolveExpr name
    return $ StructDeref newName field ()
resolveExpr (Symbol name _) = do
    metadata <- findInScopes name
    return $ Symbol name metadata
resolveExpr (IntLiteral val _) = return $ IntLiteral val ()
resolveExpr (FloatLiteral val _) = return $ FloatLiteral val ()
resolveExpr (BoolLiteral val _) = return $ BoolLiteral val ()
resolveExpr (CharLiteral val _) = return $ CharLiteral val ()
resolveExpr (StringLiteral val _) = return $ StringLiteral val ()
resolveExpr (GroupedExpression parenExpr _) = do
    newExpr <- resolveExpr parenExpr
    return $ GroupedExpression newExpr ()


freshId :: ResolverState SymbolId
freshId = do
    s <- get
    let newId = nextId s
    _ <- put s { nextId = newId + 1}
    return newId

enterScope :: ResolverState ()
enterScope = modify pushScope
    where
        pushScope :: Resolver -> Resolver
        pushScope old = old { scopeStack = Map.empty : scopeStack old }

exitScope :: ResolverState ()
exitScope = modify popScope
    where
        popScope :: Resolver -> Resolver
        popScope old = old { scopeStack = tail $ scopeStack old }

findInScopes :: T.Text -> ResolverState ResolvedInfo
findInScopes name = do
    s <- get
    let scopes = scopeStack s
    case searchScopes name scopes of
        Just symInfo -> return symInfo
        Nothing    -> do
            let err = "Missing declaration of " <> name <> " in dummy location."
            modify $ \currState -> currState{ errors = err : errors currState }
            return $ ResolvedInfo LocalVar VoidType (-1) -- Poison doesn't matter, if we have an error we're not moving on anyway. Arbitrary SymbolKind and type.
    where
        searchScopes :: T.Text -> [Scope] -> Maybe ResolvedInfo
        searchScopes _ [] = Nothing
        searchScopes symName (scope:rest) = case Map.lookup symName scope of
            Just info -> Just info
            Nothing -> searchScopes symName rest

insertInScope :: SymbolKind -> Type -> T.Text -> ResolverState ResolvedInfo
insertInScope kind t name = do
    s <- get
    let scope = head $ scopeStack s
    if Map.member name scope
        then do
            let err = "Double declaration of " <> name <> " in dummy location."
            let info = ResolvedInfo kind t (-1)
            modify $ \currState -> currState { errors = err : errors currState } -- Double declaration, new insert not required for correctness assuming we stop because of the error (we should).
            return info
        else do
            newid <- freshId
            let info = ResolvedInfo kind t newid
            modify $ \currState -> currState { scopeStack = Map.insert name info scope: tail (scopeStack currState) }
            return info
