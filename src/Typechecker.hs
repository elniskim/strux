{-# LANGUAGE OverloadedStrings #-}
module Typechecker where

import qualified Data.Text as T
import Data.List
import AST
import Control.Monad.Reader
import Control.Monad.Writer 
import Resolver (StructEnv, FuncEnv)

type TypecheckerM = ReaderT (StructEnv, FuncEnv) (Writer [T.Text])

typecheckStrux :: Program Resolved -> StructEnv -> FuncEnv -> (Program Typechecked, [T.Text])
typecheckStrux program strux funx = runWriter $ runReaderT (typecheckProgram program) (strux, funx)

typecheckProgram :: Program Resolved -> TypecheckerM (Program Typechecked)
typecheckProgram program = do 
    ds <- mapM typecheckDecl $ declList program
    return $ Program ds

typecheckDecl :: Decl Resolved -> TypecheckerM (Decl Typechecked)
typecheckDecl (FuncDef name retType args body) = do
    newBody <- mapM typecheckStmt body
    return $ FuncDef name retType args newBody
typecheckDecl (GlobalVarDecl name t) = return $ GlobalVarDecl name t
typecheckDecl (GlobalArrDecl name t) = return $ GlobalArrDecl name t
typecheckDecl (StructDef name attrs) = do 
    newAttrs <- mapM typecheckAttr attrs
    return $ StructDef name newAttrs

typecheckAttr :: StructField Resolved -> TypecheckerM (StructField Typechecked)
typecheckAttr (Scalar name sVal) = return $ Scalar name sVal
typecheckAttr (Vector name vVal) = return $ Vector name vVal
typecheckAttr (Struct name attrs) = do 
    newAttrs <- mapM typecheckAttr attrs
    return $ Struct name newAttrs

typecheckStmt :: Stmt Resolved -> TypecheckerM (Stmt Typechecked)
typecheckStmt (LocalVarDecl name varType) = return $ LocalVarDecl name varType
typecheckStmt (LocalArrDecl name arrType) = return $ LocalArrDecl name arrType
typecheckStmt (ExprStmt expr) = do {nExpr <- typecheckExpr expr; return $ ExprStmt nExpr}
typecheckStmt (IfStmt expr b1 b2) = do 
    nExpr <- typecheckExpr expr
    nb1 <- mapM typecheckStmt b1
    nb2 <- mapM typecheckStmt b2
    return $ IfStmt nExpr nb1 nb2
typecheckStmt (ForStmt init check incr body) = do 
    nInit <- mapM typecheckExpr init
    nCheck <- mapM typecheckExpr check
    nIncr <- mapM typecheckExpr incr
    nBody <- mapM typecheckStmt body 
    return $ ForStmt nInit nCheck nIncr nBody
typecheckStmt (WhileStmt check body) = do 
    nCheck <- typecheckExpr check
    nBody <- mapM typecheckStmt body
    return $ WhileStmt nCheck nBody
typecheckStmt (ReturnStmt ret) = do 
    nRet <- mapM typecheckExpr ret
    return $ ReturnStmt nRet
typecheckStmt BreakStmt = return BreakStmt
typecheckStmt ContinueStmt = return ContinueStmt 

typecheckExpr :: Expr Resolved -> TypecheckerM (Expr Typechecked)
typecheckExpr (BinaryExpr op left right _) = do 
    nLeft <- typecheckExpr left
    nRight <- typecheckExpr right
    let exprType = binTypeRes op (eMeta nLeft) (eMeta nRight)
    return $ BinaryExpr op left right exprType
typecheckExpr (UnaryExpr op right _) = do 
    nRight <- typecheckExpr right
    let exprType = unTypeRes op (eMeta nRight)
    return $ UnaryExpr op nRight exprType
typecheckExpr (FunctionCall name args _) = do 
    (_, funx) <- ask
    nArgs <- mapM typecheckExpr args
    case Map.lookup name funx of 
        Just (retType, argTypes) ->
            let usedTypes = map eMeta nArgs
            in if usedTypes == argTypes 
                then return $ FunctionCall name args retType
                else do
                    tell ["Call to" <> name <> "has mismatched types.\nExpected: " <> show argTypes <> "\nGot: " <> show $ usedTypes]
                    return $ FunctionCall name args ErrType
        Nothing -> do 
            tell ["Function " <> name <> " not found. This should never happen. Oops!"]
            return $ FunctionCall name args ErrType
typecheckExpr (ArrayIndex arr idx _) = do 
    nArr <- typecheckExpr arr
    nIdx <- typecheckExpr idx
    let intPoison = eMeta nIdx == IntType
    let arrPoison = isArrayType $ eMeta nArr
    unless intPoison $
        tell ["Array index must be integer"]
    unless arrPoison $
        tell ["Cannot index non-array type"]
    let resultType = if intPoison || arrPoison
        then ErrType
        else getElementType (eMeta nArr)
    return $ ArrayIndex nArr nIdx resultType 
typecheckExpr (StructDeref struct field _) = do 
    (strux, _) <- ask
    let getName = \field -> case field of
        (Scalar name _) -> name
        (Vector name _) -> name 
        (Struct name _) -> name
    nStruct <- typecheckExpr struct
    let (sType, sName) = getStructType $ eNeta nStruct
    case Map.lookup sName strux of
        Just fields ->
            case find (\f -> (getName f) == field) fields of 
                Just structField -> return $ StructDeref struct field (getFieldType structfield)
                Nothing -> do
                    tell ["Struct " <> sName <> " does not have field " <> field <> "."]
                    return $ StructDeref struct field ErrType
        Nothing -> do 
            tell ["Struct " <> sName <> " not registered."]
            return $ StructDeref struct field ErrType
typecheckExpr (Symbol name meta) = return $ Symbol name (symType meta)
typecheckExpr (IntLiteral val _) = return $ IntLiteral val IntType
typecheckExpr (FloatLiteral val _) = return $ FloatLiteral val FloatType
typecheckExpr (BoolLiteral val _) = return $ BoolLiteral val BoolType
typecheckExpr (CharLiteral val _) = return $ CharLiteral val CharType
typecheckExpr (StringLiteral val _) = return $ StringLiteral val (ArrayType 0 CharType)
typecheckExpr (GroupedExpression expr _) = do 
    nExpr <- typecheckExpr expr
    return $ GroupedExpression nExpr (eMeta nExpr)



isArrayType :: Type -> Bool
isArrayType (ArrayType _ _) = True
isArrayType _ = False

isStructType :: Type -> Bool
isStructType (StructType _ _) = True
isStructType _ = False

getFieldType :: StructField -> Type
getFieldType (Scalar _ sType) = sType
getFieldType (Vector _ vType) = vType
getFieldType (Struct name _) = StructType name

getStructType :: Type -> (Type, T.Text)
isStructType (StructType name) = (StructType name, name)
isStructType _ = (ErrType, "")

getElementType :: Type -> Type
getElementType (ArrayType _ elem) = elem 
getElementType _ = ErrType
