{-# LANGUAGE OverloadedStrings #-}
module Typechecker where

import qualified Data.Text as T
import Data.List
import Data.Map as Map
import AST
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad ( when, unless )
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
    newBody <- mapM (typecheckStmt retType) body
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

typecheckStmt :: Type -> Stmt Resolved -> TypecheckerM (Stmt Typechecked)
typecheckStmt _ (LocalVarDecl name varType) = return $ LocalVarDecl name varType
typecheckStmt _ (LocalArrDecl name arrType) = return $ LocalArrDecl name arrType
typecheckStmt _ (ExprStmt expr) = do {nExpr <- typecheckExpr expr; return $ ExprStmt nExpr}
typecheckStmt expectedType (IfStmt expr b1 b2) = do
    nExpr <- typecheckExpr expr
    nb1 <- mapM (typecheckStmt expectedType) b1
    nb2 <- mapM (typecheckStmt expectedType) b2
    return $ IfStmt nExpr nb1 nb2
typecheckStmt expectedType (ForStmt init check incr body) = do
    nInit <- mapM typecheckExpr init
    nCheck <- mapM typecheckExpr check
    nIncr <- mapM typecheckExpr incr
    nBody <- mapM (typecheckStmt expectedType) body
    return $ ForStmt nInit nCheck nIncr nBody
typecheckStmt expectedType (WhileStmt check body) = do
    nCheck <- typecheckExpr check
    nBody <- mapM (typecheckStmt expectedType) body
    return $ WhileStmt nCheck nBody
typecheckStmt expectedType (ReturnStmt ret) = do
    nRet <- mapM typecheckExpr ret
    let actualType = maybe VoidType getExprMeta nRet
    when (actualType /= expectedType && actualType /= ErrType) $
        tell ["Return type mismatch between expected " <> T.pack (show expectedType) <> " and actual " <> T.pack (show actualType) <> "."]
    return $ ReturnStmt nRet
typecheckStmt _ BreakStmt = return BreakStmt
typecheckStmt _ ContinueStmt = return ContinueStmt

typecheckExpr :: Expr Resolved -> TypecheckerM (Expr Typechecked)
typecheckExpr (BinaryExpr op left right _) = do
    nLeft <- typecheckExpr left
    nRight <- typecheckExpr right
    let lType = getExprMeta nLeft
        rType = getExprMeta nRight
        exprType = binTypeRes op lType rType
    when (exprType == ErrType) $
        tell ["Illegal types " <> T.pack (show lType) <> " and " <> T.pack (show rType) <> " for binary operator " <> T.pack (show op) <> "."]
    return $ BinaryExpr op nLeft nRight exprType
typecheckExpr (UnaryExpr op right _) = do
    nRight <- typecheckExpr right
    let rType = getExprMeta nRight
        exprType = unTypeRes op rType
    when (exprType == ErrType) $
        tell ["Illegal type " <> T.pack (show rType) <> "for unary operator " <> T.pack (show op) <> "."]
    return $ UnaryExpr op nRight exprType
typecheckExpr (FunctionCall name args _) = do
    -- blank string name indicates attempt to call non callable object
    (_, funx) <- ask
    nArgs <- mapM typecheckExpr args
    case name of
        "" -> do
            tell ["Attempt to call non-callable object."]
            return $ FunctionCall name nArgs ErrType

        _ -> case Map.lookup name funx of
            Just (retType, argTypes) ->
                let usedTypes = fmap getExprMeta nArgs
                in if usedTypes == argTypes
                    then return $ FunctionCall name nArgs retType
                    else do
                        tell ["Call to " <> name <> " has mismatched types.\nExpected: " <> T.pack (show argTypes) <> "\nGot: " <> T.pack (show usedTypes)]
                        return $ FunctionCall name nArgs ErrType
            Nothing -> do
                tell ["Function " <> name <> " not found."]
                return $ FunctionCall name nArgs ErrType
typecheckExpr (ArrayIndex arr idx _) = do
    nArr <- typecheckExpr arr
    nIdx <- typecheckExpr idx
    let intCheck = getExprMeta nIdx == IntType
    let arrCheck = isArrayType $ getExprMeta nArr
    unless intCheck $
        tell ["Array index must be integer"]
    unless arrCheck $
        tell ["Cannot index non-array type"]
    let resultType = if not intCheck || not arrCheck
        then ErrType
        else getElementType (getExprMeta nArr)
    return $ ArrayIndex nArr nIdx resultType
typecheckExpr (StructDeref struct field _) = do
    (strux, _) <- ask
    let getName (Scalar name _) = name
        getName (Vector name _) = name
        getName (Struct name _) = name
    nStruct <- typecheckExpr struct
    let (_, sName) = getStructType $ getExprMeta nStruct
    case Map.lookup sName strux of
        Just fields ->
            case find (\f -> getName f == field) fields of
                Just structField -> return $ StructDeref nStruct field (getFieldType structField)
                Nothing -> do
                    tell ["Struct " <> sName <> " does not have field " <> field <> "."]
                    return $ StructDeref nStruct field ErrType
        Nothing -> do
            tell ["Struct " <> sName <> " not registered."]
            return $ StructDeref nStruct field ErrType
typecheckExpr (Symbol name meta) = return $ Symbol name (symType meta)
typecheckExpr (IntLiteral val _) = return $ IntLiteral val IntType
typecheckExpr (FloatLiteral val _) = return $ FloatLiteral val FloatType
typecheckExpr (BoolLiteral val _) = return $ BoolLiteral val BoolType
typecheckExpr (CharLiteral val _) = return $ CharLiteral val CharType
typecheckExpr (StringLiteral val _) = return $ StringLiteral val (ArrayType 0 CharType)
typecheckExpr (GroupedExpression expr _) = do
    nExpr <- typecheckExpr expr
    return $ GroupedExpression nExpr (getExprMeta nExpr)

binTypeRes :: Op -> Type -> Type -> Type
binTypeRes _ ErrType _ = ErrType
binTypeRes _ _ ErrType = ErrType
binTypeRes op left right
    | isArithmeticOp op = if left == right && (left == IntType || left == FloatType)
                          then left
                          else ErrType
    | isCompOp op = if left == right && (left == IntType || left == FloatType)
                    then BoolType
                    else ErrType
    | isBoolOp op = if left == right && left == BoolType
                    then BoolType
                    else ErrType
    | op == ASSIGN = if left == right
                     then left 
                     else ErrType
    | otherwise = ErrType

unTypeRes :: Op -> Type -> Type
unTypeRes _ ErrType = ErrType
unTypeRes op right
    | op == SUB = if right == IntType || right == FloatType
                  then right
                  else ErrType
    | op == UNARYNOT = if right == BoolType
                       then BoolType
                       else ErrType
    | otherwise = ErrType

isArrayType :: Type -> Bool
isArrayType (ArrayType _ _) = True
isArrayType _ = False

isStructType :: Type -> Bool
isStructType (StructType _) = True
isStructType _ = False

getFieldType :: StructField Resolved -> Type
getFieldType (Scalar _ sType) = sType
getFieldType (Vector _ vType) = vType
getFieldType (Struct name _) = StructType name

getStructType :: Type -> (Type, T.Text)
getStructType (StructType name) = (StructType name, name)
getStructType _ = (ErrType, "")

getElementType :: Type -> Type
getElementType (ArrayType _ tElem) = tElem
getElementType _ = ErrType

isArithmeticOp :: Op -> Bool
isArithmeticOp ADD = True
isArithmeticOp SUB = True
isArithmeticOp MULT = True
isArithmeticOp DIV = True
isArithmeticOp MOD = True
isArithmeticOp BITAND = True
isArithmeticOp BITOR = True
isArithmeticOp _ = False

isCompOp :: Op -> Bool
isCompOp COMPGT = True
isCompOp COMPGE = True
isCompOp COMPLT = True
isCompOp COMPLE = True
isCompOp COMPEQ = True
isCompOp COMPNEQ = True
isCompOp _ = False

-- Function only used for binary operators, so no UNARYNOT
isBoolOp :: Op -> Bool
isBoolOp LOGAND = True
isBoolOp LOGOR = True
isBoolOp _ = False

getExprMeta :: Expr Typechecked -> Type
getExprMeta (Symbol _ exprType) = exprType
getExprMeta expr = eMeta expr