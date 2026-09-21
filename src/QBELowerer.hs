{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedStrings #-}
module QBELowerer where

import AST
import IRTypes
import Control.Monad
import Control.Monad.RWS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

type SymbolTable = M.Map T.Text Ident -- Maps from symbol name to SSA target name

data QBEReader = QBEReader {
    structOffsetMap :: M.Map (T.Text, T.Text) Int, -- (Struct, Field)
    structSizeMap :: M.Map T.Text Int,
    falloffTarget :: Label,
    breakTarget :: Label,
    contTarget :: Label
}
defaultReader :: QBEReader
defaultReader = QBEReader M.empty M.empty "" "" "" -- BeNice ensured no naked breaks or continues for back edges. Empty string serves as a simple placeholder value.  

data QBEState = QBEState {
    nextIdent :: Int,
    nextLabel :: Int,
    stringLiteralMap :: M.Map T.Text Ident,
    toAllocBuffer :: [(SymbolId, Int)] -- int represents size 
}
defaultState :: QBEState
defaultState = QBEState 1 1 M.empty []

type QBEM a = RWS QBEReader () QBEState a

lowerStrux :: Program Typechecked -> QBEIR
lowerStrux pgrm = let (res, _) = evalRWS (lowerProgram pgrm) defaultReader defaultState in res

lowerProgram :: Program Typechecked -> QBEM QBEIR
lowerProgram (Program decls) = do
    let struxSize = generateStructSizes decls
    let struxOffset = generateStructOffsets decls struxSize
    loweredDecls <- local (\inEnv -> inEnv {structOffsetMap = struxOffset, structSizeMap = struxSize}) (mapM lowerDecl decls)
    return $ mconcat loweredDecls

lowerDecl :: Decl Typechecked -> QBEM QBEIR
lowerDecl (GlobalArrDecl vName aType@(ArrayType {})) = return $ QBEIR [] [] [QBEArrDecl vName (convertType $ exposeArrType aType) (getExtent aType)]
lowerDecl (GlobalVarDecl vName vType) = return $ QBEIR [] [] [QBEVarDecl vName (convertType vType)]
lowerDecl (StructDef name attrs) = let
    attrTypes = [createFieldIR attr | attr <- attrs ]
    in return $ QBEIR [] [QBEStruct name attrTypes] []
lowerDecl (FuncDef fName retType fArgs fBody) = do
    let argsIR = [ (if isPointer $ argType fArg then "s." <> (T.pack . show . argId) fArg else argName fArg, (convertType . argType) fArg) | fArg <- fArgs ]
    let argLoadInsts = concatMap lowerArgLoad fArgs
    argLoadLabel <- getNextLabel
    let fChunks = chunkStmts fBody
    fBlocks <- mapM lowerChunk fChunks
    hoistedDecls <- gets toAllocBuffer
    modify (\inState -> inState {toAllocBuffer = []})
    let hoistedInsts = map (\(sId, sSize) -> Alloc ("s." <> (T.pack . show) sId) sSize) hoistedDecls
    let initBlock = SimpleBlock (BasicBlock argLoadLabel (argLoadInsts ++ hoistedInsts) (Next ()))
    let fLinear = linearize $ initBlock : fBlocks
    return $ QBEIR [QBEFunc fName (convertType retType) argsIR fLinear] [] []
lowerDecl _ = error "lowerDecl: likely malformed array"

lowerChunk :: StmtChunk -> QBEM Block
lowerChunk (StdChunk stmts chunkTerm) = do
    bLabel <- getNextLabel
    loweredStmts <- mapM lowerStmt stmts
    let bStmts = concat loweredStmts
    (termInsts, term) <- lowerTerminator chunkTerm
    return $ SimpleBlock (BasicBlock bLabel (bStmts ++ termInsts) term)
lowerChunk (BranchChunk ifCond tBlock fBlock) = do
    cLabel <- getNextLabel
    tLabel <- getNextLabel
    fLabel <- getNextLabel
    mLabel <- getNextLabel
    (cReg, cInsts) <- lowerExpr ifCond
    let tChunks = chunkStmts tBlock
    let fChunks = chunkStmts fBlock
    tBlocks <- local (\inEnv -> inEnv {falloffTarget = mLabel}) (mapM lowerChunk tChunks)
    fBlocks <- local (\inEnv -> inEnv {falloffTarget = mLabel}) (mapM lowerChunk fChunks)
    return $ IfElseBlock cLabel cInsts cReg tLabel tBlocks fLabel fBlocks mLabel
lowerChunk (LoopChunk lInit lCond lIncr lBody) = do
    iLabel <- getNextLabel
    hLabel <- getNextLabel
    lLabel <- getNextLabel
    eLabel <- getNextLabel
    (_, initInsts) <- maybe (return (LitInt 0, [])) lowerExpr lInit
    (cReg, cInsts) <- maybe (return (LitInt 1, [])) lowerExpr lCond
    (_, incrInsts) <- maybe (return (LitInt 0, [])) lowerExpr lIncr
    let lChunks = chunkStmts lBody
    loweredBody <- local (\inEnv -> inEnv {falloffTarget = lLabel, contTarget = lLabel, breakTarget = eLabel}) (mapM lowerChunk lChunks)
    return $ LoopBlock iLabel initInsts cInsts cReg hLabel loweredBody lLabel incrInsts eLabel

lowerTerminator :: ChunkTerm -> QBEM ([Instruction], Terminator Tree)
lowerTerminator ContinueTerm = do {target <- asks contTarget; return ([], Jump target)}
lowerTerminator BreakTerm = do {target <- asks breakTarget; return ([], Jump target)}
lowerTerminator (RetTerm retExpr) = do
    retRes <- mapM lowerExpr retExpr
    case retRes of
        Just (retReg, retInsts) -> return (retInsts, Return $ Just retReg)
        Nothing -> return ([], Return Nothing)
lowerTerminator FalloffTerm = do {target <- asks falloffTarget; return ([], Jump target)}
lowerTerminator NextTerm = return ([], Next ())

-- Don't need to handle if-else, for loops, and while loops. Handled by chunks, only simple statements need to be handled.
-- Furthermore, don't need to handle returns, continues, or breaks. Chunked as terminators, and should NOT be present in the statements for a chunk.
-- Declarations need to be handled, but can be ignored. Just return nothing.
lowerStmt :: Stmt Typechecked -> QBEM [Instruction]
lowerStmt (LocalVarDecl _ sType sId) = do 
    sizeMap <- asks structSizeMap
    modify (\inState -> inState {toAllocBuffer = (sId, getTypeSize sType sizeMap) : toAllocBuffer inState})
    return []
lowerStmt (LocalArrDecl _ aType sId) = do
    sizeMap <- asks structSizeMap
    modify (\inState -> inState {toAllocBuffer = (sId, getTypeSize aType sizeMap) : toAllocBuffer inState})
    return []
lowerStmt (ExprStmt {}) = error "unimplemented"
lowerStmt stmt
    | isSpecialChunk stmt = error "lowerStmt: special chunk leaked into standard block"
    | otherwise = error "lowerStmt: terminator leaked into standard block"

lowerExpr ::  Expr Typechecked -> QBEM (Operand, [Instruction])
lowerExpr = error "unimplemented"

lowerArgLoad :: Argument Typechecked -> [Instruction]
lowerArgLoad arg
    | isPointer $ argType arg = []
    | otherwise = let
        regName = "s." <> (T.pack . show . argName) arg
        allocInst = Alloc regName 8
        storeInst = Store (convertType $ argType arg) (Reg $ argName arg) (Reg regName)
        in [allocInst, storeInst]


linearize :: [Block] -> [BasicBlock Linear]
linearize = error "unimplemented"

isPointer :: Type -> Bool
isPointer (StructType {}) = True
isPointer (ArrayType {}) = True
isPointer _ = False

getExtent :: Type -> Int
getExtent (ArrayType extent vType) = extent * getExtent vType
getExtent _ = 1

exposeArrType :: Type -> Type
exposeArrType (ArrayType _ aType) = exposeArrType aType
exposeArrType struxType = struxType

createFieldIR :: StructField Typechecked -> (QBEType, Int)
createFieldIR (Scalar _ fType) = (convertType fType, 1)
createFieldIR (Vector _ aType@(ArrayType _ vType)) = (convertType $ exposeArrType vType, getExtent aType)
createFieldIR (Struct name _) = (Aggregate name, 1)
createFieldIR _ = error "createFieldIR: malformed vector field"

getNextIdent :: QBEM Ident
getNextIdent = do
    i <- gets nextIdent
    modify (\inState -> inState {nextIdent = i + 1})
    return $ (T.pack . show) i

getNextLabel :: QBEM Label
getNextLabel = do
    i <- gets nextLabel
    modify (\inState -> inState {nextLabel = i + 1})
    return $ (T.pack . show) i

isStruct :: Decl Typechecked -> Bool
isStruct (StructDef _ _) = True
isStruct _ = False

generateStructOffsets :: [Decl Typechecked] -> M.Map T.Text Int -> M.Map (T.Text, T.Text) Int
generateStructOffsets [] _ = M.empty
generateStructOffsets ((StructDef sName attrs) : rest) sizeMap = findStructOffsets sName attrs sizeMap 0 <> generateStructOffsets rest sizeMap
generateStructOffsets (_ : rest) sizeMap = generateStructOffsets rest sizeMap

findStructOffsets :: T.Text -> [StructField Typechecked] -> M.Map T.Text Int -> Int -> M.Map (T.Text, T.Text) Int
findStructOffsets _ [] _ _ = M.empty
findStructOffsets sName (attr : attrs) sizeMap currOffset = let
    newMap = M.singleton (sName, getFieldName attr) currOffset
    oldMap = findStructOffsets sName attrs sizeMap (currOffset + getFieldSize attr sizeMap)
    in newMap <> oldMap

generateStructSizes :: [Decl Typechecked] -> M.Map T.Text Int
generateStructSizes decls = sizes
    where
        structAttrs = M.fromList [ (name, attrs) | StructDef name attrs <- decls ]

        sizes = M.map (sum . fmap fieldSize) structAttrs

        fieldSize :: StructField Typechecked -> Int
        fieldSize (Scalar _ fieldType) = typeSize fieldType
        fieldSize (Vector _ fieldType) = typeSize fieldType
        fieldSize (Struct _ attrs) = sum $ fmap fieldSize attrs

        typeSize :: Type -> Int
        typeSize (ArrayType extent elementType) = extent * typeSize elementType
        typeSize (StructType name) =
            case M.lookup name sizes of -- This does not cause an infinite loop. Assume A depends on B non-cyclically. A calls for the size of B. This lookup will create a thunk for the size of A, but never evalutate it. Only the thunk for the size of B is created. No loop.
                Just size -> size
                Nothing -> error $ "QBELowerer: struct type not found: " <> T.unpack name
        typeSize _ = 8





getFieldSize :: StructField Typechecked -> M.Map T.Text Int -> Int
getFieldSize (Scalar _ sType) sizeMap = getTypeSize sType sizeMap
getFieldSize (Vector _ aType@(ArrayType _ _)) sizeMap = getTypeSize aType sizeMap
getFieldSize (Struct _ attrs) sizeMap = sum $ fmap (`getFieldSize` sizeMap) attrs
getFieldSize (Vector _ _) _ = error "QBELowerer: vector field of struct without arraytype"

getFieldName :: StructField Typechecked -> T.Text
getFieldName (Scalar name _) = name
getFieldName (Vector name _) = name
getFieldName (Struct name _) = name

getTypeSize :: Type -> M.Map T.Text Int -> Int
getTypeSize IntType _ = 8
getTypeSize FloatType _ = 8
getTypeSize BoolType _ = 8
getTypeSize CharType _ = 8
getTypeSize (StructType sName) sizeMap = case M.lookup sName sizeMap of
    Nothing -> error "getTypeSize: struct name not found in size map"
    Just size -> size
getTypeSize (ArrayType extent aType) sizeMap = extent * getTypeSize aType sizeMap
getTypeSize _ _ = error "getTypeSize: malformed type, no meaningful size"



data StmtChunk
    = StdChunk [Stmt Typechecked] ChunkTerm
    | BranchChunk (Expr Typechecked) [Stmt Typechecked] [Stmt Typechecked]
    | LoopChunk (Maybe (Expr Typechecked)) (Maybe (Expr Typechecked)) (Maybe (Expr Typechecked)) [Stmt Typechecked]

data ChunkTerm
    = ContinueTerm
    | BreakTerm
    | RetTerm (Maybe (Expr Typechecked))
    | FalloffTerm
    | NextTerm

chunkStmts :: [Stmt Typechecked] -> [StmtChunk]
chunkStmts [] = []
chunkStmts stmts@(s : ss)
    | isSpecialChunk s = wrapSpecial s : chunkStmts ss
    | otherwise = let (newChunk, rest) = break isTerminator stmts
                  in case L.uncons rest of
                    Just (ContinueStmt, remaining) -> StdChunk newChunk ContinueTerm : chunkStmts remaining
                    Just (BreakStmt, remaining) -> StdChunk newChunk BreakTerm : chunkStmts remaining
                    Just (ReturnStmt retExpr, remaining) -> StdChunk newChunk (RetTerm retExpr) : chunkStmts remaining
                    Nothing -> [StdChunk newChunk FalloffTerm] -- We don't need to know why. Because functions will have a return statement by this point (parser injects returns at the end of void functions), this falloff will have meaning.
                    _ -> StdChunk newChunk NextTerm : chunkStmts rest -- This means the next statement is a special statement.


isTerminator :: Stmt Typechecked -> Bool
isTerminator (IfStmt {}) = True
isTerminator (ForStmt {}) = True
isTerminator (WhileStmt {}) = True
isTerminator (BreakStmt {}) = True
isTerminator (ContinueStmt {}) = True
isTerminator (ReturnStmt {}) = True
isTerminator _ = False

isSpecialChunk :: Stmt Typechecked -> Bool
isSpecialChunk (IfStmt {}) = True
isSpecialChunk (ForStmt {}) = True
isSpecialChunk (WhileStmt {}) = True
isSpecialChunk _ = False

wrapSpecial :: Stmt Typechecked -> StmtChunk
wrapSpecial (IfStmt c b1 b2) = BranchChunk c b1 b2
wrapSpecial (ForStmt i c inc b) = LoopChunk i c inc b
wrapSpecial (WhileStmt c b) = LoopChunk Nothing (Just c) Nothing b
wrapSpecial _ = error "wrapSpecial: non-special chunk type provided"

-- Meant for use for function paramenters. Is correct, but use exposeArrType first for a majority of other applications.
convertType :: Type -> QBEType
convertType IntType = Long
convertType FloatType = Double
convertType BoolType = Long
convertType CharType = Long
convertType (StructType name)= Aggregate name -- As a pointer 
convertType ArrayType {} = Long -- Also as a pointer
convertType _ = error "convertType: invalid type recieved, don't use on functions or voids"

