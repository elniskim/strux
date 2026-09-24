{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Redundant lambda" -}
{- HLINT ignore "Replace case with fromMaybe" -}
module QBELowerer where

import AST
import IRTypes
import Control.Monad
import Control.Monad.RWS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char (ord)

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
    nextLitString :: Int,
    stringLiteralMap :: M.Map T.Text Ident,
    toAllocBuffer :: [(SymbolId, Int)], -- int represents size
    currStructTemps :: M.Map Int Int, -- cleared on statement level
    maxStructTemps :: M.Map Int Int -- cleared on function level
}
defaultState :: QBEState
defaultState = QBEState 1 1 1 M.empty [] M.empty M.empty

type QBEM a = RWS QBEReader () QBEState a

lowerStrux :: Program Typechecked -> QBEIR
lowerStrux pgrm = let (res, _) = evalRWS (lowerProgram pgrm) defaultReader defaultState in res

lowerProgram :: Program Typechecked -> QBEM QBEIR
lowerProgram (Program decls) = do
    let struxSize = generateStructSizes decls
    let struxOffset = generateStructOffsets decls struxSize
    loweredDecls <- local (\inEnv -> inEnv {structOffsetMap = struxOffset, structSizeMap = struxSize}) (mapM lowerDecl decls)
    strMap <- gets stringLiteralMap
    let strDecls = generateLiteralStrings strMap
    return $ mconcat loweredDecls

lowerDecl :: Decl Typechecked -> QBEM QBEIR
lowerDecl (GlobalArrDecl vName aType@(ArrayType {})) = return $ QBEIR [] [] [QBEArrDecl vName (convertType $ exposeArrType aType) (getExtent aType)]
lowerDecl (GlobalVarDecl vName vType) = return $ QBEIR [] [] [QBEVarDecl vName (convertType vType)]
lowerDecl (StructDef name attrs) = let
    attrTypes = [createFieldIR attr | attr <- attrs ]
    in return $ QBEIR [] [QBEStruct name attrTypes] []
lowerDecl (FuncDef fName retType fArgs fBody) = do
    let argsIR = ([(".sret", Long) | isStructType retType]) ++ [ (if isPointer $ argType fArg then ".s_" <> (T.pack . show . argId) fArg else argName fArg, (convertType . argType) fArg) | fArg <- fArgs ]
    let argLoadInsts = concatMap lowerArgLoad fArgs
    argLoadLabel <- getNextLabel
    let fChunks = chunkStmts fBody
    fBlocks <- mapM lowerChunk fChunks
    hoistedDecls <- gets toAllocBuffer
    modify (\inState -> inState {toAllocBuffer = []})
    let hoistedInsts = map (\(sId, sSize) -> Alloc (".s_" <> (T.pack . show) sId) sSize) hoistedDecls
    maxMap <- gets maxStructTemps
    let structTempInsts = generateStructTemps maxMap
    modify (\inState -> inState {maxStructTemps = M.empty})
    let initBlock = SimpleBlock (BasicBlock argLoadLabel (argLoadInsts ++ hoistedInsts ++ structTempInsts) (Next ()))
    let fLinear = linearize $ initBlock : fBlocks
    return $ QBEIR [QBEFunc fName (if isStructType retType || retType == VoidType then Nothing else Just $ convertType retType) argsIR fLinear] [] []
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
    mLabel <- getNextLabel
    (cReg, cInsts) <- lowerExpr ifCond
    let tChunks = chunkStmts tBlock
    let fChunks = chunkStmts fBlock
    tBlocks <- local (\inEnv -> inEnv {falloffTarget = mLabel}) (mapM lowerChunk tChunks)
    fBlocks <- local (\inEnv -> inEnv {falloffTarget = mLabel}) (mapM lowerChunk fChunks)
    return $ IfElseBlock cLabel cInsts cReg tBlocks fBlocks mLabel
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
lowerTerminator (RetTerm retExpr)
    | isStructType $ maybe VoidType getExprType retExpr = do
        let sType = maybe VoidType getExprType retExpr
        let sName = getStructName sType
        sizeMap <- asks structSizeMap
        let sSize = sizeMap M.! sName
        retRes <- mapM lowerExpr retExpr
        case retRes of
            Just (retReg, retInsts) -> return (retInsts ++ [Blit retReg (Reg ".sret") sSize], Return Nothing)
            Nothing -> error "lowerTerminator: this cannot realistically happen"
    | otherwise = do
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
lowerStmt (ExprStmt expr) = do
    (_, insts) <- lowerExpr expr
    modify (\inState -> inState {currStructTemps = M.empty})
    return insts
lowerStmt stmt
    | isSpecialChunk stmt = error "lowerStmt: special chunk leaked into standard block"
    | otherwise = error "lowerStmt: terminator leaked into standard block"

lowerExpr ::  Expr Typechecked -> QBEM (Operand, [Instruction])
lowerExpr (BinaryExpr ASSIGN lVal rVal (StructType sName)) = do -- BeNice ensured that we aren't trying to assign arrays.
    (lValReg, lValInsts) <- lowerExpr lVal
    (rValReg, rValInsts) <- lowerExpr rVal
    sizeMap <- asks structSizeMap
    let copySize = sizeMap M.! sName
    return (rValReg, lValInsts ++ rValInsts ++ [Blit rValReg lValReg copySize])
lowerExpr (BinaryExpr ASSIGN lVal rVal aType) = do
    (lValReg, lValInsts) <- lowerExpr lVal
    (rValReg, rValInsts) <- lowerExpr rVal
    return (rValReg, lValInsts ++ rValInsts ++ [Store (convertType aType) rValReg lValReg])
lowerExpr (BinaryExpr binOp lVal rVal eType) = do
    let loweredOp = convertBinOp binOp eType
    (lValReg, lValInsts) <- lowerExpr lVal
    (rValReg, rValInsts) <- lowerExpr rVal
    tempReg <- getNextIdent
    return (Reg tempReg, lValInsts ++ rValInsts ++ [BinInstr loweredOp (convertType eType) tempReg lValReg rValReg])
lowerExpr (UnaryExpr AST.SUB rVal eType) = do
    (rValReg, rValInsts) <- lowerExpr rVal
    tempReg <- getNextIdent
    return (Reg tempReg, rValInsts ++ [BinInstr IRTypes.SUB (convertType eType) tempReg (if eType == FloatType then LitFloat 0 else LitInt 0) rValReg])
lowerExpr (UnaryExpr UNARYNOT rVal eType) = do
    (rValReg, rValInsts) <- lowerExpr rVal
    tempReg <- getNextIdent
    return (Reg tempReg, rValInsts ++ [UnInstr NEG (convertType eType) tempReg rValReg])
lowerExpr(UnaryExpr {}) = error "lowerExpr: invalid unary operation provided"
lowerExpr (FunctionCall fName fArgs (FuncType (StructType retTypeName) _)) = do
    loweredArgs <- mapM lowerExpr fArgs
    let argRegs = map fst loweredArgs
    let argInsts = concatMap snd loweredArgs
    sizeMap <- asks structSizeMap
    retValTemp <- getStructTemp (sizeMap M.! retTypeName)
    let argIR = zip (map (convertType . getExprType) fArgs) argRegs
    return (retValTemp, argInsts ++ [Call Nothing fName ((Long, retValTemp) : argIR)])
lowerExpr (FunctionCall fName fArgs (FuncType retType _)) = do
    loweredArgs <- mapM lowerExpr fArgs
    let argRegs = map fst loweredArgs
    let argInsts = concatMap snd loweredArgs
    tempReg <- getNextIdent
    let argIR = zip (map (convertType . getExprType) fArgs) argRegs
    return (Reg tempReg, argInsts ++ [Call (Just (fName, convertType retType)) fName argIR])
lowerExpr (FunctionCall {}) = error "lowerExpr: cannot lower function without FuncType"
lowerExpr (ArrayIndex arrExpr idxExpr (ArrayType _ baseType)) = do
    sizeMap <- asks structSizeMap
    let stepSize = getTypeSize baseType sizeMap
    (arrReg, arrInsts) <- lowerExpr arrExpr
    (idxReg, idxInsts) <- lowerExpr idxExpr
    offsetReg <- getNextIdent
    finReg <- getNextIdent
    return (Reg finReg, arrInsts ++ idxInsts ++ [BinInstr MUL Long offsetReg (LitInt stepSize) idxReg, BinInstr IRTypes.ADD Long finReg arrReg (Reg offsetReg)])
lowerExpr (ArrayIndex {}) = error "lowerExpr: cannot index array with non-array type"
lowerExpr (StructDeref sExpr fName _) = do -- We don't blit here, we want to modify the original struct. That's half the point!
    offsetMap <- asks structOffsetMap
    let sName = getStructName $ getExprType sExpr
    let offset = offsetMap M.! (sName, fName)
    (structReg, structInsts)<- lowerExpr sExpr
    tempReg <- getNextIdent
    return (Reg tempReg, structInsts ++ [BinInstr IRTypes.ADD Long tempReg structReg (LitInt offset)])
lowerExpr (Symbol sName sInfo)
    | isPointer $ symType sInfo = return (Reg $ ".s_" <> (T.pack . show. symType) sInfo, [])
    | otherwise = do
        tempReg <- getNextIdent
        return (Reg tempReg, [Load ((convertType . symType) sInfo) tempReg (if symKind sInfo == GlobalVar then Global sName else Reg $ ".s_" <> (T.pack . show . symId) sInfo)])
lowerExpr (IntLiteral num _) = return (LitInt num, [])
lowerExpr (FloatLiteral num _) = return (LitFloat num, [])
lowerExpr (BoolLiteral bVal _) = return (if bVal then LitInt 1 else LitInt 0, [])
lowerExpr (CharLiteral char _) = return (LitInt $ ord char, [])
lowerExpr (StringLiteral stringVal _) = do
    strMap <- gets stringLiteralMap
    case M.lookup stringVal strMap of
        Nothing -> do
            litStrNum <- getNextLitString
            let litStrName = ".litStr_" <> litStrNum
            let newStrMap = M.insert stringVal litStrName strMap
            modify (\inState -> inState {stringLiteralMap = newStrMap})
            return (Global litStrName, [])
        Just litStrName -> return (Global litStrName, [])


lowerExpr (GroupedExpression expr _) = lowerExpr expr

lowerArgLoad :: Argument Typechecked -> [Instruction]
lowerArgLoad arg
    | isPointer $ argType arg = []
    | otherwise = let
        regName = ".s_" <> (T.pack . show . argName) arg
        allocInst = Alloc regName 8
        storeInst = Store (convertType $ argType arg) (Reg $ argName arg) (Reg regName)
        in [allocInst, storeInst]

getStructTemp :: Int -> QBEM Operand
getStructTemp sSize = do
    currMap <- gets currStructTemps
    maxMap <- gets maxStructTemps
    let currNumTemps = case M.lookup sSize currMap of
                        Nothing -> 0
                        Just num -> num
    let maxNumTemps = case M.lookup sSize maxMap of
                        Nothing -> 0
                        Just num -> num
    let newNumTemps = currNumTemps + 1
    let newCurrMap = M.insert sSize newNumTemps currMap
    let newMaxMap = M.insert sSize (max newNumTemps maxNumTemps) maxMap
    modify (\inState -> inState {currStructTemps = newCurrMap, maxStructTemps = newMaxMap})
    return $ Reg (".temp_" <> (T.pack . show) sSize <> "_" <> (T.pack . show) newNumTemps)



linearize :: [Block] -> [BasicBlock Linear]
linearize [] = []
linearize blocks = let treeBlocks = concatMap firstPass blocks in secondPass treeBlocks
    where
        firstPass :: Block -> [BasicBlock Tree]
        firstPass (SimpleBlock basicBlock) = [basicBlock]
        firstPass (IfElseBlock cLabel cInsts cReg tBlocks fBlocks mLabel) = let
            tLin = concatMap firstPass tBlocks
            fLin = concatMap firstPass fBlocks
            tLabel = diveForLabel tLin mLabel
            fLabel = diveForLabel fLin mLabel
            in [BasicBlock cLabel cInsts (JumpNZ cReg tLabel fLabel)] ++ tLin ++ fLin ++ [BasicBlock mLabel [] (Next ())]
        firstPass (LoopBlock initLabel initInsts cInsts cReg hLabel lBody latchLabel incrInsts eLabel) = let
            lLin = concatMap firstPass lBody
            loopLabel = diveForLabel lLin eLabel
            in [BasicBlock initLabel initInsts (Jump hLabel),
            BasicBlock hLabel cInsts (JumpNZ cReg loopLabel eLabel)] ++
            lLin ++
            [BasicBlock latchLabel incrInsts (Jump hLabel), BasicBlock eLabel [] (Next ())]


        secondPass :: [BasicBlock Tree] -> [BasicBlock Linear]
        secondPass [] = []
        secondPass [BasicBlock _ _ (Next _)] = []
        secondPass (curr@(BasicBlock _ _ (Next _)) : rest@((BasicBlock nextTarget _ _) : _)) = curr { terminator = Jump nextTarget } : secondPass rest
        secondPass ((BasicBlock label insts term) : rest) = BasicBlock label insts (clipTerm term) : secondPass rest

        diveForLabel :: [BasicBlock a] -> Label -> Label
        diveForLabel [] fallback = fallback
        diveForLabel ((BasicBlock {blockLabel = label}) : _) _ = label

        clipTerm :: Terminator Tree -> Terminator Linear
        clipTerm (Jump label) = Jump label
        clipTerm (JumpNZ reg t1 t2) = JumpNZ reg t1 t2
        clipTerm (Return retVal) = Return retVal
        clipTerm (Next _) = error "clipTerm: no nexts can be present in the linearized block list"


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

getNextLitString :: QBEM T.Text
getNextLitString = do
    i <- gets nextLitString
    modify (\inState -> inState {nextLitString = i + 1})
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
chunkStmts [] = [] -- Don't need any special checking, functions cannot be empty because we require return statements, and the parser injects a void return in functions that need it.
chunkStmts stmts@(s : ss)
    | isSpecialChunk s = wrapSpecial s : chunkStmts ss
    | otherwise = let (newChunk, rest) = break isTerminator stmts
                  in case L.uncons rest of
                    -- Don't need to process rest of statement block, as a terminator will make all proceeding code dead. (?)
                    Just (ContinueStmt, _) -> [StdChunk newChunk ContinueTerm] -- : chunkStmts remaining
                    Just (BreakStmt, _) -> [StdChunk newChunk BreakTerm] -- : chunkStmts remaining
                    Just (ReturnStmt retExpr, _) -> [StdChunk newChunk (RetTerm retExpr)] -- : chunkStmts remaining
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

isStructType :: Type -> Bool
isStructType (StructType {}) = True
isStructType _ = False

-- Meant for use for function paramenters. Is correct, but use exposeArrType first for a majority of other applications.
convertType :: Type -> QBEType
convertType IntType = Long
convertType FloatType = Double
convertType BoolType = Long
convertType CharType = Long
convertType (StructType name)= Aggregate name -- As a pointer 
convertType ArrayType {} = Long -- Also as a pointer
convertType _ = error "convertType: invalid type recieved, don't use on functions or voids"

getExprType :: Expr Typechecked -> Type
getExprType (Symbol _ sInfo) = symType sInfo
getExprType expr = eMeta expr

getStructName :: Type -> T.Text
getStructName (StructType sName) = sName
getStructName _ = error "getStructName: non-struct type passed in"

generateStructTemps :: M.Map Int Int -> [Instruction]
generateStructTemps maxMap = let pairedSizes = [ (key, [1..maxMap M.! key]) | key <- M.keys maxMap ]
                                 makeInst = (\key tempNum -> Alloc (".temp_" <> (T.pack . show) key <> "_" <> (T.pack . show) tempNum) key)
                                 tempInsts = map (\(key, tempNums) -> map (makeInst key) tempNums) pairedSizes
                             in  concat tempInsts

convertBinOp :: Op -> Type -> QBEOp
convertBinOp COMPGT FloatType = IRTypes.GT
convertBinOp COMPGT _ = IRTypes.SGT
convertBinOp COMPGE FloatType = IRTypes.GE
convertBinOp COMPGE _ = IRTypes.SGE
convertBinOp COMPLT FloatType = IRTypes.LT
convertBinOp COMPLT _ = IRTypes.SLT
convertBinOp COMPLE FloatType = IRTypes.LE
convertBinOp COMPLE _ = IRTypes.SLE
convertBinOp COMPEQ _ = IRTypes.EQ
convertBinOp COMPNEQ _ = IRTypes.NE
convertBinOp AST.ADD _ = IRTypes.ADD
convertBinOp AST.SUB _ = IRTypes.SUB
convertBinOp LOGOR _ = IRTypes.OR
convertBinOp BITOR _ = IRTypes.OR
convertBinOp MULT _ = IRTypes.MUL
convertBinOp AST.DIV _ = IRTypes.DIV
convertBinOp MOD _ = IRTypes.REM
convertBinOp LOGAND _ = IRTypes.AND
convertBinOp BITAND _ = IRTypes.AND
convertBinOp _ _ = error "convertBinOp: invalid binary operation provided"

generateLiteralStrings :: M.Map T.Text Ident -> [QBEIR]
generateLiteralStrings strMap = map generateLiteralString (M.toList strMap)
    where
        generateLiteralString :: (T.Text, Ident) -> QBEIR
        generateLiteralString (str, loc) = QBEIR [] [] [QBEStrDecl str loc]