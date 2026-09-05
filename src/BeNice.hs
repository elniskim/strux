{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant if" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Redundant where" -}
module BeNice where
-- Check that all global constants can be defined at compile time? Move to a new structure. New layer of the AST.
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import AST
import Pretty

pgrmChecks :: [Check (Program Typechecked)]
pgrmChecks = [recursiveStructCheck, mainExistsCheck]

declChecks :: [Check (Decl Typechecked)]
declChecks = [globalArrSizeCheck, allPathsReturnCheck, duplicateFieldCheck, globalDeclResolutionCheck]

stmtChecks :: [Check (Stmt Typechecked)]
stmtChecks = [localArrSizeCheck, nakedJumpCheck, loneSymbolCheck]

exprChecks :: [Check (Expr Typechecked)]
exprChecks = [divisionByZeroCheck, lValCheck]

recursiveStructCheck :: Check (Program Typechecked)
recursiveStructCheck (Program decls) = do
    case findRecursiveStructs decls of
        Nothing -> return ()
        Just names -> tell ["Struct recursion along contain stack " <> T.pack (show names) <> "."]

mainExistsCheck :: Check (Program Typechecked)
mainExistsCheck (Program decls) = do
    let check = any isMain decls
    if check then return () else tell ["No function named \"main\" in program."]
        where
            isMain :: Decl Typechecked -> Bool
            isMain (FuncDef "main" _ _ _) = True
            isMain _ = False

duplicateFieldCheck :: Check (Decl Typechecked)
duplicateFieldCheck (StructDef name attrs) = do
    let attrNames = fmap getFieldName attrs
    case findDuplicate attrNames of
        Just dup -> tell ["Duplicate struct field name " <> dup <> " found in struct " <> name <> "."]
        Nothing -> return ()
duplicateFieldCheck _ = return ()

allPathsReturnCheck :: Check (Decl Typechecked)
allPathsReturnCheck (FuncDef name _ _ body) = if funcReturns body then return () else tell ["Function " <> name <> " does not return on all paths."]
    where
        funcReturns :: [Stmt Typechecked] -> Bool
        funcReturns (s:ss)
            | (ReturnStmt _) <- s = True
            | (IfStmt _ b1 b2) <- s = (funcReturns b1 && funcReturns b2) || funcReturns ss
            | otherwise = funcReturns ss
        funcReturns [] = False
allPathsReturnCheck _ = return ()

globalArrSizeCheck :: Check (Decl Typechecked)
globalArrSizeCheck (GlobalArrDecl name (ArrayType size _))
    | size > 0 = return ()
    | otherwise = tell ["Global array " <> name <> " is defined with size less than/equal to zero."]
globalArrSizeCheck _ = return ()

localArrSizeCheck :: Check (Stmt Typechecked)
localArrSizeCheck (LocalArrDecl name (ArrayType size _))
    | size > 0 = pure ()
    | otherwise = tell ["Local array " <> name <> " is defined with size less than/equal to zero."]
localArrSizeCheck _ = return ()

nakedJumpCheck :: Check (Stmt Typechecked)
nakedJumpCheck BreakStmt = do
    val <- asks inLoop
    if val
        then pure ()
        else tell ["Naked break statement."]
nakedJumpCheck ContinueStmt = do
    val <- asks inLoop
    if val
        then pure ()
        else tell ["Naked break statement."]
nakedJumpCheck _ = return ()

loneSymbolCheck :: Check (Stmt Typechecked)
loneSymbolCheck (ExprStmt (Symbol name _)) = tell ["Lone symbol " <> name <> " has value discarded."]
loneSymbolCheck _ = return ()

divisionByZeroCheck :: Check (Expr Typechecked)
divisionByZeroCheck (BinaryExpr DIV _ (IntLiteral 0 _) _) = tell ["Literal division by zero."]
divisionByZeroCheck (BinaryExpr DIV _ (FloatLiteral 0.0 _) _) = tell ["Literal division by zero."]
divisionByZeroCheck _ = return ()

lValCheck :: Check(Expr Typechecked)
lValCheck asgn@(BinaryExpr ASSIGN leftExpr _ _) = if isLVal leftExpr then return () else tell ["Invalid l-value in assignment " <> pretty 0 asgn]
    where
        isLVal :: Expr Typechecked -> Bool
        isLVal (Symbol {}) = True
        isLVal (ArrayIndex {}) = True -- Typechecker guarantees that the array index and the struct deref are valid. All that matters is that there can be a properly resolved location that it refers to.
        isLVal (StructDeref {}) = True
        isLVal _ = False
lValCheck _ = return ()



data CheckerState = CheckerState {
    inLoop :: Bool
}

startState :: CheckerState
startState = CheckerState False

setInLoop :: CheckerState -> CheckerState
setInLoop cs = cs {inLoop = True}

type Checker = ReaderT CheckerState (Writer [T.Text]) ()
type Check a = a -> Checker

runChecks :: [Check a] -> a -> Checker
runChecks checks node = mapM_ ($ node) checks

runChecker :: Checker -> [T.Text]
runChecker checker = execWriter $ runReaderT checker startState



checkStrux :: Program Typechecked -> [T.Text]
checkStrux pgrm = runChecker $ checkProgram pgrm

checkProgram :: Program Typechecked -> Checker
checkProgram pgrm = mapM_ checkDecl $ declList pgrm

checkDecl :: Decl Typechecked -> Checker
checkDecl func@(FuncDef _ _ _ body) = do
    mapM_ checkStmt body
    runChecks declChecks func
checkDecl decl = runChecks declChecks decl

checkStmt :: Stmt Typechecked -> Checker
checkStmt stmt@(ExprStmt expr) = do
    checkExpr expr
    runChecks stmtChecks stmt
checkStmt stmt@(IfStmt condition block1 block2) = do
    checkExpr condition
    mapM_ checkStmt block1
    mapM_ checkStmt block2
    runChecks stmtChecks stmt
checkStmt stmt@(ForStmt initExpr condExpr incrExpr body) = do
    mapM_ checkExpr initExpr
    mapM_ checkExpr condExpr
    mapM_ checkExpr incrExpr
    local setInLoop (mapM_ checkStmt body)
    runChecks stmtChecks stmt
checkStmt stmt@(WhileStmt condExpr body) = do
    checkExpr condExpr
    local setInLoop (mapM_ checkStmt body)
    runChecks stmtChecks stmt
checkStmt stmt@(ReturnStmt ret) = do
    mapM_ checkExpr ret
    runChecks stmtChecks stmt
checkStmt stmt = runChecks stmtChecks stmt

checkExpr :: Expr Typechecked -> Checker
checkExpr expr@(BinaryExpr _ leftExpr rightExpr _) = do
    checkExpr leftExpr
    checkExpr rightExpr
    runChecks exprChecks expr
checkExpr expr@(UnaryExpr _ rightExpr _) = do
    checkExpr rightExpr
    runChecks exprChecks expr
checkExpr expr@(FunctionCall _ argList _) = do
    mapM_ checkExpr argList
    runChecks exprChecks expr
checkExpr expr@(ArrayIndex arr idx _) = do
    checkExpr arr
    checkExpr idx
    runChecks exprChecks expr
checkExpr expr@(StructDeref struct _ _) = do
    checkExpr struct
    runChecks exprChecks expr
checkExpr expr@(GroupedExpression parens _) = do
    checkExpr parens
    runChecks exprChecks expr
checkExpr expr = runChecks exprChecks expr





findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate lst = go lst S.empty
    where
        go :: Ord a => [a] -> S.Set a -> Maybe a
        go (x:xs) checked = if S.member x checked then Just x else go xs (S.insert x checked)
        go [] _ = Nothing

getFieldName :: StructField Typechecked -> T.Text
getFieldName (Scalar name _) = name
getFieldName (Vector name _) = name
getFieldName (Struct name _) = name

isStruct :: Decl Typechecked -> Bool
isStruct (StructDef _ _) = True
isStruct _ = False

isStructField :: StructField Typechecked -> Bool
isStructField (Struct _ _) = True
isStructField _ = False

findRecursiveStructs :: [Decl Typechecked] -> Maybe [T.Text]
findRecursiveStructs decls = evalState (findM go nodeNames) S.empty
    where
        structGraph = makeStructGraph decls
        nodeNames = Map.keys structGraph
        go :: T.Text -> State (S.Set T.Text) (Maybe [T.Text])
        go node = do
            modify (S.insert node)
            visited <- get
            case Map.lookup node structGraph of
                Just neighbors -> if any (\neighbor -> S.member neighbor visited) neighbors
                                  then return $ Just [node]
                                  else do {res <- findM go neighbors; return $ fmap (node :) res}
                Nothing -> error $ "struct " <> T.unpack node <> " not found in the map of structs"


makeStructGraph :: [Decl Typechecked] -> Map.Map T.Text [T.Text]
makeStructGraph decls =
    let structs = filter isStruct decls
    in Map.fromList $ fmap toNode structs
        where
            toNode :: Decl Typechecked -> (T.Text, [T.Text])
            toNode (StructDef name attrs) = (name, getSAttrNames attrs)
            toNode _ = error "toNode: non-struct found"
            getSAttrNames :: [StructField Typechecked] -> [T.Text]
            getSAttrNames attrs = fmap structureName (filter isStructField attrs)

findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM _ [] = return Nothing
findM action (x:xs) = do
    result <- action x
    case result of
        Nothing -> findM action xs
        Just val -> return $ Just val