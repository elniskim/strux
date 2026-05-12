{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE TypeFamilies #-}
module AST where 

import qualified Data.Text as T

-- Phase Tags
data Parsed
data Resolved 

type family XSymbol phase
type family XExpr   phase
type family XStmt   phase 
type family XDecl   phase

type instance XSymbol Parsed = ()
type instance XExpr   Parsed = ()
type instance XStmt   Parsed = ()
type instance XDecl   Parsed = ()

type instance XSymbol Resolved = SymbolInfo
type instance XExpr   Resolved = ()
type instance XStmt   Resolved = ()
type instance XDecl   Resolved = ()

data SymbolKind
    = LocalVar
    | GlobalVar
    | FunctionName
    | StructField
    | Parameter
    deriving (Show, Eq)

data SymbolInfo = SymbolInfo
    { symKind  :: SymbolKind
    , symType  :: Type
    , symDepth :: Int      -- scope depth, useful for IR gen later
    } deriving (Show, Eq)

data Program phase = Program {
    declList :: [Decl phase]
}

data Argument = Argument {
    argName :: T.Text,
    argType :: Type
}

data Decl phase
    = GlobalVarDecl   { globalName :: T.Text, globalType :: Type }
    | GlobalArrDecl   { globalArrDeclName :: T.Text, globalArrType :: Type }
    | FuncDef         { funcDeclName :: T.Text, returnType :: Type, args :: [Argument], funcBody :: [Stmt phase] }
    | StructDef       { structDeclName :: T.Text, attributes :: [Decl phase] }

data Stmt phase
    = LocalVarDecl   { localName :: T.Text, localType :: Type }
    | LocalArrDecl   { localArrDeclName :: T.Text, localArrType :: Type }
    | ExprStmt       { expression :: Expr phase }
    | IfStmt         { cond :: Expr phase, ifBlock :: [Stmt phase], elseBlock :: [Stmt phase] }
    | ForStmt        { initial :: Maybe (Expr phase), forCondition :: Maybe (Expr phase), increment :: Maybe (Expr phase), forBody :: [Stmt phase] }
    | WhileStmt      { whileCondition :: Expr phase, whileBody :: [Stmt phase] }
    | ReturnStmt     { retVal :: Maybe (Expr phase) }
    | BreakStmt
    | ContinueStmt

data Expr phase
    = BinaryExpr        { binaryOp :: Op, left :: Expr phase, right :: Expr phase, eMeta :: XExpr phase }
    | UnaryExpr         { unaryOp :: Op, right :: Expr phase, eMeta :: XExpr phase }
    | FunctionCall      { funcName :: Expr phase, arguments :: [Expr phase], eMeta :: XExpr phase }
    | ArrayIndex        { arrName :: Expr phase, index :: Expr phase, eMeta :: XExpr phase }
    | StructDeref       { structName :: Expr phase, fieldName :: T.Text, eMeta :: XExpr phase }
    | Symbol            { symbolName :: T.Text, sMeta :: XSymbol phase }
    | IntLiteral        { intVal :: Int, eMeta :: XExpr phase }
    | FloatLiteral      { floatVal :: Float, eMeta :: XExpr phase }
    | BoolLiteral       { boolVal :: Bool, eMeta :: XExpr phase }
    | CharLiteral       { charVal :: Char, eMeta :: XExpr phase }
    | StringLiteral     { strVal :: T.Text, eMeta :: XExpr phase }
    | GroupedExpression { inParens :: Expr phase, eMeta :: XExpr phase }

data Op
    = ASSIGN
    | COMPGT
    | COMPGE
    | COMPLT
    | COMPLE
    | COMPEQ
    | COMPNEQ
    | ADD
    | SUB -- this one is also allowed to be a unary operator, remember for semantics
    | LOGOR
    | BITOR
    | MULT
    | DIV
    | MOD
    | LOGAND
    | BITAND
    | UNARYNOT

data Type
    = IntType
    | FloatType
    | BoolType
    | StructType T.Text
    | ArrayType Int Type
    | VoidType
    deriving (Show, Eq)