{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE TypeFamilies #-}
module AST where 

import qualified Data.Text as T

-- Phase Tags
data Parsed
data Resolved 
data Typechecked

type family XSymbol phase
type family XExpr   phase
type family XStmt   phase 
type family XDecl   phase
type family XAttr   phase
type family XArg    phase
type family XLocal  phase

type instance XSymbol Parsed = ()
type instance XExpr   Parsed = ()
type instance XStmt   Parsed = ()
type instance XDecl   Parsed = ()
type instance XAttr   Parsed = Decl Parsed
type instance XArg    Parsed = ()
type instance XLocal  Parsed = ()

type instance XSymbol Resolved = ResolvedInfo
type instance XExpr   Resolved = ()
type instance XStmt   Resolved = ()
type instance XDecl   Resolved = ()
type instance XAttr   Resolved = StructField Resolved 
type instance XArg    Resolved = SymbolId
type instance XLocal  Resolved = SymbolId

type instance XSymbol Typechecked = ResolvedInfo
type instance XExpr   Typechecked = Type
type instance XStmt   Typechecked = ()
type instance XDecl   Typechecked = ()
type instance XAttr   Typechecked = StructField Typechecked 
type instance XArg    Typechecked = SymbolId
type instance XLocal  Typechecked = SymbolId

data SymbolKind
    = LocalVar
    | GlobalVar
    | FunctionName
    | StructField
    | Parameter
    deriving (Show, Eq)

type SymbolId = Int

data ResolvedInfo = ResolvedInfo { 
    symKind  :: SymbolKind,
    symType  :: Type,
    symId :: SymbolId
} deriving (Show, Eq)

data Program phase = Program {
    declList :: [Decl phase]
}

data Argument phase = Argument {
    argName :: T.Text,
    argType :: Type,
    argId :: XArg phase
}

data Decl phase
    = GlobalVarDecl   { globalName :: T.Text, globalType :: Type }
    | GlobalArrDecl   { globalArrDeclName :: T.Text, globalArrType :: Type }
    | FuncDef         { funcDeclName :: T.Text, returnType :: Type, args :: [Argument phase], funcBody :: [Stmt phase] }
    | StructDef       { structDeclName :: T.Text, attributes :: [XAttr phase] }

data StructField phase
    = Scalar          { scalarName :: T.Text, scalarType :: Type }
    | Vector          { vectorName :: T.Text, vectorType :: Type }
    | Struct          { structureName :: T.Text, structAttrs :: [StructField phase] }

data Stmt phase
    = LocalVarDecl   { localName :: T.Text, localType :: Type, varId :: XLocal phase }
    | LocalArrDecl   { localArrDeclName :: T.Text, localArrType :: Type, arrId :: XLocal phase }
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
    | FunctionCall      { funcName :: T.Text, arguments :: [Expr phase], eMeta :: XExpr phase }
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
    deriving (Show, Eq)

data Type
    = IntType
    | FloatType
    | BoolType
    | CharType
    | StructType T.Text
    | ArrayType Int Type
    | FuncType Type [Type]
    | VoidType
    | ErrType -- Used for poison in the typechecker
    deriving (Show, Eq)
