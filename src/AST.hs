{- HLINT ignore "Use newtype instead of data" -}
module AST where 

import qualified Data.Text as T

data Program = Program {
    declList :: [Decl]
}

data Argument = Argument {
    argName :: T.Text,
    argType :: Type
}

data Decl
    = GlobalVarDecl   { globalName :: T.Text, globalType :: Type }
    | GlobalArrDecl   { globalArrDeclName :: T.Text, globalArrType :: Type }
    | FuncDef         { funcDeclName :: T.Text, returnType :: Type, args :: [Argument], funcBody :: [Stmt] }
    | StructDef       { structDeclName :: T.Text, attributes :: [Decl] }

data Stmt
    = LocalVarDecl   { localName :: T.Text, localType :: Type }
    | LocalArrDecl   { localArrDeclName :: T.Text, localArrType :: Type }
    | ExprStmt       { expression :: Expr }
    | IfStmt         { cond :: Expr, ifBlock :: [Stmt], elseBlock :: [Stmt] }
    | ForStmt        { initial :: Maybe Expr, forCondition :: Maybe Expr, increment :: Maybe Expr, forBody :: [Stmt] }
    | WhileStmt      { whileCondition :: Expr, whileBody :: [Stmt] }
    | ReturnStmt     { retVal :: Maybe Expr }
    | BreakStmt
    | ContinueStmt

data Expr
    = BinaryExpr        { binaryOp :: Op, left :: Expr, right :: Expr }
    | UnaryExpr         { unaryOp :: Op, right :: Expr }
    | FunctionCall      { funcName :: Expr, arguments :: [Expr] }
    | ArrayIndex        { arrName :: Expr, index :: Expr }
    | StructDeref       { structName :: Expr, fieldName :: T.Text }
    | Symbol            { symbolName :: T.Text }
    | IntLiteral        { intVal :: Int }
    | FloatLiteral      { floatVal :: Float }
    | BoolLiteral       { boolVal :: Bool }
    | CharLiteral       { charVal :: Char }
    | StringLiteral     { strVal :: T.Text }
    | GroupedExpression { inParens :: Expr }

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