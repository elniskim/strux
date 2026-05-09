{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import AST
import qualified Data.Text as T

indent :: Int -> T.Text
indent n = T.replicate n "\t"

maybePretty :: (Pretty a) => Int -> Maybe a -> T.Text
maybePretty i possible = case possible of { Nothing -> T.replicate i "\t"; Just expr -> pretty i expr }

class Pretty a where
    pretty :: Int -> a -> T.Text

instance Pretty Program where
    pretty _ Program { declList = decls } = T.intercalate "\n\n" (map (pretty 0) decls)

instance Pretty Argument where
    pretty _ Argument { argName = name, argType = paramType } = name <> ": " <> pretty 0 paramType

instance Pretty Decl where
    pretty i GlobalVarDecl { globalName = name, globalType = varType } =
        indent i <> name <> ": " <> pretty 0 varType <> ";"
    pretty i GlobalArrDecl { globalArrDeclName = name, globalArrType = arrType } =
        indent i <> name <> ": " <> pretty 0 arrType <> ";"
    pretty i FuncDef { funcDeclName = name, returnType = retType, args = argList, funcBody = stmts } =
        indent i <> "def " <> name <> "(" <> T.intercalate ", " (map (pretty 0) argList) <> ") -> " <> pretty 0 retType <> " {\n" <>
            T.intercalate "\n" (map (pretty (i+1)) stmts) <> indent i <> "\n}"
    pretty i StructDef { structDeclName = name, attributes = attrs } =
        indent i <> "struct " <> name <> " {\n" <> T.intercalate "\n" (map (pretty (i+1)) attrs) <> "\n" <> indent i <> "}"

instance Pretty Stmt where
    pretty i LocalVarDecl { localName = name, localType = varType } = 
        indent i <> name <> ": " <> pretty 0 varType <> ";"
    pretty i LocalArrDecl { localArrDeclName = name, localArrType = arrType } = 
        indent i <> name <> ": " <> pretty 0 arrType <> ";"
    pretty i ExprStmt { expression = expr } = 
        indent i <> pretty 0 expr <> ";"
    pretty i IfStmt { cond = condition, ifBlock = b1, elseBlock = [] } = 
        indent i <> "if (" <> pretty 0 condition <> ") {\n" <> T.intercalate "\n" (map (pretty (i+1)) b1) <> indent i <> "}"
    pretty i IfStmt { cond = condition, ifBlock = b1, elseBlock = b2 } = 
        indent i <> "if (" <> pretty 0 condition <> ") {\n" <> T.intercalate "\n" (map (pretty (i+1)) b1) <> indent i <> "\n" <>
            indent i <> "} else {\n" <> T.intercalate "\n" (map (pretty (i+1)) b2) <> "\n" <> indent i <> "}"
    pretty i ForStmt { initial = first, forCondition = condition, increment = incr, forBody = stmts } = 
        indent i <> "for (" <> maybePretty 0 first <> "; " <> maybePretty 0 condition <> "; " <> maybePretty 0 incr <> ") {\n" <>
            T.intercalate "\n" (map (pretty (i+1)) stmts) <> "\n" <> indent i <> "}"
    pretty i WhileStmt { whileCondition = condition, whileBody = stmts } = 
        indent i <> "while (" <> pretty 0 condition <> ") {\n" <> T.intercalate "\n" (map (pretty (i+1)) stmts) <> indent i <> "\n" <> indent i <> "}"
    pretty i ReturnStmt { retVal = Nothing } = indent i <> "return;"
    pretty i ReturnStmt { retVal = (Just expr) } = indent i <> "return " <> pretty (i+1) expr <> ";"
    pretty i BreakStmt = indent i <> "break;"
    pretty i ContinueStmt = indent i <> "continue;"

instance Pretty Expr where 
    pretty _ BinaryExpr { binaryOp = binOp, left = e1, right = e2 } = 
        "(" <> pretty 0 e1 <> " " <> pretty 0 binOp <> " " <> pretty 0 e2 <> ")"
    pretty _ UnaryExpr { unaryOp = unOp, right = expr } = 
        "(" <> pretty 0 unOp <> pretty 0 expr <> ")"
    pretty _ FunctionCall { funcName = name, arguments = funcArgs } = 
        "(" <>pretty 0 name <> "(" <> T.intercalate ", " (map (pretty 0) funcArgs) <> "))" 
    pretty _ ArrayIndex { arrName = name, index = expr } = 
        "(" <> pretty 0 name <> "[" <> pretty 0 expr <> "]" <> ")"
    pretty _ StructDeref { structName = sname, fieldName = fname } = 
        "(" <> pretty 0 sname <> "->" <> fname <> ")"
    pretty _ Symbol { symbolName = name } = name
    pretty _ IntLiteral { intVal = val } = T.pack (show val) 
    pretty _ FloatLiteral { floatVal = val } = T.pack (show val)
    pretty _ BoolLiteral { boolVal = val } = T.pack (show val)
    pretty _ CharLiteral { charVal = val } = T.pack (show val)
    pretty _ StringLiteral { strVal = val } = "\"" <> val <> "\"" 
    pretty _ GroupedExpression { inParens = expr } = "(" <> pretty 0 expr <> ")"

instance Pretty Op where 
    pretty _ ASSIGN = "="
    pretty _ COMPGT = ">"
    pretty _ COMPGE = ">="
    pretty _ COMPLT = "<"
    pretty _ COMPLE = "<="
    pretty _ COMPEQ = "=="
    pretty _ COMPNEQ = "!="
    pretty _ ADD = "+"
    pretty _ SUB = "-"
    pretty _ LOGOR = "||"
    pretty _ BITOR = "|"
    pretty _ MULT = "*"
    pretty _ DIV = "/"
    pretty _ MOD = "%"
    pretty _ LOGAND = "&&"
    pretty _ BITAND = "&"
    pretty _ UNARYNOT = "!"

instance Pretty Type where 
    pretty _ IntType = "int"
    pretty _ FloatType = "float" 
    pretty _ BoolType = "bool"
    pretty _ (StructType name) = name
    pretty _ (ArrayType extent base) = pretty 0 base <> "[" <> T.pack (show extent) <> "]"
    pretty _ VoidType = "void" 
