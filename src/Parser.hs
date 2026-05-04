{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use newtype instead of data" -}
module Parser where

import Lexer
import Data.Data (toConstr)
import qualified Data.Text as T
import Data.List (uncons)
import Control.Applicative (many, some, optional, Alternative(..))

newtype Parser a = Parser {
    runParser :: [Token] -> Maybe (a, [Token])
}

instance Functor Parser where
    fmap f p = Parser $ \tokens ->
        let output = runParser p tokens
        in case output of
            Nothing          -> Nothing
            Just (val, rest) -> Just (f val, rest)

instance Applicative Parser where
    pure x = Parser $ \tokens -> Just (x, tokens)
    pf <*> px = Parser $ \tokens ->
        let output = runParser pf tokens
        in case output of
            Nothing -> Nothing
            Just (f, rest) -> runParser (fmap f px) rest

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \tokens ->
        case runParser p1 tokens of
            Just x  -> Just x
            Nothing -> runParser p2 tokens

instance Monad Parser where
    px >>= f = Parser $ \tokens ->
        let output = runParser px tokens
        in case output of
            Nothing -> Nothing
            Just (a, rest) -> runParser (f a) rest

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
    | StructDef       { strucDecltName :: T.Text, attributes :: [Decl] } 

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
    = Binary         { binaryOp :: BinOp, left :: Expr, right :: Expr }
    | Unary          { unaryOp :: UnaryOp, right :: Expr }
    | FunctionCall   { funcName :: Expr, arguments :: [Expr] }
    | ArrayIndex     { arrName :: Expr, index :: Expr }
    | StructDeref    { structName :: Expr, fieldName :: T.Text }
    | Primary        { atom :: Atom }

data Atom
    = Symbol            { symbolName :: T.Text, varType :: Type }
    | IntLiteral        { intVal :: Int }
    | FloatLiteral      { floatVal :: Float }
    | CharLiteral       { charVal :: Char }
    | StringLiteral     { strVal :: T.Text }
    | GroupedExpression { inParens :: Expr }

data BinOp
    = GT
    | GE
    | LT
    | LE
    | EQ
    | NEQ
    | ADD
    | SUB
    | LOGOR
    | BITOR
    | MULT
    | DIV
    | MOD
    | LOGAND
    | BITAND

data UnaryOp
    = NOT
    | NEG

data Type
    = IntType
    | FloatType
    | BoolType
    | StructType T.Text
    | ArrayType Int Type
    | VoidType
    deriving (Show, Eq)

-- Matches singleton types.
matchType :: T.Text -> Type
matchType t = case t of
    "int"   -> IntType
    "float" -> FloatType
    "bool"  -> BoolType
    _       -> StructType t

-- Factory for one token parsers.
satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = Parser $ \tokens -> case tokens of
    []     -> Nothing
    (t:ts) -> if predicate t
              then Just (t,ts)
              else Nothing

matchToken :: TokenType -> Token -> Bool
matchToken type1 token =
    let type2 = tokenType token
    in toConstr type1 == toConstr type2

parseStrux :: [Token] -> Maybe Program
parseStrux tokens =
    let output = runParser (many parseDecl) tokens
    in case output of
        Just (decls, []) -> Just (Program decls)
        _                -> Nothing

parseDecl :: Parser Decl
parseDecl = parseGlobalVarDecl <|> parseGlobalArrayDecl <|> parseFuncDef <|> parseStructDef

parseGlobalVarDecl :: Parser Decl
parseGlobalVarDecl = do
    n <- parseIdent
    _ <- parseColon
    t <- parseType
    _ <- parseSemi
    pure (GlobalVarDecl n t)

parseGlobalArrayDecl :: Parser Decl
parseGlobalArrayDecl = do
    n <- parseIdent
    _ <- parseSqBra
    e <- parseInt
    _ <- parseSqKet
    _ <- parseColon
    t <- parseType
    _ <- parseSemi
    pure (GlobalArrDecl n (ArrayType e t))

parseFuncDef :: Parser Decl
parseFuncDef = do
    _ <- parseDef
    n <- parseIdent
    _ <- parseOpenParen
    ps <- parseParamList
    _ <- parseCloseParen
    _ <- parseArrow
    t <- parseType
    ss <- parseStmtBlock
    pure (FuncDef n t ps ss)

parseParamList :: Parser [Argument]
parseParamList = parseParams <|> pure []
    where
        parseParams :: Parser [Argument]
        parseParams = do 
            first <- parseParam
            rest <- parseRest
            pure (first : rest)
        parseParam :: Parser Argument
        parseParam = do 
            n <- parseIdent
            _ <- parseColon 
            t <- parseType
            pure (Argument n t)
        parseRest :: Parser [Argument] 
        parseRest = many (parseComma *> parseParam)

parseStructDef :: Parser Decl
parseStructDef = do
    _ <- parseStruct
    n <- parseIdent
    _ <- parseCrBra
    ds <- many parseAttr
    _ <- parseCrKet
    pure (StructDef n ds)
    where
        parseAttr :: Parser Decl
        parseAttr = parseGlobalVarDecl <|> parseGlobalArrayDecl <|> parseStructDef

parseStmtBlock :: Parser [Stmt]
parseStmtBlock = do 
    _ <- parseCrBra
    ss <- many parseStmt 
    _ <- parseCrKet
    pure ss

parseStmt :: Parser Stmt 
parseStmt = parseLocalVarDecl <|> parseLocalArrDecl <|> parseExprStmt <|> parseIfStmt <|> 
    parseFor <|> parseWhile <|> parseReturn <|> parseBreak <|> parseContinue 

parseLocalVarDecl :: Parser Stmt 
parseLocalVarDecl = do 
    n <- parseIdent 
    _ <- parseColon 
    t <- parseType 
    _ <- parseSemi 
    pure (LocalVarDecl n t)

parseLocalArrDecl :: Parser Stmt 
parseLocalArrDecl = do 
    n <- parseIdent 
    _ <- parseSqBra
    e <- parseInt 
    _ <- parseSqKet 
    _ <- parseColon 
    t <- parseType 
    _ <- parseSemi 
    pure (LocalArrDecl n (ArrayType e t))

parseExprStmt :: Parser Stmt 
parseExprStmt = ExprStmt <$> parseExpr0

parseIfStmt :: Parser Stmt 
parseIfStmt = do 
    _ <- parseIf
    _ <- parseOpenParen 
    c <- parseExpr0 
    _ <- parseCloseParen 
    b1 <- parseStmtBlock 
    b2 <- parseElseBlock
    pure (IfStmt c b1 b2)
    where
        parseElseBlock :: Parser [Stmt]
        parseElseBlock = (parseElse *> parseStmtBlock) <|> pure []


-- TODO: Maybe find a way to make the unwrapping parsers nicer...
parseIdent :: Parser T.Text
parseIdent = Parser $ \tokens -> case uncons tokens of
    Just (Token (TokIdent ident) _, ts) -> Just (ident, ts)
    _                                   -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \tokens -> case uncons tokens of
    Just (Token (TokInt int) _, ts) -> Just (int, ts)
    _                               -> Nothing

parseFloat :: Parser Float
parseFloat = Parser $ \tokens -> case uncons tokens of
    Just (Token (TokFloat float) _, ts) -> Just (float, ts)
    _                                   -> Nothing

parseString :: Parser T.Text
parseString = Parser $ \tokens -> case uncons tokens of
    Just (Token (TokString string) _, ts) -> Just (string, ts)
    _                                     -> Nothing

parseChar :: Parser Char
parseChar = Parser $ \tokens -> case uncons tokens of
    Just (Token (TokChar char) _, ts) -> Just (char, ts)
    _                                 -> Nothing

parseType :: Parser Type
parseType = matchType <$> parseIdent

parseIf :: Parser Token
parseIf = parseToken TokIf

parseElse :: Parser Token
parseElse = parseToken TokElse

parseFor :: Parser Token
parseFor = parseToken TokFor

parseWhile :: Parser Token
parseWhile = parseToken TokWhile

parseDef :: Parser Token
parseDef = parseToken TokDef

parseStruct :: Parser Token
parseStruct = parseToken TokStruct

parseTrue :: Parser Token
parseTrue = parseToken TokTrue

parseFalse :: Parser Token
parseFalse = parseToken TokFalse

parseBreak :: Parser Token
parseBreak = parseToken TokBreak

parseContinue :: Parser Token
parseContinue = parseToken TokContinue

parseReturn :: Parser Token
parseReturn = parseToken TokReturn

parsePlus :: Parser Token
parsePlus = parseToken TokPlus

parseMinus :: Parser Token
parseMinus = parseToken TokMinus

parseMult :: Parser Token
parseMult = parseToken TokMult

parseDiv :: Parser Token
parseDiv = parseToken TokDiv

parseMod :: Parser Token
parseMod = parseToken TokMod

parseEq :: Parser Token
parseEq = parseToken TokEq

parseNEq :: Parser Token
parseNEq = parseToken TokNEq

parseGT :: Parser Token
parseGT = parseToken TokGT

parseGE :: Parser Token
parseGE = parseToken TokGE

parseLT :: Parser Token
parseLT = parseToken TokLT

parseLE :: Parser Token
parseLE = parseToken TokLE

parseAssign :: Parser Token
parseAssign = parseToken TokAssign

parseArrow :: Parser Token
parseArrow = parseToken TokArrow

parseBitAnd :: Parser Token
parseBitAnd = parseToken TokBitAnd

parseLogAnd :: Parser Token
parseLogAnd = parseToken TokLogAnd

parseBitOr :: Parser Token
parseBitOr = parseToken TokBitOr

parseNot :: Parser Token
parseNot = parseToken TokNot

parseSqBra :: Parser Token
parseSqBra = parseToken TokSqBra

parseSqKet :: Parser Token
parseSqKet = parseToken TokSqKet

parseCrBra :: Parser Token
parseCrBra = parseToken TokCrBra

parseCrKet :: Parser Token
parseCrKet = parseToken TokCrKet

parseOpenParen :: Parser Token
parseOpenParen = parseToken TokOpenParen

parseCloseParen :: Parser Token
parseCloseParen = parseToken TokCloseParen

parseColon :: Parser Token
parseColon = parseToken TokColon

parseSemi :: Parser Token
parseSemi = parseToken TokSemi

parseComma :: Parser Token
parseComma = parseToken TokComma

parseToken :: TokenType -> Parser Token
parseToken tokType = satisfy $ matchToken tokType

anyIdent :: TokenType
anyIdent = TokIdent ""

anyInt :: TokenType
anyInt = TokInt 0

anyFloat :: TokenType
anyFloat = TokFloat 0.0

anyString :: TokenType
anyString = TokString ""

anyChar :: TokenType
anyChar = TokChar '\0'
