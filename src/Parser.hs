{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use $>" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use newtype instead of data" -}
module Parser where

import Lexer
import Data.Data (toConstr)
import qualified Data.Text as T
import Data.List (uncons)
import Data.Foldable (foldl')
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
    = BinaryExpr        { binaryOp :: Op, left :: Expr, right :: Expr }
    | UnaryExpr         { unaryOp :: Op, right :: Expr }
    | FunctionCall      { funcName :: Expr, arguments :: [Expr] }
    | ArrayIndex        { arrName :: Expr, index :: Expr }
    | StructDeref       { structName :: Expr, fieldName :: T.Text }
    | Primary           { atom :: Atom }
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
    | UNARYNEG

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
    parseForStmt <|> parseWhileStmt <|> parseReturnStmt <|> parseBreakStmt <|> parseContinueStmt

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
parseExprStmt = ExprStmt <$> (parseExpr0 <* parseSemi)

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

parseForStmt :: Parser Stmt
parseForStmt = do
    _ <- parseFor
    _ <- parseOpenParen
    forInit <- parsePart
    forCond <- parsePart
    forIncr <- parsePart'
    _ <- parseCloseParen
    ss <- parseStmtBlock
    pure (ForStmt forInit forCond forIncr ss)
    where
        parsePart :: Parser (Maybe Expr)
        parsePart = (Just <$> parseExpr0 <* parseSemi) <|> (parseSemi *> pure Nothing)
        parsePart' :: Parser (Maybe Expr)
        parsePart' = (Just <$> parseExpr0 <* parseSemi) <|> pure Nothing

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
    _ <- parseWhile
    _ <- parseOpenParen
    c <- parseExpr0
    _ <- parseCloseParen
    ss <- parseStmtBlock
    pure (WhileStmt c ss)

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    _ <- parseReturn
    e <- (Just <$> parseExpr0 <* parseSemi) <|> (parseSemi *> pure Nothing)
    pure (ReturnStmt e)

parseBreakStmt :: Parser Stmt
parseBreakStmt = parseBreak *> pure ContinueStmt

parseContinueStmt :: Parser Stmt
parseContinueStmt = parseContinue *> pure ContinueStmt

parseOpChain :: Parser Op -> Parser Expr -> Parser [(Op, Expr)]
parseOpChain opParser exprParser = many ((,) <$> opParser <*> exprParser)

chainExpr :: Expr -> (Op, Expr) -> Expr
chainExpr e1 (op, e2) = BinaryExpr op e1 e2

parseExpr0 :: Parser Expr
parseExpr0 = do
    lval  <- parseExpr1
    rval' <- optional (parseAssign *> parseExpr1)
    case rval' of
        Just rval -> pure $ BinaryExpr ASSIGN lval rval
        Nothing   -> pure lval

parseExpr1 :: Parser Expr
parseExpr1 = do
    root <- parseExpr2
    chain <- parseOpChain parseOp1 parseExpr2
    pure (foldl' chainExpr root chain)

parseOp1 :: Parser Op
parseOp1 = parseGE <|> parseLE <|> parseEq <|> parseNEq <|> parseGT <|> parseLT

parseExpr2 :: Parser Expr
parseExpr2 = do
    root <- parseExpr3
    chain <- parseOpChain parseOp2 parseExpr3
    pure (foldl' chainExpr root chain)

parseOp2 :: Parser Op
parseOp2 = parsePlus <|> parseMinus <|> parseLogOr <|> parseBitOr

parseExpr3 :: Parser Expr
parseExpr3 = do
    root <- parseExpr4
    chain <- parseOpChain parseOp3 parseExpr4
    pure (foldl' chainExpr root chain)

parseOp3 :: Parser Op
parseOp3 = parseMult <|> parseDiv <|> parseMod <|> parseLogAnd <|> parseBitAnd

parseExpr4 :: Parser Expr
parseExpr4 = parseUnaryNegate <|> parseUnaryNot <|> parseExpr5
    where
        parseUnaryNegate :: Parser Expr
        parseUnaryNegate = UnaryExpr <$> parseMinus <*> parseExpr4
        parseUnaryNot :: Parser Expr
        parseUnaryNot = UnaryExpr <$> parseNot <*> parseExpr4

parseExpr5 :: Parser Expr
parseExpr5 = do
    root <- parsePrimary
    chain <- many parsePostfix
    pure (foldl' chainPostfix root chain)
        where
            chainPostfix :: Expr -> (Expr -> Expr) -> Expr
            chainPostfix expr postfix = postfix expr

parsePostfix :: Parser (Expr -> Expr)
parsePostfix = parseArrayIndex <|> parseStructDeref <|> parseFuncCall

parseArrayIndex :: Parser (Expr -> Expr)
parseArrayIndex = do
    _ <- parseSqBra
    i <- parseExpr0
    _ <- parseSqKet
    pure (`ArrayIndex` i)

parseStructDeref :: Parser (Expr -> Expr)
parseStructDeref = do 
    _ <- parseArrow
    attr <- parseIdent 
    pure (`StructDeref` attr)

parseFuncCall :: Parser (Expr -> Expr)
parseFuncCall = do 
    _ <- parseOpenParen 
    arglist <- parseExprList 
    _ <- parseCloseParen
    pure (`FunctionCall` arglist)

parseExprList :: Parser [Expr]
parseExprList = parseExprs <|> pure []
    where 
        parseExprs :: Parser [Expr]
        parseExprs = do 
            first <- parseExpr0
            rest <- many (parseComma *> parseExpr0)
            pure(first : rest)

parsePrimary :: Parser Expr
parsePrimary = parseGroupedExpr <|> parseSymbol <|> parseLiteral 

parseGroupedExpr :: Parser Expr 
parseGroupedExpr = do 
    _ <- parseOpenParen 
    expr <- parseExpr0 
    _ <- parseCloseParen 
    pure (GroupedExpression expr)

parseSymbol :: Parser Expr
parseSymbol = Symbol <$> parseIdent

parseLiteral :: Parser Expr
parseLiteral = parseIntLiteral <|> parseFloatLiteral <|> parseBoolLiteral <|> parseCharLiteral <|> parseStringLiteral

parseIntLiteral :: Parser Expr
parseIntLiteral = IntLiteral <$> parseInt 

parseFloatLiteral :: Parser Expr
parseFloatLiteral = FloatLiteral <$> parseFloat 

parseBoolLiteral :: Parser Expr
parseBoolLiteral = (parseTrue *> pure (BoolLiteral True)) <|> (parseFalse *> pure (BoolLiteral False))

parseCharLiteral :: Parser Expr 
parseCharLiteral = CharLiteral <$> parseChar 

parseStringLiteral :: Parser Expr 
parseStringLiteral = StringLiteral <$> parseString

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

parsePlus :: Parser Op
parsePlus = parseToken TokPlus *> pure ADD

parseMinus :: Parser Op
parseMinus = parseToken TokMinus *> pure SUB

parseMult :: Parser Op
parseMult = parseToken TokMult *> pure MULT

parseDiv :: Parser Op
parseDiv = parseToken TokDiv *> pure DIV

parseMod :: Parser Op
parseMod = parseToken TokMod *> pure MOD

parseEq :: Parser Op
parseEq = parseToken TokEq *> pure COMPEQ

parseNEq :: Parser Op
parseNEq = parseToken TokNEq *> pure COMPNEQ

parseGT :: Parser Op
parseGT = parseToken TokGT *> pure COMPGT

parseGE :: Parser Op
parseGE = parseToken TokGE *> pure COMPGE

parseLT :: Parser Op
parseLT = parseToken TokLT *> pure COMPLT

parseLE :: Parser Op
parseLE = parseToken TokLE *> pure COMPLE

parseAssign :: Parser Op
parseAssign = parseToken TokAssign *> pure ASSIGN

parseArrow :: Parser Token
parseArrow = parseToken TokArrow

parseBitAnd :: Parser Op
parseBitAnd = parseToken TokBitAnd *> pure BITAND

parseLogAnd :: Parser Op
parseLogAnd = parseToken TokLogAnd *> pure LOGAND

parseBitOr :: Parser Op
parseBitOr = parseToken TokBitOr *> pure BITOR

parseLogOr :: Parser Op
parseLogOr = parseToken TokLogOr *> pure LOGOR

parseNot :: Parser Op
parseNot = parseToken TokNot *> pure UNARYNOT

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
