{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- HLINT ignore "Use $>" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use newtype instead of data" -}
module Parser where

import Lexer (Token(..), TokenType(..))
import AST
import Data.Data (toConstr)
import qualified Data.Text as T
import Data.List (uncons)
import Data.Foldable (foldl')
import Control.Applicative (many, optional, Alternative(..))

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

parseStrux :: [Token] -> Maybe (Program Parsed)
parseStrux tokens =
    let output = runParser (many parseDecl) tokens
    in case output of
        Just (decls, []) -> Just (Program decls)
        _                -> Nothing

parseDecl :: Parser (Decl Parsed)
parseDecl = parseGlobalVarDecl <|> parseGlobalArrayDecl <|> parseFuncDef <|> parseStructDef

parseGlobalVarDecl :: Parser (Decl Parsed)
parseGlobalVarDecl = do
    n <- parseIdent
    _ <- parseColon
    t <- parseType
    _ <- parseSemi
    pure (GlobalVarDecl n t)

parseGlobalArrayDecl :: Parser (Decl Parsed)
parseGlobalArrayDecl = do
    n <- parseIdent
    _ <- parseColon
    t <- parseType
    _ <- parseSqBra
    e <- parseInt
    _ <- parseSqKet
    _ <- parseSemi
    pure (GlobalArrDecl n (ArrayType e t))

parseFuncDef :: Parser (Decl Parsed)
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

parseStructDef :: Parser (Decl Parsed)
parseStructDef = do
    _ <- parseStruct
    n <- parseIdent
    _ <- parseCrBra
    ds <- many parseAttr
    _ <- parseCrKet
    pure (StructDef n ds)
    where
        parseAttr :: Parser (Decl Parsed)
        parseAttr = parseGlobalVarDecl <|> parseGlobalArrayDecl <|> parseStructDef

parseStmtBlock :: Parser [Stmt Parsed]
parseStmtBlock = do
    _ <- parseCrBra
    ss <- many parseStmt
    _ <- parseCrKet
    pure ss

parseStmt :: Parser (Stmt Parsed)
parseStmt = parseLocalVarDecl <|> parseLocalArrDecl <|> parseExprStmt <|> parseIfStmt <|>
    parseForStmt <|> parseWhileStmt <|> parseReturnStmt <|> parseBreakStmt <|> parseContinueStmt

parseLocalVarDecl :: Parser (Stmt Parsed)
parseLocalVarDecl = do
    n <- parseIdent
    _ <- parseColon
    t <- parseType
    _ <- parseSemi
    pure (LocalVarDecl n t)

parseLocalArrDecl :: Parser (Stmt Parsed)
parseLocalArrDecl = do
    n <- parseIdent
    _ <- parseColon
    t <- parseType
    _ <- parseSqBra
    e <- parseInt
    _ <- parseSqKet
    _ <- parseSemi
    pure (LocalArrDecl n (ArrayType e t))

parseExprStmt :: Parser (Stmt Parsed)
parseExprStmt = ExprStmt <$> (parseExpr0 <* parseSemi)

parseIfStmt :: Parser (Stmt Parsed)
parseIfStmt = do
    _ <- parseIf
    _ <- parseOpenParen
    c <- parseExpr0
    _ <- parseCloseParen
    b1 <- parseStmtBlock
    b2 <- parseElseBlock
    pure (IfStmt c b1 b2)
    where
        parseElseBlock :: Parser [Stmt Parsed]
        parseElseBlock = (parseElse *> parseStmtBlock) <|> pure []

parseForStmt :: Parser (Stmt Parsed)
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
        parsePart :: Parser (Maybe (Expr Parsed))
        parsePart = (Just <$> parseExpr0 <* parseSemi) <|> (parseSemi *> pure Nothing)
        parsePart' :: Parser (Maybe (Expr Parsed))
        parsePart' = optional parseExpr0

parseWhileStmt :: Parser (Stmt Parsed)
parseWhileStmt = do
    _ <- parseWhile
    _ <- parseOpenParen
    c <- parseExpr0
    _ <- parseCloseParen
    ss <- parseStmtBlock
    pure (WhileStmt c ss)

parseReturnStmt :: Parser (Stmt Parsed)
parseReturnStmt = do
    _ <- parseReturn
    e <- (Just <$> parseExpr0 <* parseSemi) <|> (parseSemi *> pure Nothing)
    pure (ReturnStmt e)

parseBreakStmt :: Parser (Stmt Parsed)
parseBreakStmt = parseBreak *> parseSemi *> pure BreakStmt

parseContinueStmt :: Parser (Stmt Parsed)
parseContinueStmt = parseContinue *> parseSemi *> pure ContinueStmt

parseOpChain :: Parser Op -> Parser (Expr Parsed) -> Parser [(Op, Expr Parsed)]
parseOpChain opParser exprParser = many ((,) <$> opParser <*> exprParser)

chainExpr :: Expr Parsed -> (Op, Expr Parsed) -> Expr Parsed
chainExpr e1 (op, e2) = mkBinaryExpr op e1 e2

parseExpr0 :: Parser (Expr Parsed)
parseExpr0 = do
    lval  <- parseExpr1
    rval' <- optional (parseAssign *> parseExpr1)
    case rval' of
        Just rval -> pure $ mkBinaryExpr ASSIGN lval rval
        Nothing   -> pure lval

parseExpr1 :: Parser (Expr Parsed)
parseExpr1 = do
    root <- parseExpr2
    chain <- parseOpChain parseOp1 parseExpr2
    pure (foldl' chainExpr root chain)

parseOp1 :: Parser Op
parseOp1 = parseGE <|> parseLE <|> parseEq <|> parseNEq <|> parseGT <|> parseLT

parseExpr2 :: Parser (Expr Parsed)
parseExpr2 = do
    root <- parseExpr3
    chain <- parseOpChain parseOp2 parseExpr3
    pure (foldl' chainExpr root chain)

parseOp2 :: Parser Op
parseOp2 = parsePlus <|> parseMinus <|> parseLogOr <|> parseBitOr

parseExpr3 :: Parser (Expr Parsed)
parseExpr3 = do
    root <- parseExpr4
    chain <- parseOpChain parseOp3 parseExpr4
    pure (foldl' chainExpr root chain)

parseOp3 :: Parser Op
parseOp3 = parseMult <|> parseDiv <|> parseMod <|> parseLogAnd <|> parseBitAnd

parseExpr4 :: Parser (Expr Parsed)
parseExpr4 = parseUnaryNegate <|> parseUnaryNot <|> parseExpr5
    where
        parseUnaryNegate :: Parser (Expr Parsed)
        parseUnaryNegate = mkUnaryExpr <$> parseMinus <*> parseExpr4
        parseUnaryNot :: Parser (Expr Parsed)
        parseUnaryNot = mkUnaryExpr <$> parseNot <*> parseExpr4

parseExpr5 :: Parser (Expr Parsed)
parseExpr5 = do
    root <- parsePrimary
    chain <- many parsePostfix
    pure (foldl' chainPostfix root chain)
        where
            chainPostfix :: Expr Parsed -> (Expr Parsed -> Expr Parsed) -> Expr Parsed
            chainPostfix expr postfix = postfix expr

parsePostfix :: Parser (Expr Parsed -> Expr Parsed)
parsePostfix = parseArrayIndex <|> parseStructDeref <|> parseFuncCall

parseArrayIndex :: Parser (Expr Parsed -> Expr Parsed)
parseArrayIndex = do
    _ <- parseSqBra
    i <- parseExpr0
    _ <- parseSqKet
    pure (`mkArrayIndex` i)

parseStructDeref :: Parser (Expr Parsed -> Expr Parsed)
parseStructDeref = do
    _ <- parseArrow
    attr <- parseIdent
    pure (`mkStructDeref` attr)

parseFuncCall :: Parser (Expr Parsed -> Expr Parsed)
parseFuncCall = do
    _ <- parseOpenParen
    arglist <- parseExprList
    _ <- parseCloseParen
    pure (`mkFunctionCall` arglist)

parseExprList :: Parser [Expr Parsed]
parseExprList = parseExprs <|> pure []
    where
        parseExprs :: Parser [Expr Parsed]
        parseExprs = do
            first <- parseExpr0
            rest <- many (parseComma *> parseExpr0)
            pure (first : rest)

parsePrimary :: Parser (Expr Parsed)
parsePrimary = parseGroupedExpr <|> parseSymbol <|> parseLiteral

parseGroupedExpr :: Parser (Expr Parsed)
parseGroupedExpr = do
    _ <- parseOpenParen
    expr <- parseExpr0
    _ <- parseCloseParen
    pure (mkGroupedExpression expr)

parseSymbol :: Parser (Expr Parsed)
parseSymbol = mkSymbol <$> parseIdent

parseLiteral :: Parser (Expr Parsed)
parseLiteral = parseIntLiteral <|> parseFloatLiteral <|> parseBoolLiteral <|> parseCharLiteral <|> parseStringLiteral

parseIntLiteral :: Parser (Expr Parsed)
parseIntLiteral = mkIntLiteral <$> parseInt

parseFloatLiteral :: Parser (Expr Parsed)
parseFloatLiteral = mkFloatLiteral <$> parseFloat

parseBoolLiteral :: Parser (Expr Parsed)
parseBoolLiteral = (parseTrue *> pure (mkBoolLiteral True)) <|> (parseFalse *> pure (mkBoolLiteral False))

parseCharLiteral :: Parser (Expr Parsed)
parseCharLiteral = mkCharLiteral <$> parseChar

parseStringLiteral :: Parser (Expr Parsed)
parseStringLiteral = mkStringLiteral <$> parseString

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

mkBinaryExpr :: Op -> Expr Parsed -> Expr Parsed -> Expr Parsed
mkBinaryExpr op l r = BinaryExpr op l r ()

mkUnaryExpr :: Op -> Expr Parsed -> Expr Parsed
mkUnaryExpr op r = UnaryExpr op r ()

mkFunctionCall :: Expr Parsed -> [Expr Parsed] -> Expr Parsed
mkFunctionCall name exprs = FunctionCall name exprs ()

mkArrayIndex :: Expr Parsed -> Expr Parsed -> Expr Parsed
mkArrayIndex name i = ArrayIndex name i ()

mkStructDeref :: Expr Parsed -> T.Text -> Expr Parsed
mkStructDeref struct field = StructDeref struct field ()

mkSymbol :: T.Text -> Expr Parsed
mkSymbol name = Symbol name ()

mkIntLiteral :: Int -> Expr Parsed
mkIntLiteral val = IntLiteral val ()

mkFloatLiteral :: Float -> Expr Parsed
mkFloatLiteral val = FloatLiteral val ()

mkBoolLiteral :: Bool -> Expr Parsed
mkBoolLiteral val = BoolLiteral val ()

mkCharLiteral :: Char -> Expr Parsed
mkCharLiteral val = CharLiteral val ()

mkStringLiteral :: T.Text -> Expr Parsed
mkStringLiteral val = StringLiteral val ()

mkGroupedExpression :: Expr Parsed -> Expr Parsed
mkGroupedExpression expr = GroupedExpression expr ()

