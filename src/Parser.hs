{- HLINT ignore "Use lambda-case" -}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Lexer
import Data.Data (toConstr)

newtype Parser a = Parser {
    runParser :: [Token] -> Maybe (a, [Token])
}

data ParseTree =
    Leaf 

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



parseIdent :: Parser Token
parseIdent = parseToken anyIdent

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

parseInt :: Parser Token 
parseInt = parseToken anyInt 

parseFloat :: Parser Token 
parseFloat = parseToken anyFloat 

parseString :: Parser Token 
parseString = parseToken anyString 

parseChar :: Parser Token 
parseChar = parseToken anyChar 

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