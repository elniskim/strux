{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Char (isLetter, isAlphaNum, isNumber, readLitChar)
import qualified Data.Set as Set
import qualified Data.Map as Map --Consider HashMap instead soon?

opChars :: Set.Set Char
opChars = Set.fromList ['<', '>', '=', '+', '%', '-', '*', '/', '!', '&', '|', '[', ']', '(', ')', '{', '}', ';', ':', ',']

data FPToken
    = WordToken     { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | NumberLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | StringLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | CharLiteral   { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Operator      { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    -- | Comments      { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    -- | Whitespace    { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    deriving (Show)


data LexerState = LexerState {
    remaining :: T.Text,
    stateLine :: Int,
    stateColumn :: Int
}

main :: IO ()
main = do
    putStrLn "Hello world!"

firstPass :: T.Text -> [FPToken]
firstPass input = go $ LexerState input 1 1
    where
        go :: LexerState -> [FPToken]
        go state@(LexerState text line column)
            | Nothing <- u
            = []

            | Just(c,_) <- u
            , inNumber c
            = let (token, rest) = T.span inNumber text in NumberLiteral token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , wordStart c
            = let (token, rest) = T.span inWord text in WordToken token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , stringStart c
            = let (token, rest) = munchString text in StringLiteral token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , charStart c
            = let (token, rest) = munchChar text in CharLiteral token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , opStart c
            = let (token, rest) = T.span opStart text in Operator token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , isWhitespace c
            = let (token, rest) = T.span opStart text in go (updateLexerState state token rest) -- Whitespace isn't a token.

            | Just(c,_) <- u
            , c == '#'
            = let (token, rest) = T.span (/= '\n') text in go (updateLexerState state token rest) -- Comments aren't a token.

            | otherwise = error $ "Illegal character" ++ "at line " ++ show line ++ "column " ++ show column

                where u = T.uncons text

updateLexerState :: LexerState -> T.Text -> T.Text -> LexerState
updateLexerState state token newRemaining =
    let foldedState = T.foldl' update state token
    in foldedState { remaining = newRemaining }
        where update :: LexerState -> Char -> LexerState
              update (LexerState r line col) char
                | char == '\n' = LexerState r (line + 1) 1
                | otherwise = LexerState r line (col + 1)

inNumber :: Char -> Bool
inNumber c = isNumber c || c == '.'

wordStart :: Char -> Bool
wordStart c = isLetter c || c == '_'

inWord :: Char -> Bool
inWord c = isAlphaNum c || c == '_'

stringStart :: Char -> Bool
stringStart c = c == '\"'

munchString :: T.Text -> (T.Text, T.Text)
munchString file =
    let numChars = findLen (T.tail file) 0 -- Get rid of the opening quote.
    in T.splitAt numChars file
        where findLen :: T.Text -> Int -> Int
              findLen text _
                | T.null text = error "EOF while parsing string."
              findLen text numChars
                | c == '\\' = findLen (T.drop 2 text) (numChars + 2)
                | c == '"' = numChars + 2
                | otherwise = findLen (T.tail text) (numChars + 1)
                    where c = T.head text

charStart :: Char -> Bool
charStart c = c == '\''

-- Needs format checking on second pass. For SURE.
munchChar :: T.Text -> (T.Text, T.Text)
munchChar file =
    let numChars = findLen (T.tail file) 0
    in T.splitAt numChars file
        where findLen :: T.Text -> Int -> Int
              findLen text _
                | T.null text = error "EOF while parsing character."
              findLen text numChars
                | c == '\\' = findLen (T.drop 2 text) (numChars + 2)
                | c == '\'' = numChars + 2
                | otherwise = findLen (T.tail text) (numChars + 1)
                    where c = T.head text

opStart :: Char -> Bool
opStart c = Set.member c opChars

commentStart :: Char -> Bool
commentStart c = c == '#'

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\n', '\t', '\r']





data TokenType
    = TokIdent T.Text
    | TokIf
    | TokElse
    | TokFor
    | TokWhile
    | TokDef
    | TokStruct
    | TokTrue
    | TokFalse
    | TokBreak
    | TokContinue
    | TokReturn
    | TokInt Int
    | TokFloat Float
    | TokString T.Text
    | TokChar Char
    | TokPlus
    | TokMinus
    | TokMult
    | TokDiv
    | TokMod
    | TokEq
    | TokNEq
    | TokGT
    | TokGE
    | TokLT
    | TokLE
    | TokAssign
    | TokArrow
    | TokBitAnd
    | TokLogAnd
    | TokBitOr
    | TokNot
    | TokSqBra
    | TokSqKet
    | TokCrBra
    | TokCrKet
    | TokOpenParen
    | TokCloseParen
    | TokColon
    | TokSemi
    | TokComma

data SrcLoc = SrcLoc {
    line :: !Int,
    col  :: !Int
} deriving (Show, Eq)

data Token = Token {
    token :: TokenType,
    loc :: SrcLoc
}

secondPass :: [FPToken] -> [Token]
secondPass = map disambiguate

disambiguate :: FPToken -> Token
disambiguate (WordToken text line col) = disambiguateWord text line col
disambiguate (NumberLiteral text line col) = disambiguateNumber text line col
disambiguate (StringLiteral text line col) = disambiguateString text line col
disambiguate (CharLiteral text line col) = disambiguateChar text line col
disambiguate (Operator text line col) = disambiguateOperator text line col

disambiguateWord :: T.Text -> Int -> Int -> Token
disambiguateWord text col line =
    let word = T.toLower text
        tokType = case word of
            "if"       -> TokIf
            "else"     -> TokElse
            "for"      -> TokFor
            "while"    -> TokWhile
            "def"      -> TokDef
            "struct"   -> TokStruct
            "true"     -> TokTrue
            "false"    -> TokFalse
            "break"    -> TokBreak
            "continue" -> TokContinue
            "return"   -> TokReturn
            _          -> TokIdent text
        loc = SrcLoc col line
    in Token tokType loc

disambiguateNumber :: T.Text -> Int -> Int -> Token
disambiguateNumber text col line = if T.any (=='.') text then disambiguateFloat text col line else disambiguateInt text col line

disambiguateFloat :: T.Text -> Int -> Int -> Token
disambiguateFloat text col line =
    let val = read (show text) :: Float
        tokType = TokFloat val
        loc = SrcLoc col line
    in Token tokType loc

disambiguateInt :: T.Text -> Int -> Int -> Token
disambiguateInt text col line=
    let val = read (show text) :: Int
        tokType = TokInt val
        loc = SrcLoc col line
    in Token tokType loc

disambiguateString :: T.Text -> Int -> Int -> Token
disambiguateString text col line =
    let tokType = TokString text
        loc = SrcLoc col line
    in Token tokType loc

disambiguateChar :: T.Text -> Int -> Int -> Token
disambiguateChar text col line =
    let valMonad = readLitChar (show text)
        val = case valMonad of
            [(c, "")] -> c
            _         -> error $ "Malformed character literal " ++ show text ++ " at line " ++ show line ++ " column " ++ show col
        tokType = TokChar val
        loc = SrcLoc col line
    in Token tokType loc


opMap :: Map.Map T.Text TokenType
opMap = Map.fromList [("+", TokPlus), ("-", TokMinus), ("*", TokMult), ("/", TokDiv), ("&", TokBitAnd), ("&&", TokLogAnd), ("|", TokBitOr), ("||", TokLogOr),
                      ("!", TokNot), ("->", TokArrow), ("%", TokMod), (">", TokGT), ("<", TokLT), (">=", TokGE), ("<=", TokLE), ("==", TokEq), ("!=", TokNEq),
                      ("=", TokAssign), ("(", TokOpenParen), (")", TokCloseParen), ("{", TokCrBra), ("}", TokCrKet), ("[", TokSqBra), ("]", TokSqKet)]
disambiguateOperator :: T.Text -> Int -> Int -> Token
disambiguateOperator text col line =
    let tokType' = Map.lookup text opMap
        tokType = case tokType' of 
            Just t  -> t 
            Nothing -> error $ "Malformed operator " ++ show text ++ " at line " ++ show line ++ " column " ++ show col
        loc = SrcLoc col line 
    in Token tokType loc 