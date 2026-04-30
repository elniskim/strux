{-# LANGUAGE OverloadedStrings #-}
module Lexer (Token(Token), TokenType, SrcLoc, lexStrux) where

import qualified Data.Text as T
import Data.Char (isLetter, isAlphaNum, isNumber, readLitChar)
import qualified Data.Set as Set
import qualified Data.Map as Map --Consider HashMap instead?

-- opChars :: Set.Set Char 
-- opChars = Set.fromList ['<', '>', '=', '+', '%', '-', '*', '/', '!', '&', '|', '[', ']', '(', ')', '{', '}', ';', ':', ',']

singleOps :: Set.Set Char
singleOps = Set.fromList ['(', ')', '{', '}', '[', ']', ';', ',', ':']

multiOps :: Set.Set Char
multiOps = Set.fromList ['<', '>', '=', '+', '%', '-', '*', '/', '!', '&', '|']

data FPToken
    = WordToken     { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | NumberLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | StringLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | CharLiteral   { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Operator      { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Malformed     { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int, errMsg :: T.Text }
    -- | Comments      { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    -- | Whitespace    { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    deriving (Show)


data LexerState = LexerState {
    remaining :: T.Text,
    stateLine :: Int,
    stateColumn :: Int
}



firstPass :: T.Text -> [FPToken]
firstPass input = go $ LexerState input 1 1
    where
        go :: LexerState -> [FPToken]
        go state@(LexerState {remaining = text, stateLine = line, stateColumn = column})
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
            , multiOpStart c
            = let (token, rest) = T.span multiOpStart text in Operator token line column : go (updateLexerState state token rest)

            | Just(c,r) <- u
            , singleOpStart c
            = let (token, rest) = (T.singleton c, r) in Operator token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , isWhitespace c
            = let (token, rest) = T.span isWhitespace text in go (updateLexerState state token rest) -- Whitespace isn't a token.

            | Just(c,_) <- u
            , commentStart c
            = let (token, rest) = T.span (/= '\n') text in go (updateLexerState state token rest) -- Comments aren't a token.

            | Just(c,r) <- u
            = let (token, rest) = (T.singleton c, r) in Malformed token line column (T.pack $ "Illegal character " ++ show c ++ " at line " ++ show line ++ " column " ++ show column) : go (updateLexerState state token rest)

                where u = T.uncons text

updateLexerState :: LexerState -> T.Text -> T.Text -> LexerState
updateLexerState state token newRemaining =
    let foldedState = T.foldl' update state token
    in foldedState { remaining = newRemaining }
        where update :: LexerState -> Char -> LexerState
              update (LexerState {remaining = r, stateLine = line, stateColumn = col}) char
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

multiOpStart :: Char -> Bool
multiOpStart c = Set.member c multiOps

singleOpStart :: Char -> Bool
singleOpStart c = Set.member c singleOps

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
    | TokLogOr
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
    | TokErr T.Text
    deriving (Show, Eq)

data SrcLoc = SrcLoc {
    srcLine :: !Int,
    srcLol  :: !Int
} deriving (Show, Eq)

data Token = Token {
    tokenType :: TokenType,
    srcLoc :: SrcLoc
}

instance Show Token where
    show token = show $ tokenType token

secondPass :: [FPToken] -> [Token]
secondPass = map disambiguate

disambiguate :: FPToken -> Token
disambiguate (WordToken text line col) = disambiguateWord text line col
disambiguate (NumberLiteral text line col) = disambiguateNumber text line col
disambiguate (StringLiteral text line col) = disambiguateString text line col
disambiguate (CharLiteral text line col) = disambiguateChar text line col
disambiguate (Operator text line col) = disambiguateOperator text line col
disambiguate (Malformed text line col msg) = disambiguateMalformed text line col msg

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
    let val = read (T.unpack text) :: Float
        tokType = TokFloat val
        loc = SrcLoc col line
    in Token tokType loc

disambiguateInt :: T.Text -> Int -> Int -> Token
disambiguateInt text col line=
    let val = read (T.unpack text) :: Int
        tokType = TokInt val
        loc = SrcLoc col line
    in Token tokType loc

disambiguateString :: T.Text -> Int -> Int -> Token
disambiguateString text col line =
    let unwrapped = T.drop 1 $ T.dropEnd 1 text
        tokType = TokString unwrapped
        loc = SrcLoc col line
    in Token tokType loc

disambiguateChar :: T.Text -> Int -> Int -> Token
disambiguateChar text col line =
    let unwrapped = T.drop 1 $ T.dropEnd 1 text -- Expensive. :(
        valMonad = readLitChar (T.unpack unwrapped)
        val = case valMonad of
            [(c, "")] -> Just c
            _         -> Nothing
        tokType = case val of
            Just c -> TokChar c
            Nothing -> TokErr $ T.pack ("Malformed character literal " ++ show text ++ " at line " ++ show line ++ " column " ++ show col)
        loc = SrcLoc col line
    in Token tokType loc

opMap :: Map.Map T.Text TokenType
opMap = Map.fromList [("+", TokPlus), ("-", TokMinus), ("*", TokMult), ("/", TokDiv), ("&", TokBitAnd), ("&&", TokLogAnd), ("|", TokBitOr), ("||", TokLogOr),
                      ("!", TokNot), ("->", TokArrow), ("%", TokMod), (">", TokGT), ("<", TokLT), (">=", TokGE), ("<=", TokLE), ("==", TokEq), ("!=", TokNEq),
                      ("=", TokAssign), ("(", TokOpenParen), (")", TokCloseParen), ("{", TokCrBra), ("}", TokCrKet), ("[", TokSqBra), ("]", TokSqKet),
                      (";", TokSemi), (":", TokColon)]
disambiguateOperator :: T.Text -> Int -> Int -> Token
disambiguateOperator text col line =
    let tokType' = Map.lookup text opMap
        tokType = case tokType' of
            Just t  -> t
            Nothing -> TokErr $ T.pack ("Malformed operator " ++ show text ++ " at line " ++ show line ++ " column " ++ show col)
        loc = SrcLoc col line
    in Token tokType loc

disambiguateMalformed :: T.Text -> Int -> Int -> T.Text -> Token
disambiguateMalformed _ col line msg =
    let tokType = TokErr msg
        loc = SrcLoc col line
    in Token tokType loc

-- TODO: Likely return an either with a Right [Token] and a Left T.Text with an error/list of errors. Something with folding a bunch of error tokens together to get a list of problems.
lexStrux' :: T.Text -> [Token]
lexStrux' = secondPass . firstPass

lexStrux :: T.Text -> Either T.Text [Token]
lexStrux input =
    let tokens = lexStrux' input
        report = errReport tokens
    in case report of
        [] -> Right tokens
        _  -> Left $ T.intercalate "\n" ("Parse errors reported:" : report)

errReport :: [Token] -> [T.Text]
errReport = foldr addErr []
    where addErr token report = case tokenType token of 
              TokErr msg -> msg : report
              _          -> report 
