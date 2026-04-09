module Main where

import qualified Data.Text as T
import Data.Char (ord, isLetter, isAlphaNum, isNumber)
import qualified Data.Set as Set

opChars :: Set.Set Char
opChars = Set.fromList ['<', '>', '=', '+', '%', '-', '*', '/', '!', '&', '|', '[', ']', '(', ')', '{', '}', ';']

data FPToken
    = Word          { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | NumberLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | StringLiteral { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | CharLiteral   { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Operators     { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Comments      { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
    | Whitespace    { tokenText :: T.Text, tokenLine :: Int, tokenColumn :: Int }
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
            = let (token, rest) = T.span inNumber text in Word token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , wordStart c
            = let (token, rest) = T.span inWord text in Word token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , stringStart c
            = let (token, rest) = munchString text in Word token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , charStart c
            = let (token, rest) = munchChar text in Word token line column : go (updateLexerState state token rest)

            | Just(c,_) <- u
            , opStart c
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