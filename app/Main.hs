module Main where

import qualified Data.Text as T
import Data.Char (ord)
import qualified Data.Set as Set

opChars :: Set.Set Char
opChars = Set.fromList ['.', '<', '>', '=', '+', '%', '-', '*', '/', '!', '&', '|', '[', ']', '(', ')', ';']

data FPToken
    = Word          { tokenText :: T.Text, line :: Int, column :: Int }
    | NumberLiteral { tokenText :: T.Text, line :: Int, column :: Int }
    | StringLiteral { tokenText :: T.Text, line :: Int, column :: Int }
    | CharLiteral   { tokenText :: T.Text, line :: Int, column :: Int }
    | Operators     { tokenText :: T.Text, line :: Int, column :: Int }
    | Comments      { tokenText :: T.Text, line :: Int, column :: Int }
    | Whitespace    { tokenText :: T.Text, line :: Int, column :: Int }
    deriving (Show)

data LexerState = LexerState {
    remaining :: T.Text,
    line :: Int,
    column :: Int
}

main :: IO ()
main = do
    putStrLn "Hello world!"

firstPass :: T.Text -> [FPToken]
firstPass input = go $ LexerState input 1 1
    where
        go :: LexerState -> [FPToken]
        go LexerState [] _ _ = []
        go (LexerState text line column)
            | wordStart c    = let (token, rest) = T.span inWord text in (Word token line column) : go (LexerState rest line (column + T.length token))
            | stringStart c  =
            | charStart c    =
            | opStart c      =
            | commentStart c =
            | isWhitespace c =
            | otherwise      = error $ "Unexpected character " ++ show c ++ " at line" ++ show line ++ ", col" ++ show column ++ " while parsing."
                where c = T.head text

wordStart :: Char -> Bool
wordStart c = isLetter c || c == '_'

inWord :: Char -> Bool
inWord c = isAlphaNum c || c == '_'

stringStart :: Char -> Bool
stringStart c = c == "\""

charStart :: Char -> Bool
charStart c = c =="\'"

opStart :: Char -> Bool
opStart c = Set.member c opChars

commentStart :: Char -> Bool
commentStart c = c == '#'

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\n', '\t', '\r']