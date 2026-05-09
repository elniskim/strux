{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Lexer (lexStrux)
import Parser (parseStrux)
import Pretty (pretty)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    code <- TIO.readFile file
    _ <- TIO.writeFile "golden/output.strx" (code <> "\n-----\n")
    let tokens = lexStrux code
    print $ show tokens 
    let parseTree = parseStrux tokens
    case parseTree of
        Just tree -> TIO.appendFile "golden/output.strx" (pretty 0 tree)
        Nothing   -> putStrLn "bro what"