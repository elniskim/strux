{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Lexer (lexStrux)
import Parser (parseStrux)
import Resolver (resolveStrux)
import Pretty (pretty)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

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
    let maybeResolved = resolveStrux <$> parseTree
    case maybeResolved of
        Just (_, errors, _) -> if null errors then TIO.putStrLn "you good" else TIO.putStrLn $ T.intercalate "\n" errors
        Nothing       -> putStrLn "still, bro what"