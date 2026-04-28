import System.Environment (getArgs)
import Lexer (lexStrux)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    code <- TIO.readFile file
    print $ lexStrux code