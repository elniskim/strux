{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedStrings #-}
module QBE where

import AST
import Control.Monad.RWS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B


data QBEReader = QBEReader {
    backEdgeTarget :: T.Text
}
defaultReader :: QBEReader
defaultReader = QBEReader "" -- BeNice ensured no naked breaks or continues for back edges. Empty string serves as a simple placeholder value.  

data QBEState = QBEState {
    nextLabel :: Int
}
defaultState :: QBEState
defaultState = QBEState 1

type Codegen a = RWS QBEReader B.Builder QBEState a

emitStrux :: Program Typechecked -> T.Text
emitStrux pgrm = let (_, backendBuilder) = evalRWS (emitProgram pgrm) defaultReader defaultState in TL.toStrict $ B.toLazyText backendBuilder

emitProgram :: Program Typechecked -> Codegen ()
emitProgram pgrm = mapM_ emitDecl (declList pgrm)

emitDecl :: Decl Typechecked -> Codegen ()
emitDecl (GLobalVarDecl vName vType) = do



getNextLabel :: Codegen Int
getNextLabel = do 
    i <- gets nextLabel
    modify (\inState -> inState {nextLabel = i + 1})
    return i