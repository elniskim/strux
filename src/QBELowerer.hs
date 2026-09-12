{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use camelCase" -}
module QBELowerer where

import AST
import IRTypes
import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.Text as T

type SymbolTable = Map.Map Ident -- Maps from symbol name to SSA target name

data QBEReader = QBEReader {
    backEdgeTarget :: Label
}
defaultReader :: QBEReader
defaultReader = QBEReader "" -- BeNice ensured no naked breaks or continues for back edges. Empty string serves as a simple placeholder value.  

data QBEState = QBEState {
    nextLabel :: Int,
    ifElseMergeLabelStack :: [Label],
    structOffsetMap :: Map.Map (T.Text, T.Text) Int -- (Struct, Field)
    -- Add loop info stack once designed.
}
defaultState :: QBEState
defaultState = QBEState 1 [] Map.empty

type QBEM a = RWS QBEReader () QBEState a

lowerStrux :: Program Typechecked -> QBEIR
lowerStrux pgrm = let (res, _) = evalRWS (lowerProgram pgrm) defaultReader defaultState in res

lowerProgram :: Program Typechecked -> QBEM QBEIR
lowerProgram (Program decls) = do
    let orderedDecls = filter isStruct decls ++ filter (not . isStruct) decls -- Guarantees that the structs are processed first to make the map of struct offsets.
    loweredDecls <- mapM lowerDecl orderedDecls
    return $ mconcat loweredDecls

lowerDecl :: Decl Typechecked -> QBEM QBEIR
lowerDecl (GlobalArrDecl vName (ArrayType num _)) = return $ QBEIR [] [] [QBEArrDecl vName Long num]
lowerDecl (GlobalVarDecl vName _) = return $ QBEIR [] [] [QBEVarDecl vName Long]
lowerDecl (StructDef name attrs) = do 
    updateOffsets name attrs 0
    return $ QBEIR [] [QBEStruct name [(Long, getExtent field) | field <- attrs]] []







getNextLabel :: QBEM Int
getNextLabel = do
    i <- gets nextLabel
    modify (\inState -> inState {nextLabel = i + 1})
    return i

isStruct :: Decl Typechecked -> Bool
isStruct (StructDef _ _) = True
isStruct _ = False

getExtent :: StructField Typechecked -> Int
getExtent (Vector _ (ArrayType extent _)) = extent
getExtent _ = 1

updateOffsets :: T.Text -> [StructField Typechecked] -> Int -> QBEM ()
updateOffsets _ [] _ = return ()
updateOffsets structName (attr@(Scalar fieldName _) : attrs) currOffset = do
    oldMap <- gets structOffsetMap
    let newMap = Map.insert (structName, fieldName) currOffset oldMap
    modify (\inState -> inState {structOffsetMap = newMap})
    updateOffsets structName attrs (currOffset + getFieldSize attr)
updateOffsets structName (attr@(Vector fieldName _) : attrs) currOffset = do
    oldMap <- gets structOffsetMap
    let newMap = Map.insert (structName, fieldName) currOffset oldMap
    modify (\inState -> inState {structOffsetMap = newMap})
    updateOffsets structName attrs (currOffset + getFieldSize attr)
updateOffsets structName (attr@(Struct fieldName _): attrs) currOffset = do
    oldMap <- gets structOffsetMap
    let newMap = Map.insert (structName, fieldName) currOffset oldMap
    modify (\inState -> inState {structOffsetMap = newMap})
    updateOffsets structName attrs (currOffset + getFieldSize attr)

getFieldSize :: StructField Typechecked -> Int
getFieldSize (Scalar _ _) = 64
getFieldSize (Vector _ (ArrayType extent _)) = extent * 64
getFieldSize (Struct _ attrs) = sum $ fmap getFieldSize attrs
getFieldSize (Vector _ _) = error "QBELowerer: vector field of struct without arraytype"
