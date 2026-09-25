{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import IRTypes
import Control.Monad.Writer
import Data.Text as T
import Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as TL

type CodegenM = Writer B.Builder ()

outputStrux :: QBEIR -> T.Text
outputStrux qbeir = (TL.toStrict . toLazyText . execWriter) $ outputIR qbeir

outputIR :: QBEIR -> CodegenM
outputIR (QBEIR funcs structs decls)= do
    mapM_ outputStruct structs
    mapM_ outputDecl decls
    mapM_ outputFunc funcs

outputStruct :: QBEStruct -> CodegenM
outputStruct (QBEStruct name contents) = do
    let strContents = T.intercalate ", " $ fmap (\(qType, fieldCount) -> if fieldCount == 1 then typeText qType else typeText qType <> " " <> (T.pack . show) fieldCount) contents
    tellDeclaration $ "type :" <> name <> " = { " <> strContents <> " }"

outputDecl :: QBEDecl -> CodegenM
outputDecl (QBEStrDecl stringVal name) = do
    let strContents = T.pack $ Prelude.unwords (fmap (\c -> '\'' : c : ['\'']) (T.unpack stringVal))
    tellDeclaration $ "data $" <> name <> " = { l " <> strContents <> " 0 }"
outputDecl (QBEGlobalDecl name declSize) = tellDeclaration $ "data $" <> name <> " = align 8 { z " <> (T.pack . show) declSize <> " }"

outputFunc :: QBEFunc -> CodegenM
outputFunc (QBEFunc funcName retType funcArgs fBody) = do
    let sigArgs = T.intercalate ", " $ fmap (\(ident, qType) -> typeText qType <> " %" <> ident) funcArgs
    let funcSig = "function " <> maybe " " (\t -> " " <> typeText t <> " ") retType <> "$" <> funcName <> "(" <> sigArgs <> ") {"
    tellTopLevel funcSig
    mapM_ outputBlock fBody
    tellTopLevel "}"

outputBlock :: BasicBlock Linear -> CodegenM
outputBlock (BasicBlock bLabel bInsts bTerm) = do
    tellTopLevel $ "@" <> bLabel
    mapM_ outputInst bInsts
    outputTerm bTerm

outputInst :: Instruction -> CodegenM
outputInst (BinInstr qOp qType dest lVal rVal) = do 
    let lValText = operandText lVal
    let rValText = operandText rVal
    tellInst $ "%" <> dest <> " =" <> typeText qType <> " " <> opText qOp <> " " <> lValText <> " ," <> rValText
outputInst (UnInstr qOp qType dest rVal) = do 
    let rValText = operandText rVal
    tellInst $ "%" <> dest <> " =" <> typeText qType <> " " <> opText qOp <> " " <> rValText
outputInst (Call retPair fName callArgs) = do 
    let retText = case retPair of
            Nothing -> "" 
            Just (regName, qType) -> "%" <> regName <> " =" <> typeText qType <> " "
    let callInfo = T.intercalate ", " $ fmap (\(qType, loc) -> typeText qType <> " " <> operandText loc) callArgs
    tellInst $ retText <> "call $" <> fName <> "(" <> callInfo <> ")"
outputInst (Store qType valOp addrOp) = tellInst $ "store" <> typeText qType <> " " <> operandText valOp <> ", " <> operandText addrOp
outputInst (Load qType dest addrOp) = tellInst $ "%" <> dest <> " =" <> typeText qType <> " load" <> typeText qType <> " " <> operandText addrOp
outputInst (Alloc dest numBytes) = tellInst $ "%" <> dest <> " =l alloc8 " <> (T.pack . show) numBytes
outputInst (Blit fromReg toReg numBytes) = tellInst $ "blit " <> operandText fromReg <> ", " <> operandText toReg <> ", " <> (T.pack . show) numBytes

outputTerm :: Terminator Linear -> CodegenM
outputTerm (Jump label) = tellInst $ "jmp @" <> label
outputTerm (JumpNZ reg tLabel fLabel) = tellInst $ "jnz " <> operandText reg <> ", @" <> tLabel <> ", @" <> fLabel
outputTerm (Return retReg) = tellInst $ "ret " <> maybe "" operandText retReg
outputTerm (Next _) = error "outputTerm: next should be unconstructable in linear basic blocks"

tellDeclaration :: T.Text -> CodegenM
tellDeclaration txt = tell $ B.fromText (txt <> "\n\n")

tellTopLevel :: T.Text -> CodegenM
tellTopLevel txt = tell $ B.fromText (txt <> "\n")

tellInst :: T.Text -> CodegenM
tellInst txt = tell $ B.fromText ("\t" <> txt <> "\n")

typeText :: QBEType -> T.Text
typeText Word = "w"
typeText Long = "l"
typeText Single = "s"
typeText Double = "d"
typeText (Aggregate name) = ":" <> name

opText :: QBEOp -> T.Text
opText SGT = "sgt" 
opText IRTypes.GT = "gt"
opText SGE = "sge"
opText GE = "ge"
opText SLT = "slt"
opText IRTypes.LT = "lt"
opText SLE = "sle"
opText LE = "le"
opText IRTypes.EQ = "eq"
opText NE = "ne"
opText ADD = "add"
opText SUB = "sub"
opText MUL = "mul"
opText DIV = "div"
opText NEG = "neg"
opText OR = "or"
opText AND = "and"
opText REM = "rem"

operandText :: Operand -> T.Text
operandText (Reg regName) = "%" <> regName
operandText (LitInt intVal) = (T.pack . show) intVal
operandText (LitFloat floatVal) = (T.pack . show) floatVal
operandText (Global regName) = "$" <> regName