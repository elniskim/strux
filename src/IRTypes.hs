{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module IRTypes where 

import qualified Data.Text as T
import Data.Void

data Tree
data Linear

type family NextTerminator phase
type instance NextTerminator Tree   = ()
type instance NextTerminator Linear = Void



data QBEType = Word | Long | Single | Double | Aggregate T.Text deriving (Show, Eq)

data QBEOp 
    = SGT | GT | SGE | GE | SLT | LT | SLE | LE | EQ | 
    NE | ADD | SUB | MUL | DIV | NEG | OR | AND | REM
    deriving (Show, Eq)

type Label = T.Text
type Ident = T.Text

data Operand
  = Reg Ident
  | LitInt Int
  | LitFloat Float
  | Global Ident
  deriving (Show, Eq)

data Instruction 
  = BinInstr QBEOp QBEType Ident Operand Operand
  | UnInstr QBEOp QBEType Ident Operand
  | Call (Maybe (Ident, QBEType)) Ident [(QBEType, Operand)]
  | Store QBEType Operand Operand  -- value operand, address operand
  | Load QBEType Ident Operand     -- destination reg, address operand
  | Alloc Ident Int                -- %dest =l alloc8 size
  | Blit Operand Operand Int       
  deriving (Show, Eq)

data Terminator phase
    = Jump Label
    | JumpNZ Operand Label Label -- First is the value to check, then the true block, then the false block 
    | Return (Maybe Operand)
    | Next (NextTerminator phase) -- Dummy jump forward to the next block, made into a real jump during linearization.

data BasicBlock phase = BasicBlock {
    blockLabel :: Label,
    blockInsts :: [Instruction],
    terminator :: Terminator phase
}

data Block
    = SimpleBlock (BasicBlock Tree)
    | IfElseBlock {
        condLabel :: Label,
        condInsts :: [Instruction],
        condResult :: Operand,
        thenBody :: [Block],
        elseBody :: [Block],
        mergeLabel :: Label
    }
    | LoopBlock {
        initLabel :: Label,
        initInsts :: [Instruction],
        condInsts :: [Instruction],
        condResult :: Operand,
        headerLabel :: Label,
        loopBody :: [Block], -- No explicit body label. If empty body, body label is latch. If there is a body, pop head and retrieve first label.
        latchLabel :: Label,
        incrInsts :: [Instruction],
        exitLabel :: Label
    }

data QBEFunc = QBEFunc T.Text (Maybe QBEType) [(Ident, QBEType)] [BasicBlock Linear]
data QBEStruct = QBEStruct T.Text [(QBEType, Int)]
data QBEDecl = QBEVarDecl T.Text QBEType | QBEArrDecl T.Text QBEType Int | QBEStrDecl T.Text Ident

data QBEIR = QBEIR {
    funcIRs :: [QBEFunc],
    structIRs :: [QBEStruct],
    declIRs :: [QBEDecl]
}

instance Semigroup QBEIR where 
    (<>) :: QBEIR -> QBEIR -> QBEIR
    (<>) (QBEIR f1 s1 d1) (QBEIR f2 s2 d2) = QBEIR (f1 ++ f2) (s1 ++ s2) (d1 ++ d2)

instance Monoid QBEIR where
    mempty :: QBEIR
    mempty = QBEIR [] [] []
