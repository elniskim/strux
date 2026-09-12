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



data QBEType = Word | Long | Single | Double deriving (Show, Eq)

data QBEOp 
    = SGT | GT | SGE | GE | SLT | LT | SLE | LE | EQ | 
    NE | ADD | SUB | MUL | DIV | NEG | OR | AND | REM |
    ALLOC
    deriving (Show, Eq)

type Label = T.Text
type Ident = T.Text

data Operand
  = Reg Ident
  | LitInt Integer
  | Global Ident
  deriving (Show, Eq)

data Instruction 
  = BinInstr QBEOp QBEType Ident Operand Operand
  | UnInstr QBEOp QBEType Ident Operand
  | Call (Maybe (Ident, QBEType)) Ident [(QBEType, Operand)]
  | Store QBEType Operand Operand  -- value operand, address operand
  | Load QBEType Ident Operand     -- destination reg, address operand
  | Alloc Ident Int                -- %dest =l alloc8 size
  | Copy Ident QBEType Operand     -- %dest =ty copy %src
  deriving (Show, Eq)

data Terminator phase
    = Jump Label
    | JumpNZ Operand Label Label -- First is the value to check, then the true block, then the false block 
    | Return Operand
    | Next (NextTerminator phase) -- Dummy jump forward to the next block, made into a real jump during linearization.

data Phi = Phi Ident QBEType [(Label, Ident)]

data BasicBlock phase = BasicBlock {
    blockLabel :: Label,
    blockPhis :: [Phi],
    blockInsts :: [Instruction],
    terminator :: Terminator phase
}

data Block phase
    = SimpleBlock (BasicBlock Tree)
    | IfElseBlock {
        condLabel :: Label,
        condInsts :: [Instruction],
        condResult :: Operand,
        thenLabel :: Label,
        thenBody :: [Block Tree],
        elseLabel :: Label,
        elseBody :: [Block Tree],
        mergeLabel :: Label,
        mergePhis :: [Phi] -- Will result in a final block composed of mostly phis.
    }
    | LoopBlock {
        initLabel :: Label,
        initInsts :: [Instruction],
        headerLabel :: Label,
        condInsts :: [Instruction],
        condResult :: Operand,
        headerPhis :: [Phi],
        bodyLabel :: Label,
        loopBody :: [Block Tree],
        exitLabel :: Label,
        exitPhis :: [Phi] 
    }

data QBEFunc = QBEFunc T.Text [(Ident, QBEType)] [Block Linear]
data QBEStruct = QBEStruct T.Text [(QBEType, Int)]
data QBEDecl = QBEVarDecl T.Text QBEType | QBEArrDecl T.Text QBEType Int

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