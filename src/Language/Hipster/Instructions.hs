{- Copyright (C) 2015 Calvin Beck
   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Hipster.Instructions where

import Compiler.Hoopl
import LinearScan.Hoopl
import LinearScan
import Unsafe.Coerce


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Int -- ^ Specific register reserved.
     deriving (Show, Eq)

-- | Indicates a register that we write to.
type Dest = Register

-- | Indicates a register that we read from.
type Source = Register

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Data type representing MIPS instructions, and comments.
data Inst v e x where
    -- Labels
    LABEL :: Label -> String -> Int -> Inst v C O
    BLANK_LABEL :: Label -> Inst v C O

    -- Arithmetic
    ADD :: Dest -> Source -> Source -> Inst v O O
    ADDU :: Dest -> Source -> Source -> Inst v O O
    ADDI :: Dest -> Source -> Immediate -> Inst v O O
    ADDIU :: Dest -> Source -> Immediate -> Inst v O O
    SUB :: Dest -> Source -> Source -> Inst v O O
    SUBU :: Dest -> Source -> Source -> Inst v O O
    SUBI :: Dest -> Source -> Immediate -> Inst v O O
    SUBIU :: Dest -> Source -> Immediate -> Inst v O O
    MULT :: Source -> Source -> Inst v O O
    MULTU :: Source -> Source -> Inst v O O
    DIV :: Source -> Source -> Inst v O O
    DIVU :: Source -> Source -> Inst v O O

    -- Shifts
    SLL :: Dest -> Source -> Immediate -> Inst v O O
    SLLV :: Dest -> Source -> Source -> Inst v O O
    SRA :: Dest -> Source -> Immediate -> Inst v O O
    SRL :: Dest -> Source -> Immediate -> Inst v O O
    SRLV :: Dest -> Source -> Source -> Inst v O O

    -- Logic
    AND :: Dest -> Source -> Source -> Inst v O O
    ANDI :: Dest -> Source -> Immediate -> Inst v O O
    OR :: Dest -> Source -> Source -> Inst v O O
    ORI :: Dest -> Source -> Immediate -> Inst v O O
    XOR :: Dest -> Source -> Source -> Inst v O O
    XORI :: Dest -> Source -> Immediate -> Inst v O O

    -- Sets
    SLT :: Dest -> Source -> Source -> Inst v O O
    SLTI :: Dest -> Source -> Immediate -> Inst v O O
    SLTU :: Dest -> Source -> Source -> Inst v O O
    SLTIU :: Dest -> Source -> Immediate -> Inst v O O

    -- Branches and jumps
    J :: Label -> Inst v O C
    JAL :: Label -> Inst v O C

    -- Loads
    LB :: Dest -> Immediate -> Source -> Inst v O O
    LUI :: Dest -> Immediate -> Inst v O O
    LW :: Dest -> Immediate -> Source -> Inst v O O
    MFHI :: Dest -> Inst v O O
    MFLO :: Dest -> Inst v O O

    -- Stores
    SB :: Source -> Immediate -> Source -> Inst v O O
    SW :: Source -> Immediate -> Source -> Inst v O O

    -- Noop
    NOOP :: Inst v O O

    -- System calls
    SYSCALL :: Inst v O O

    -- Comments
    COMMENT :: String -> Inst v O O
    INLINE_COMMENT :: Inst v e x -> String -> Inst v e x

deriving instance Show (Inst v e x)
deriving instance Eq (Inst v e x)

instance NonLocal (Inst v) where
  entryLabel (LABEL l _ _) = l
  successors (J l) = [l]

instance HooplNode (Inst v) where
  mkBranchNode = J
  mkLabelNode = BLANK_LABEL

-- | Turn a register into a VarInfo
toVarInfo :: VarKind -> Register -> VarInfo
toVarInfo k (Var id) = VarInfo (Right $ uniqueToInt id) k False
toVarInfo k (Reg id) = VarInfo (Left id) k True

toTemp :: Register -> VarInfo
toTemp = toVarInfo Temp

toInput :: Register -> VarInfo
toInput = toVarInfo Input

toOutput :: Register -> VarInfo
toOutput = toVarInfo Output

toInputOutput :: Register -> VarInfo
toInputOutput = toVarInfo InputOutput

-- | Convert a Unique value to an Int.
uniqueToInt :: Unique -> Int
uniqueToInt = unsafeCoerce

instance NodeAlloc (Inst Register) (Inst Register) where
  isCall _ = False
  
  isBranch (J _) = True
  
  retargetBranch (J _) _ = J
  
  mkLabelOp = BLANK_LABEL
  
  mkJumpOp = J

  -- Arithmetic
  getReferences (ADD d s t) = instInfo d s t
  getReferences (ADDU d s t) = instInfo d s t
  getReferences (ADDI d s _) = immInfo d s
  getReferences (ADDIU d s _) = immInfo d s
  getReferences (SUB d s t) = instInfo d s t
  getReferences (SUBU d s t) = instInfo d s t
  getReferences (SUBI d s _) = immInfo d s
  getReferences (SUBIU d s _) = immInfo d s
  getReferences (MULT a b) = map toInput [a, b]
  getReferences (MULTU a b) = map toInput [a, b]
  getReferences (DIV a b) = map toInput [a, b]
  getReferences (DIVU a b) = map toInput [a, b]

  -- Shifts
  getReferences (SLL d s _) = immInfo d s
  getReferences (SLLV d s t) = instInfo d s t
  getReferences (SRA d s _) = immInfo d s
  getReferences (SRL d s _) = immInfo d s
  getReferences (SRLV d s t) = instInfo d s t

  -- Logic
  getReferences (AND d s t) = instInfo d s t
  getReferences (ANDI d s _) = immInfo d s
  getReferences (OR d s t) = instInfo d s t
  getReferences (ORI d s _) = immInfo d s
  getReferences (XOR d s t) = instInfo d s t
  getReferences (XORI d s _) = immInfo d s

  -- Sets
  getReferences (SLT d s t) = instInfo d s t
  getReferences (SLTI d s _) = immInfo d s
  getReferences (SLTU d s t) = instInfo d s t
  getReferences (SLTIU d s _) = immInfo d s

  -- Branches and jumps
  -- getReferences (JAL _) = -- $ra

  -- Loads
  getReferences (LB d _ s) = immInfo d s
  getReferences (LUI d _) = [toOutput d]
  getReferences (LW d _ s) = immInfo d s
  getReferences (MFHI d) = [toOutput d]
  getReferences (MFLO d) = [toOutput d]

  -- Stores
  getReferences (SB a _ b) = immInfo a b
  getReferences (SW a _ b) = immInfo a b

  -- Misc
  getReferences (INLINE_COMMENT i _) = getReferences i
  getReferences _ = []


instInfo :: Register -> Register -> Register -> [VarInfo]
instInfo d s t = [toOutput d, toInput s, toInput t]

immInfo :: Register -> Register -> [VarInfo]
immInfo d s = [toOutput d, toInput s]
