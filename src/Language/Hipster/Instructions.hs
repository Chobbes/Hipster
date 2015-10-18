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
import LinearScan
import LinearScan.Hoopl
import LinearScan.Hoopl.DSL (getStackSlot)
import Unsafe.Coerce
import Data.Maybe


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Int -- ^ Specific register reserved.
     deriving (Eq)

instance Show Register where
  show (Var n) = "(Var " ++ show n ++ ")"
  show (Reg n)
    | n == 0 = "$zero"
    | n == 1 = "$at"
    | n == 2 = "$v0"
    | n == 3 = "$v1"
    | n >= 4 && n <= 7 = "$a" ++ show (n - 4)
    | n >= 8 && n <= 15 = "$t" ++ show (n - 8)
    | n >= 16 && n <= 23 = "$s" ++ show (n - 16)
    | n == 24 = "$t8"
    | n == 25 = "$t9"
    | n == 26 = "$k0"
    | n == 27 = "$k1"
    | n == 28 = "$gp"
    | n == 29 = "$sp"
    | n == 30 = "$fp"
    | n == 31 = "$ra"

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
    ADD :: v -> v -> v -> Inst v O O
    ADDU :: v -> v -> v -> Inst v O O
    ADDI :: v -> v -> Immediate -> Inst v O O
    ADDIU :: v -> v -> Immediate -> Inst v O O
    SUB :: v -> v -> v -> Inst v O O
    SUBU :: v -> v -> v -> Inst v O O
    SUBI :: v -> v -> Immediate -> Inst v O O
    SUBIU :: v -> v -> Immediate -> Inst v O O
    MULT :: v -> v -> Inst v O O
    MULTU :: v -> v -> Inst v O O
    DIV :: v -> v -> Inst v O O
    DIVU :: v -> v -> Inst v O O

    -- Shifts
    SLL :: v -> v -> Immediate -> Inst v O O
    SLLV :: v -> v -> v -> Inst v O O
    SRA :: v -> v -> Immediate -> Inst v O O
    SRL :: v -> v -> Immediate -> Inst v O O
    SRLV :: v -> v -> v -> Inst v O O

    -- Logic
    AND :: v -> v -> v -> Inst v O O
    ANDI :: v -> v -> Immediate -> Inst v O O
    OR :: v -> v -> v -> Inst v O O
    ORI :: v -> v -> Immediate -> Inst v O O
    XOR :: v -> v -> v -> Inst v O O
    XORI :: v -> v -> Immediate -> Inst v O O

    -- Sets
    SLT :: v -> v -> v -> Inst v O O
    SLTI :: v -> v -> Immediate -> Inst v O O
    SLTU :: v -> v -> v -> Inst v O O
    SLTIU :: v -> v -> Immediate -> Inst v O O

    -- Branches and jumps
    J :: Label -> Inst v O C
    JAL :: Label -> Inst v O C

    -- Loads
    LB :: v -> Immediate -> v -> Inst v O O
    LUI :: v -> Immediate -> Inst v O O
    LW :: v -> Immediate -> v -> Inst v O O
    MFHI :: v -> Inst v O O
    MFLO :: v -> Inst v O O

    -- Stores
    SB :: v -> Immediate -> v -> Inst v O O
    SW :: v -> Immediate -> v -> Inst v O O

    -- Noop
    NOOP :: Inst v O O

    -- System calls
    SYSCALL :: Inst v O O

    -- Comments
    COMMENT :: String -> Inst v O O
    INLINE_COMMENT :: Inst v e x -> String -> Inst v e x

deriving instance (Show v) => Show (Inst v e x)
deriving instance (Eq v) => Eq (Inst v e x)

instance NonLocal (Inst v) where
  entryLabel (LABEL l _ _) = l
  successors (J l) = [l]

instance HooplNode (Inst v) where
  mkBranchNode = J
  mkLabelNode = BLANK_LABEL

-- | Turn a register into a VarInfo
toVarInfo :: VarKind -> Register -> VarInfo
toVarInfo k (Var id) = VarInfo (Right $ uniqueToInt id) k True
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
  getReferences (SB a _ b) = map toInput [a, b]
  getReferences (SW a _ b) = map toInput [a, b]

  -- Misc
  getReferences (INLINE_COMMENT i _) = getReferences i
  getReferences _ = []


  setRegisters m (ADD d s t) = return $ dstSet m ADD d s t
  setRegisters m (ADDU d s t) = return $ dstSet m ADDU d s t
  setRegisters m (ADDI d s i) = return $ immSet m ADDI d s i
  setRegisters m (ADDIU d s i) = return $ immSet m ADDIU d s i
  setRegisters m (SUB d s t) = return $ dstSet m SUB d s t
  setRegisters m (SUBU d s t) = return $ dstSet m SUBU d s t
  setRegisters m (SUBI d s i) = return $ immSet m SUBI d s i
  setRegisters m (SUBIU d s i) = return $ immSet m SUBIU d s i
  setRegisters m (MULT a b) = return $ MULT (regSetIn m a) (regSetIn m b)
  setRegisters m (MULTU a b) = return $ MULTU (regSetIn m a) (regSetIn m b)
  setRegisters m (DIV a b) = return $ DIV (regSetIn m a) (regSetIn m b)
  setRegisters m (DIVU a b) = return $ DIVU (regSetIn m a) (regSetIn m b)

  -- Shifts
  setRegisters m (SLL d s i) = return $ immSet m SLL d s i
  setRegisters m (SLLV d s t) = return $ dstSet m SLLV d s t
  setRegisters m (SRA d s i) = return $ immSet m SRA d s i
  setRegisters m (SRL d s i) = return $ immSet m SRL d s i
  setRegisters m (SRLV d s t) = return $ dstSet m SRLV d s t

  -- Logic
  setRegisters m (AND d s t) = return $ dstSet m AND d s t
  setRegisters m (ANDI d s i) = return $ immSet m ANDI d s i
  setRegisters m (OR d s t) = return $ dstSet m OR d s t
  setRegisters m (ORI d s i) = return $ immSet m ORI d s i
  setRegisters m (XOR d s t) = return $ dstSet m XOR d s t
  setRegisters m (XORI d s i) = return $ immSet m XORI d s i

  -- Sets
  setRegisters m (SLT d s t) = return $ dstSet m SLT d s t
  setRegisters m (SLTI d s i) = return $ immSet m SLTI d s i
  setRegisters m (SLTU d s t) = return $ dstSet m SLTU d s t
  setRegisters m (SLTIU d s i) = return $ immSet m SLTIU d s i

  -- Branches and jumps
  -- setRegisters m (JAL _) = -- $ra

  -- Loads
  setRegisters m (LB d o s) = return $ LB (regSetOut m d) o (regSetIn m s)
  setRegisters m (LUI d i) = return $ LUI (regSetOut m d) i
  setRegisters m (LW d o s) = return $ LW (regSetOut m d) o (regSetIn m s)
  setRegisters m (MFHI d) = return $ MFHI (regSetOut m d)
  setRegisters m (MFLO d) = return $ MFLO (regSetOut m d)

  -- Stores
  setRegisters m (SB a o b) = return $ SB (regSetIn m a) o (regSetIn m b)
  setRegisters m (SW a o b) = return $ SW (regSetIn m a) o (regSetIn m b)

  -- Misc
  setRegisters m (INLINE_COMMENT i str) = do setI <- setRegisters m i
                                             return $ INLINE_COMMENT setI str

  setRegisters _ l@(LABEL _ _ _) = return l
  setRegisters _ j@(J _) = return j

  setRegisters _ n = error $ "Unimplemented setRegisters: " ++ show n


  mkMoveOps source _ dest = return [ADDI (Reg dest) (Reg source) 0]

  mkSaveOps source id = do offset <- getStackSlot (Just id)
                           return [SW (Reg source) (fromIntegral offset) (Reg 29)]

  mkRestoreOps id dest = do offset <- getStackSlot (Just id)
                            return [LW (Reg dest) (fromIntegral offset) (Reg 29)]

  op1ToString = show

dstSet :: [((VarId, VarKind), PhysReg)] -> (Register -> Register -> Register -> Inst Register e x) -> Register -> Register -> Register -> Inst Register e x
dstSet m n d s t = n (regSetOut m d) (regSetIn m s) (regSetIn m t)

immSet :: [((VarId, VarKind), PhysReg)] -> (Register -> Register -> Immediate -> Inst Register e x) -> Register -> Register -> Immediate -> Inst Register e x
immSet m n d s i = n (regSetOut m d) (regSetIn m s) i

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = (\(a, b, c) -> f a b c)

deriving instance Show VarKind

regSet :: VarKind -> [((VarId, VarKind), PhysReg)] -> Register -> Register
regSet k m (Reg id) = Reg id
regSet k m (Var id) = Reg . fromMaybe (-1) {-(error $ "id: " ++ show id ++ ", kind: " ++ show k ++ ", m: " ++ show m) -} $ lookup (id, k) m

regSetOut :: [((VarId, VarKind), PhysReg)] -> Register -> Register
regSetOut = regSet Output

regSetIn :: [((VarId, VarKind), PhysReg)] -> Register -> Register
regSetIn = regSet Input

regSetTemp :: [((VarId, VarKind), PhysReg)] -> Register -> Register
regSetTemp = regSet Temp

instInfo :: Register -> Register -> Register -> [VarInfo]
instInfo d s t = [toOutput d, toInput s, toInput t]

immInfo :: Register -> Register -> [VarInfo]
immInfo d s = [toOutput d, toInput s]
