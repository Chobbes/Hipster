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

import Language.Hipster.Registers
import Compiler.Hoopl
import LinearScan
import LinearScan.Hoopl
import LinearScan.Hoopl.DSL (getStackSlot)
import Unsafe.Coerce
import Data.Maybe
import Data.List


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


deriving instance (Eq v) => Eq (Inst v e x)

instance Show v => Show (Inst v e x) where
  -- Labels
  show (LABEL l str i)
    | i == 0 = str ++ ":"
    | otherwise = str ++ "_" ++ show i ++ ":"

  show (BLANK_LABEL l) = show l ++ ":"
  
  -- Arithmetic
  show (ADD d s t) = "add " ++ (intercalate ", " $ map show [d, s, t])
  show (ADDU d s t) = "addu " ++ (intercalate ", " $ map show [d, s, t])
  show (ADDI d s i) = "addi " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (ADDIU d s i) = "addiu " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SUB d s t) = "sub " ++ (intercalate ", " $ map show [d, s, t])
  show (SUBU d s t) = "subu " ++ (intercalate ", " $ map show [d, s, t])
  show (SUBI d s i) = "subi " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SUBIU d s i) = "subiu " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (MULT a b) = "mult " ++ show a ++ ", " ++ show b
  show (MULTU a b) = "multu " ++ show a ++ ", " ++ show b
  show (DIV a b) = "div " ++ show a ++ ", " ++ show b
  show (DIVU a b) = "divu " ++ show a ++ ", " ++ show b

    -- Shifts
  show (SLL d s i) = "sll " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SLLV d s t) = "sllv " ++ (intercalate ", " $ map show [d, s, t])
  show (SRA d s i) = "sra " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SRL d s i) = "srl " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SRLV d s t) = "srlv " ++ (intercalate ", " $ map show [d, s, t])

    -- Logic
  show (AND d s t) = "and " ++ (intercalate ", " $ map show [d, s, t])
  show (ANDI d s i) = "andi " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (OR d s t) = "or " ++ (intercalate ", " $ map show [d, s, t])
  show (ORI d s i) = "ori " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (XOR d s t) = "xor " ++ (intercalate ", " $ map show [d, s, t])
  show (XORI d s i) = "xori " ++ show d ++ ", " ++ show s ++ ", " ++ show i

    -- Sets
  show (SLT d s t) = "slt " ++ (intercalate ", " $ map show [d, s, t])
  show (SLTI d s i) = "slti " ++ show d ++ ", " ++ show s ++ ", " ++ show i
  show (SLTU d s t) = "sltu " ++ (intercalate ", " $ map show [d, s, t])
  show (SLTIU d s i) = "sltiu " ++ show d ++ ", " ++ show s ++ ", " ++ show i

    -- Branches and jumps
  -- Currently these are problems.
  show (J l) = "j " ++ show l
  show (JAL l) = "jal " ++ show l

    -- Loads
  show (LB d o s) = "lb " ++ show d ++ ", " ++ show o ++ "(" ++ show s ++ ")"
  show (LUI d i) = "lui " ++ show d ++ ", " ++ show i
  show (LW d o s) = "lw " ++ show d ++ ", " ++ show o ++ "(" ++ show s ++ ")"
  show (MFHI d) = "mfhi " ++ show d
  show (MFLO d) = "mflo " ++ show d

    -- Stores
  show (SB d o s) = "sb " ++ show d ++ ", " ++ show o ++ "(" ++ show s ++ ")"
  show (SW d o s) = "sw " ++ show d ++ ", " ++ show o ++ "(" ++ show s ++ ")"

    -- Noop
  show NOOP = "noop"

    -- System calls
  show SYSCALL = "syscall"

    -- Comments
  show (COMMENT str) = "# " ++ str
  show (INLINE_COMMENT i str) = (show i) ++ " # " ++ str


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

instance NodeAlloc (Inst Register) (Inst Physical) where
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

  setRegisters _ (LABEL l s n) = return $ LABEL l s n
  setRegisters _ (J l) = return $ J l

  setRegisters _ n = error $ "Unimplemented setRegisters: " ++ show n


  mkMoveOps source _ dest = return [ADDI (Phys dest) (Phys source) 0]

  mkSaveOps source id = do offset <- getStackSlot (Just id)
                           return [SW (Phys source) (fromIntegral offset) (physical sp)]

  mkRestoreOps id dest = do offset <- getStackSlot (Just id)
                            return [LW (Phys dest) (fromIntegral offset) (physical sp)]

  op1ToString = show

dstSet :: [((VarId, VarKind), PhysReg)] -> (Physical -> Physical -> Physical -> Inst Physical e x) -> Register -> Register -> Register -> Inst Physical e x
dstSet m n d s t = n (regSetOut m d) (regSetIn m s) (regSetIn m t)

immSet :: [((VarId, VarKind), PhysReg)] -> (Physical -> Physical -> Immediate -> Inst Physical e x) -> Register -> Register -> Immediate -> Inst Physical e x
immSet m n d s i = n (regSetOut m d) (regSetIn m s) i

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = (\(a, b, c) -> f a b c)

deriving instance Show VarKind

regSet :: VarKind -> [((VarId, VarKind), PhysReg)] -> Register -> Physical
regSet k m (Reg id) = Phys id
regSet k m (Var id) = Phys . toGeneralReg . fromJust $ lookup (id, k) m

regSetOut :: [((VarId, VarKind), PhysReg)] -> Register -> Physical
regSetOut = regSet Output

regSetIn :: [((VarId, VarKind), PhysReg)] -> Register -> Physical
regSetIn = regSet Input

regSetTemp :: [((VarId, VarKind), PhysReg)] -> Register -> Physical
regSetTemp = regSet Temp

instInfo :: Register -> Register -> Register -> [VarInfo]
instInfo d s t = [toOutput d, toInput s, toInput t]

immInfo :: Register -> Register -> [VarInfo]
immInfo d s = [toOutput d, toInput s]

toGeneralReg :: Int -> Int
toGeneralReg n
  | n >= 0 && n <= 17 = n + 8
  | otherwise = error "Invalid general purpose register."
