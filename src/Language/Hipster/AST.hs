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


module Language.Hipster.AST where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Control.Monad.State
import Compiler.Hoopl
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Function
import Data.Maybe
import Unsafe.Coerce
import LinearScan.Hoopl
import LinearScan


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Int -- ^ Specific register reserved.
     deriving (Show, Eq)

-- | Indicates a register that we write to.
type Dest = Register

-- | Indicates a register that we read from.
type Source = Register

-- | Turn a register into a VarInfo
toVarInfo :: Register -> VarInfo
toVarInfo (Var id) = VarInfo (Right $ uniqueToInt id) Temp False
toVarInfo (Reg id) = VarInfo (Left id) Temp True

-- | Convert a Unique value to an Int.
uniqueToInt :: Unique -> Int
uniqueToInt = unsafeCoerce

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Data type representing MIPS instructions, and comments.
data Inst e x where
    -- Labels
    LABEL :: Label -> String -> Int -> Inst C O
    BLANK_LABEL :: Label -> Inst C O

    -- Arithmetic
    ADD :: Dest -> Source -> Source -> Inst O O
    ADDU :: Dest -> Source -> Source -> Inst O O
    ADDI :: Dest -> Source -> Immediate -> Inst O O
    ADDIU :: Dest -> Source -> Immediate -> Inst O O
    SUB :: Dest -> Source -> Source -> Inst O O
    SUBU :: Dest -> Source -> Source -> Inst O O
    SUBI :: Dest -> Source -> Immediate -> Inst O O
    SUBIU :: Dest -> Source -> Immediate -> Inst O O
    MULT :: Source -> Source -> Inst O O
    MULTU :: Source -> Source -> Inst O O
    DIV :: Source -> Source -> Inst O O
    DIVU :: Source -> Source -> Inst O O

    -- Shifts
    SLL :: Dest -> Source -> Immediate -> Inst O O
    SLLV :: Dest -> Source -> Source -> Inst O O
    SRA :: Dest -> Source -> Immediate -> Inst O O
    SRL :: Dest -> Source -> Immediate -> Inst O O
    SRLV :: Dest -> Source -> Source -> Inst O O

    -- Logic
    AND :: Dest -> Source -> Source -> Inst O O
    ANDI :: Dest -> Source -> Immediate -> Inst O O
    OR :: Dest -> Source -> Source -> Inst O O
    ORI :: Dest -> Source -> Immediate -> Inst O O
    XOR :: Dest -> Source -> Source -> Inst O O
    XORI :: Dest -> Source -> Immediate -> Inst O O

    -- Sets
    SLT :: Dest -> Source -> Source -> Inst O O
    SLTI :: Dest -> Source -> Immediate -> Inst O O
    SLTU :: Dest -> Source -> Source -> Inst O O
    SLTIU :: Dest -> Source -> Immediate -> Inst O O

    -- Branches and jumps
    J :: Label -> Inst O C
    JAL :: Label -> Inst O C

    -- Loads
    LB :: Dest -> Immediate -> Source -> Inst O O
    LUI :: Dest -> Immediate -> Inst O O
    LW :: Dest -> Immediate -> Source -> Inst O O
    MFHI :: Dest -> Inst O O
    MFLO :: Dest -> Inst O O

    -- Stores
    SB :: Source -> Immediate -> Source -> Inst O O
    SW :: Source -> Immediate -> Source -> Inst O O

    -- Noop
    NOOP :: Inst O O

    -- System calls
    SYSCALL :: Inst O O

    -- Comments
    COMMENT :: String -> Inst O O
    INLINE_COMMENT :: Inst e x -> String -> Inst e x

deriving instance Show (Inst e x)
deriving instance Eq (Inst e x)

instance NonLocal Inst where
  entryLabel (LABEL l _ _) = l
  successors (J l) = [l]

instance HooplNode Inst where
  mkBranchNode = J
  mkLabelNode = BLANK_LABEL

instance NodeAlloc Inst Inst where
  isCall _ = False
  
  isBranch (J _) = True
  
  retargetBranch (J _) _ l = J l
  
  mkLabelOp = BLANK_LABEL
  
  mkJumpOp = J

  -- Arithmetic
  getReferences (ADD d s t) = map toVarInfo [d, s, t]
  getReferences (ADDU d s t) = map toVarInfo [d, s, t]
  getReferences (ADDI d s _) = map toVarInfo [d, s]
  getReferences (ADDIU d s _) = map toVarInfo [d, s]
  getReferences (SUB d s t) = map toVarInfo [d, s, t]
  getReferences (SUBU d s t) = map toVarInfo [d, s, t]
  getReferences (SUBI d s _) = map toVarInfo [d, s]
  getReferences (SUBIU d s _) = map toVarInfo [d, s]
  getReferences (MULT a b) = map toVarInfo [a, b]
  getReferences (MULTU a b) = map toVarInfo [a, b]
  getReferences (DIV a b) = map toVarInfo [a, b]
  getReferences (DIVU a b) = map toVarInfo [a, b]

  -- Shifts
  getReferences (SLL d s _) = map toVarInfo [d, s]
  getReferences (SLLV d s t) = map toVarInfo [d, s, t]
  getReferences (SRA d s _) = map toVarInfo [d, s]
  getReferences (SRL d s _) = map toVarInfo [d, s]
  getReferences (SRLV d s t) = map toVarInfo [d, s, t]

  -- Logic
  getReferences (AND d s t) = map toVarInfo [d, s, t]
  getReferences (ANDI d s _) = map toVarInfo [d, s]
  getReferences (OR d s t) = map toVarInfo [d, s, t]
  getReferences (ORI d s _) = map toVarInfo [d, s]
  getReferences (XOR d s t) = map toVarInfo [d, s, t]
  getReferences (XORI d s _) = map toVarInfo [d, s]

  -- Sets
  getReferences (SLT d s t) = map toVarInfo [d, s, t]
  getReferences (SLTI d s _) = map toVarInfo [d, s]
  getReferences (SLTU d s t) = map toVarInfo [d, s, t]
  getReferences (SLTIU d s _) = map toVarInfo [d, s]

  -- Loads
  getReferences (LB d _ s) = map toVarInfo [d, s]
  getReferences (LUI d _) = [toVarInfo d]
  getReferences (LW d _ s) = map toVarInfo [d, s]
  getReferences (MFHI d) = [toVarInfo d]
  getReferences (MFLO d) = [toVarInfo d]

  -- Stores
  getReferences (SB a _ b) = map toVarInfo [a, b]
  getReferences (SW a _ b) = map toVarInfo [a, b]

  -- Misc
  getReferences (INLINE_COMMENT i _) = getReferences i
  getReferences _ = []


-- | MIPS assembly is essentially a list of instructions, which is what
-- the Free monad gives us.
type MipsBlock a = FreeT ((,) (Inst O O)) SimpleUniqueMonad a

-- | Convert a MIPS block to a list of instructions.
compileBlock :: MipsBlock a -> SimpleUniqueMonad [Inst O O]
compileBlock = compile'
  where compile' free =
               do x <- runFreeT free
                  case x of
                    (Pure _) -> return []
                    (Free (inst, fs)) -> (inst :) <$> compile' fs

-- | Convert a MIPS block to a Hoopl block.
toHooplBlock :: MipsBlock (Inst O C) -> SimpleUniqueMonad (Block Inst O C)
toHooplBlock free = do fNode <- runFreeT free
                       case fNode of
                         (Pure close) -> return $ BlockOC emptyBlock close
                         (Free (inst, fs)) -> blockCons inst <$> toHooplBlock fs


-- | Allocate a new 32-bit value (any register will do).
newVar :: MipsBlock Register
newVar = do unique <- lift freshUnique
            return $ Var unique


add :: Dest -> Source -> Source -> MipsBlock Register
add d s t = liftF (ADD d s t, d)

sub :: Dest -> Source -> Source -> MipsBlock Register
sub d s t = liftF (SUB d s t, d)

jmp :: Label -> MipsBlock (Inst O C)
jmp l = return $ J l


-- | Keeps track of label numbers, and unique Hoopl labelings.
type LabelMonad = State (M.Map String Int, Label)

getLabel :: String -> LabelMonad Label
getLabel name = do strNum <- gets $ fromMaybe 0 . M.lookup name . fst
                   unique <- gets $ intToLabel . succ . labelToInt . snd
                   modify $ \(m,_) -> (M.insert name (strNum+1) m, unique)
                   return unique

labelToInt :: Label -> Int
labelToInt = unsafeCoerce

intToLabel :: Int -> Label
intToLabel = unsafeCoerce

-- Need a state monad which keeps track of the labels.
type MipsProgram = StateT (LabelMap (MipsLabelBlock (Inst O C))) LabelMonad


-- | A basic block for a MIPS program.
data MipsLabelBlock a = MipsLabelBlock { blockLabel :: Label  -- ^ Unique Hoopl label.
                                       , labelPrefix :: String -- ^ Label prefix string.
                                       , labelNum :: Int  -- ^ May be multiple labels with the same prefix.
                                       , mipsBlock :: MipsBlock a -- ^ Actual block of MIPS instructions.
                                       }

-- | Create a new basic block with a named label.
newBB :: String -> MipsBlock (Inst O C) -> MipsProgram Label
newBB name block = do label <- lift $ getLabel name
                      let newBlock = MipsLabelBlock label name 0 block
                      
                      modify $ mapInsert label newBlock
                      return label


-- | Convert a MIPS program to an assoc list of labels and their blocks.
compileProgList :: MipsProgram a -> SimpleUniqueMonad [(Label, (Block Inst C C))]
compileProgList = flip evalState (M.empty, intToLabel 0) . compile'
  where compile' state =
          do x <- execStateT state mapEmpty
             return . mapM (\(k, v) -> do cb <- toHooplClosed v
                                          return (k, cb)) $ mapToList x


-- | Convert a MipsLabelBlock into a closed Hoopl Block.
toHooplClosed :: MipsLabelBlock (Inst O C) -> SimpleUniqueMonad (Block Inst C C)
toHooplClosed lb = blockJoinHead label <$> mipsComp
  where label = LABEL (blockLabel lb) (labelPrefix lb) (labelNum lb)
        mipsComp = toHooplBlock $ mipsBlock lb


-- | Compile a MIPS program to a Hoopl graph.
compileProg :: MipsProgram a -> SimpleUniqueMonad (Graph Inst C C)
compileProg prog = liftM (bodyGraph . mapFromList) $ compileProgList prog
