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


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Integer -- ^ Specific register reserved.
     deriving (Show, Eq)

-- | Indicates a register that we write to.
type Dest = Register

-- | Indicates a register that we read from.
type Source = Register

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Type for keeping track of unique MIPS labels.
type MipsLabel = Int

-- | Data type representing MIPS instructions.
data Inst e x where
    LABEL :: MipsLabel -> Inst C O
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
    JMP :: MipsLabel -> Inst O C
    COMMENT :: String -> Inst O O
    INLINE_COMMENT :: Inst e x -> String -> Inst e x

deriving instance Show (Inst e x)
deriving instance Eq (Inst e x)

instance NonLocal Inst where
  entryLabel (LABEL l) = mipsToLabel l
  successors (JMP l) = [mipsToLabel l]


-- | Convert a MipsLabel into a Hoopl label.
mipsToLabel :: MipsLabel -> Label
mipsToLabel = unsafeCoerce  -- Label is just a newtype of Unique, which is a newtype of Int.

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

jmp :: MipsLabel -> MipsBlock (Inst O C)
jmp l = return $ JMP l


-- | Keeps track of label numbers, and unique Hoopl labelings.
type LabelMonad = State (M.Map String Int, MipsLabel)

getLabel :: String -> LabelMonad MipsLabel
getLabel name = do strNum <- gets $ fromMaybe 0 . M.lookup name . fst
                   unique <- gets $ succ . snd
                   modify $ \(m,_) -> (M.insert name (strNum+1) m, unique)
                   return unique

-- Need a state monad which keeps track of the labels.
type MipsProgram = StateT (IM.IntMap (MipsLabelBlock (Inst O C))) LabelMonad


-- | A basic block for a MIPS program.
data MipsLabelBlock a = MipsLabelBlock { blockLabel :: MipsLabel  -- ^ Unique Hoopl label.
                                       , labelPrefix :: String -- ^ Label prefix string.
                                       , labelNum :: Int  -- ^ May be multiple labels with the same prefix.
                                       , mipsBlock :: MipsBlock a -- ^ Actual block of MIPS instructions.
                                       }

-- | Create a new basic block with a named label.
newBB :: String -> MipsBlock (Inst O C) -> MipsProgram MipsLabel
newBB name block = do label <- lift $ getLabel name
                      let newBlock = MipsLabelBlock label name 0 block
                      
                      modify $ IM.insert label newBlock
                      return label


-- | Convert a MIPS block to a list of instructions.
compileProg :: MipsProgram a -> SimpleUniqueMonad [(MipsLabel, String, [Inst O O])]
compileProg = flip evalState (M.empty, 0) . compile'
  where compile' state =
          do x <- execStateT state IM.empty
             return . mapM (\(k, v) -> do cb <- compileBlock $ mipsBlock v
                                          return (k, labelPrefix v, cb)) $ IM.toList x


toHooplClosed :: MipsLabelBlock (Inst O C) -> SimpleUniqueMonad (Block Inst C C)
toHooplClosed lb = blockJoinHead label <$> mipsComp
  where label = LABEL $ blockLabel lb
        mipsComp = toHooplBlock $ mipsBlock lb

{-
compileGraph :: MipsProgram a -> SimpleUniqueMonad (Graph (MipsLabelBlock (Inst)) C C)
compileGraph = undefined
-}
