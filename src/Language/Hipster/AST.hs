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

-- | Data type representing MIPS instructions.
data Inst e x where
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
    JMP :: Int -> Inst O C
    COMMENT :: String -> Inst O O
    INLINE_COMMENT :: Inst e x -> String -> Inst e x

deriving instance Show (Inst e x)
deriving instance Eq (Inst e x)

-- | MIPS assembly is essentially a list of instructions, which is what
-- the Free monad gives us.
type MipsBlock a = FreeT ((,) (Inst O O)) SimpleUniqueMonad a

-- | Convert a MIPS block to a list of instructions.
compileBlock :: MipsBlock a -> [Inst O O]
compileBlock = runSimpleUniqueMonad . compile'
  where compile' free =
               do x <- runFreeT free
                  case x of
                    (Pure _) -> return []
                    (Free (inst, fs)) -> (inst :) <$> compile' fs


-- | Allocate a new 32-bit value (any register will do).
newVar :: MipsBlock Register
newVar = do unique <- lift freshUnique
            return $ Var unique


add :: Dest -> Source -> Source -> MipsBlock Register
add d s t = liftF (ADD d s t, d)

sub :: Dest -> Source -> Source -> MipsBlock Register
sub d s t = liftF (SUB d s t, d)

jmp :: Int -> MipsBlock (Inst O C)
jmp l = return $ JMP l


-- | Keeps track of label numbers, and unique Hoopl labelings.
type LabelMonad = State (M.Map String Int, Int)

getLabel :: String -> LabelMonad Int
getLabel name = do strNum <- gets $ fromMaybe 0 . M.lookup name . fst
                   unique <- gets $ succ . snd
                   modify $ \(m,_) -> (M.insert name (strNum+1) m, unique)
                   return unique

-- Need a state monad which keeps track of the labels.
type MipsProgram = StateT (IM.IntMap (MipsLabelBlock (Inst O C))) LabelMonad


-- | A basic block for a MIPS program.
data MipsLabelBlock a = MipsLabelBlock { blockLabel :: Int  -- ^ Unique Hoopl label.
                                       , labelPrefix :: String -- ^ Label prefix string.
                                       , labelNum :: Int  -- ^ May be multiple labels with the same prefix.
                                       , mipsBlock :: MipsBlock a -- ^ Actual block of MIPS instructions.
                                       }

newBB :: String -> MipsBlock (Inst O C) -> MipsProgram Int
newBB name block = do label <- lift $ getLabel name
                      let newBlock = MipsLabelBlock label name 0 block
                      
                      modify $ IM.insert label newBlock
                      return label

-- | Convert a MIPS block to a list of instructions.
compileProg :: MipsProgram a -> [(Int, String, [Inst O O])]
compileProg = flip evalState (M.empty, 0) . compile'
  where compile' state =
          do x <- execStateT state IM.empty
             return . map (\(k, v) -> (k, labelPrefix v, compileBlock $ mipsBlock v)) $ IM.toList x
