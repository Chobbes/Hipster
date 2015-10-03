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
import Compiler.Hoopl hiding (LabelMap)
import Data.Map as M


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Integer -- ^ Specific register reserved.
     deriving (Show, Eq)

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Data type representing MIPS instructions.
data Inst e x where
    ADD :: Register -> Register -> Register -> Inst O O
    ADDU :: Register -> Register -> Register -> Inst O O
    ADDI :: Register -> Register -> Immediate -> Inst O O
    ADDIU :: Register -> Register -> Immediate -> Inst O O
    SUB :: Register -> Register -> Register -> Inst O O
    SUBU :: Register -> Register -> Register -> Inst O O
    SUBI :: Register -> Register -> Immediate -> Inst O O
    SUBIU :: Register -> Register -> Immediate -> Inst O O
    MULT :: Register -> Register -> Inst O O
    MULTU :: Register -> Register -> Inst O O
    DIV :: Register -> Register -> Inst O O
    DIVU :: Register -> Register -> Inst O O
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
                    (Pure a) -> return []
                    (Free (inst, fs)) -> (inst :) <$> compile' fs


-- | Allocate a new 32-bit value (any register will do).
newVar :: MipsBlock Register
newVar = do unique <- lift freshUnique
            return $ Var unique


add :: Register -> Register -> Register -> MipsBlock Register
add d s t = liftF (ADD d s t, d)

sub :: Register -> Register -> Register -> MipsBlock Register
sub d s t = liftF (SUB d s t, d)


-- | An actual MIPS program contains labels to basic blocks.
type LabelMap = M.Map String Integer

-- Need a state monad which keeps track of the labels.
type LabelState = State LabelMap

-- type MipsProgram a = FreeT ((,) LabelMap) MipsBlock a
