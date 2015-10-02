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

module Language.Hipster.AST where

-- import Control.Monad.Free hiding (Free, Pure)
import Control.Monad.Trans.Free  -- hiding (Free, Pure)
import Control.Monad.Trans.Class
import Compiler.Hoopl
import Data.Map as M


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Integer -- ^ Specific register reserved.
     deriving (Show, Eq)

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Data type representing MIPS instructions.
data Inst
    = ADD Register Register Register
    | ADDU Register Register Register
    | ADDI Register Register Immediate
    | ADDIU Register Register Immediate
    | SUB Register Register Register
    | SUBU Register Register Register
    | SUBI Register Register Immediate
    | SUBIU Register Register Immediate
    | MULT Register Register
    | MULTU Register Register
    | DIV Register Register
    | DIVU Register Register
    deriving (Show, Eq)

-- | MIPS assembly is essentially a list of instructions, which is what
-- the Free monad gives us.
type MipsBlock a = FreeT ((,) Inst) SimpleUniqueMonad a


-- | Convert a MIPS block to a list of instructions.
compileBlock :: MipsBlock a -> [Inst]
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

-- type MipsProgram a = FreeT ((,) )
