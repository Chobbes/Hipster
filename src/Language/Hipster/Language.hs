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

module Language.Hipster.Language where

import Language.Hipster.AST
import Language.Hipster.Instructions
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Compiler.Hoopl


-- | Allocate a new 32-bit value (any register will do).
newVar :: MipsBlock Register Register
newVar = do unique <- lift freshUnique
            return $ Var unique

-- Arithmetic
add :: Dest -> Source -> Source -> MipsBlock Register Register
add d s t = liftF (ADD d s t, d)

addu :: Dest -> Source -> Source -> MipsBlock Register Register
addu d s t = liftF (ADDU d s t, d)

addi :: Dest -> Source -> Immediate -> MipsBlock Register Register
addi d s i = liftF (ADDI d s i, d)

addiu :: Dest -> Source -> Immediate -> MipsBlock Register Register
addiu d s i = liftF (ADDIU d s i, d)

sub :: Dest -> Source -> Source -> MipsBlock Register Register
sub d s t = liftF (SUB d s t, d)

subu :: Dest -> Source -> Source -> MipsBlock Register Register
subu d s t = liftF (SUBU d s t, d)

subi :: Dest -> Source -> Immediate -> MipsBlock Register Register
subi d s i = liftF (SUBI d s i, d)

subiu :: Dest -> Source -> Immediate -> MipsBlock Register Register
subiu d s i = liftF (SUBIU d s i, d)

mult :: Source -> Source -> MipsBlock Register ()
mult a b = liftF (MULT a b, ())

multu :: Source -> Source -> MipsBlock Register ()
multu a b = liftF (MULTU a b, ())

div :: Source -> Source -> MipsBlock Register ()
div a b = liftF (DIV a b, ())

divu :: Source -> Source -> MipsBlock Register ()
divu a b = liftF (DIVU a b, ())


-- Shifts
sll :: Dest -> Source -> Immediate -> MipsBlock Register Register
sll d s i = liftF (SLL d s i, d)

sllv :: Dest -> Source -> Source -> MipsBlock Register Register
sllv d s t = liftF (SLLV d s t, d)

sra :: Dest -> Source -> Immediate -> MipsBlock Register Register
sra d s i = liftF (SRA d s i, d)

srl :: Dest -> Source -> Immediate -> MipsBlock Register Register
srl d s i = liftF (SRL d s i, d)

srlv :: Dest -> Source -> Source -> MipsBlock Register Register
srlv d s t = liftF (SRLV d s t, d)


-- Logic
and :: Dest -> Source -> Source -> MipsBlock Register Register
and d s t = liftF (AND d s t, d)

andi :: Dest -> Source -> Immediate -> MipsBlock Register Register
andi d s i = liftF (ANDI d s i, d)

or :: Dest -> Source -> Source -> MipsBlock Register Register
or d s t = liftF (OR d s t, d)

ori :: Dest -> Source -> Immediate -> MipsBlock Register Register
ori d s i = liftF (ORI d s i, d)

xor :: Dest -> Source -> Source -> MipsBlock Register Register
xor d s t = liftF (XOR d s t, d)

xori :: Dest -> Source -> Immediate -> MipsBlock Register Register
xori d s i = liftF (XORI d s i, d)


-- Sets
slt :: Dest -> Source -> Source -> MipsBlock Register Register
slt d s t = liftF (SLT d s t, d)

slti :: Dest -> Source -> Immediate -> MipsBlock Register Register
slti d s i = liftF (SLTI d s i, d)

sltu :: Dest -> Source -> Source -> MipsBlock Register Register
sltu d s t = liftF (SLTU d s t, d)

sltiu :: Dest -> Source -> Immediate -> MipsBlock Register Register
sltiu d s i = liftF (SLTIU d s i, d)


-- Branches and jumps
j :: Label -> MipsBlock Register (Inst Register O C)
j = return . J

jal :: Label -> MipsBlock Register (Inst Register O C)
jal = return . JAL

-- Loads
lb :: Dest -> Immediate -> Source -> MipsBlock Register Register
lb d i s = liftF (LB d i s, d)

lui :: Dest -> Immediate -> MipsBlock Register Register
lui d i = liftF (LUI d i, d)

lw :: Dest -> Immediate -> Source -> MipsBlock Register Register
lw d i s = liftF (LW d i s, d)

mfhi :: Dest -> MipsBlock Register Register
mfhi d = liftF (MFHI d, d)

mflo :: Dest -> MipsBlock Register Register
mflo d = liftF (MFLO d, d)

-- Stores
sb :: Source -> Immediate -> Source -> MipsBlock Register Register
sb s o a = liftF (SB s o a, s)

sw :: Source -> Immediate -> Source -> MipsBlock Register Register
sw s o a = liftF (SW s o a, s)

-- Noop
noop :: MipsBlock Register ()
noop = liftF (NOOP, ())

-- System calls
syscall :: MipsBlock Register ()
syscall = liftF (SYSCALL, ())

-- Comments
comment :: String -> MipsBlock Register ()
comment str = liftF (COMMENT str, ())

