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

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.Hipster.Registers where

import Compiler.Hoopl


-- | Data type for register allocation.
data Register
     = Var Unique  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Reg Int -- ^ Specific register reserved.
     deriving (Eq)

instance Show Register where
  show (Var n) = "(Var " ++ show n ++ ")"
  show (Reg n) = regToStr n

-- | Indicates a register that we write to.
type Dest = Register

-- | Indicates a register that we read from.
type Source = Register

-- | Immediate values in MIPS.
type Immediate = Integer

-- | Data type for physical register locations.
newtype Physical = Phys Int

instance Show Physical where
  show (Phys n) = regToStr n

regToStr :: Int -> String
regToStr n
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
    | otherwise = error $ "Invalid register: " ++ show n

zero = Reg 0
at = Reg 1
v0 = Reg 2
v1 = Reg 3
a0 = Reg 4
a1 = Reg 5
a2 = Reg 6
a3 = Reg 7
t0 = Reg 8
t1 = Reg 9
t2 = Reg 10
t3 = Reg 11
t4 = Reg 12
t5 = Reg 13
t6 = Reg 14
t7 = Reg 15
s0 = Reg 16
s1 = Reg 17
s2 = Reg 18
s3 = Reg 19
s4 = Reg 20
s5 = Reg 21
s6 = Reg 22
s7 = Reg 23
t8 = Reg 24
t9 = Reg 25
k0 = Reg 26
k1 = Reg 27
gp = Reg 28
sp = Reg 29
fp = Reg 30
ra = Reg 31
