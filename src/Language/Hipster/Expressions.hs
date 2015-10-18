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


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Language.Hipster.Expressions where

import Language.Hipster.Instructions
import Language.Hipster.Language
import Language.Hipster.AST


instance Num (MipsBlock Register Register) where
  fromInteger i = do res <- newVar
                     li res i

  (+) b1 b2 = do r1 <- b1
                 r2 <- b2
                 res <- newVar
                 add res r1 r2

  (-) b1 b2 = do r1 <- b1
                 r2 <- b2
                 res <- newVar
                 sub res r1 r2

  (*) b1 b2 = do r1 <- b1
                 r2 <- b2
                 res <- newVar
                 mult r1 r2
                 mflo res

  negate b1 = do r1 <- b1
                 res <- newVar
                 sub res (Reg 0) r1

  
{-

Want to be able to just do this...

y <- newVar
lui y 32
x <- 2 * 3 + y

Which means 2 * 3 + y :: MipsBlock Register Register

Which is not ideal :(.

-}
