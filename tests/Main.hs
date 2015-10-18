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

{-# LANGUAGE RecursiveDo #-}

import Test.Hspec
import Compiler.Hoopl
import Language.Hipster.AST
import Language.Hipster.Instructions
import Language.Hipster.Language
import Control.Monad
import LinearScan.Hoopl
import LinearScan
import Unsafe.Coerce


printAllocRes :: (String, Either [String] (Graph (Inst Register) C C)) -> IO ()
printAllocRes (str, Right g) = putStrLn str >> putStrLn (showGraph show g)
printAllocRes (str, Left fails) = putStrLn str >> print fails

main :: IO ()
main = do putStrLn . showGraph show $ runSimpleUniqueMonad . compileProg $ labelTest
          putStrLn . showGraph show $ runSimpleUniqueMonad . compileProg $ labelTest'
          printAllocRes . allocateHoopl 5 0 4 VerifyEnabled (unsafeCoerce (1 :: Int)) $ runSimpleUniqueMonad . compileProg $ labelTest'
          hspec $
            describe "AST var test" $
              it "newVar composition test" $
                let addNews = do { a <- newVar; b <- newVar; c <- newVar; add a b c }
                    comp = runSimpleUniqueMonad . compileBlock $ replicateM_ 3 addNews
                    insts =  [ ADD (Var 1) (Var 2) (Var 3)
                             , ADD (Var 4) (Var 5) (Var 6)
                             , ADD (Var 7) (Var 8) (Var 9)]
                in comp `shouldBe` insts

labelTest :: MipsProgram Register ()
labelTest = mdo l1 <- newBB "l1" $ do
                  res <- newVar
                  x <- newVar
                  y <- newVar
                  add res x y
                  j l1
                l2 <- newBB "l2" $ do
                  res <- newVar
                  x <- newVar
                  y <- newVar
                  sub res x y
                  j l1
                return ()

labelTest' :: MipsProgram Register ()
labelTest' = mdo l1 <- newBB "l1" $ do
                   res <- newVar
                   x <- newVar
                   y <- newVar
                   z <- newVar
                   w <- newVar
                   u <- newVar
                   lui x 1
                   lui y 2
                   lui z 3
                   lui w 4
                   lui u 5
                   add res x y
                   add res res x
                   add res res y
                   add res res z
                   add res res w
                   add res res u
                   j l2
                 l2 <- newBB "l2" $ do
                   res <- newVar
                   x <- newVar
                   y <- newVar
                   lui x 3
                   lui y 4
                   sub res x y
                   j l1
                 return ()
