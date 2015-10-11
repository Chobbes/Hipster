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
{-# LANGUAGE MultiParamTypeClasses #-}


module Language.Hipster.AST where

import Language.Hipster.Instructions
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


-- | MIPS assembly is essentially a list of instructions, which is what
-- the Free monad gives us.
type MipsBlock v a = FreeT ((,) (Inst v O O)) SimpleUniqueMonad a


-- | Convert a MIPS block to a list of instructions.
compileBlock :: MipsBlock v a -> SimpleUniqueMonad [Inst v O O]
compileBlock = compile'
  where compile' free =
               do x <- runFreeT free
                  case x of
                    (Pure _) -> return []
                    (Free (inst, fs)) -> (inst :) <$> compile' fs


-- | Convert a MIPS block to a Hoopl block.
toHooplBlock :: MipsBlock v (Inst v O C) -> SimpleUniqueMonad (Block (Inst v) O C)
toHooplBlock free = do fNode <- runFreeT free
                       case fNode of
                         (Pure close) -> return $ BlockOC emptyBlock close
                         (Free (inst, fs)) -> blockCons inst <$> toHooplBlock fs


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
type MipsProgram v = StateT (LabelMap (MipsLabelBlock v (Inst v O C))) LabelMonad


-- | A basic block for a MIPS program.
data MipsLabelBlock v a = MipsLabelBlock { blockLabel :: Label  -- ^ Unique Hoopl label.
                                         , labelPrefix :: String -- ^ Label prefix string.
                                         , labelNum :: Int  -- ^ May be multiple labels with the same prefix.
                                         , mipsBlock :: MipsBlock v a -- ^ Actual block of MIPS instructions.
                                       }


-- | Create a new basic block with a named label.
newBB :: String -> MipsBlock v (Inst v O C) -> MipsProgram v Label
newBB name block = do label <- lift $ getLabel name
                      let newBlock = MipsLabelBlock label name 0 block
                      
                      modify $ mapInsert label newBlock
                      return label


-- | Convert a MIPS program to an assoc list of labels and their blocks.
compileProgList :: MipsProgram v a -> SimpleUniqueMonad [(Label, Block (Inst v) C C)]
compileProgList = flip evalState (M.empty, intToLabel 0) . compile'
  where compile' state =
          do x <- execStateT state mapEmpty
             return . mapM (\(k, v) -> do cb <- toHooplClosed v
                                          return (k, cb)) $ mapToList x


-- | Convert a MipsLabelBlock into a closed Hoopl Block.
toHooplClosed :: MipsLabelBlock v (Inst v O C) -> SimpleUniqueMonad (Block (Inst v) C C)
toHooplClosed lb = blockJoinHead label <$> mipsComp
  where label = LABEL (blockLabel lb) (labelPrefix lb) (labelNum lb)
        mipsComp = toHooplBlock $ mipsBlock lb


-- | Compile a MIPS program to a Hoopl graph.
compileProg :: MipsProgram v a -> SimpleUniqueMonad (Graph (Inst v) C C)
compileProg prog = liftM (bodyGraph . mapFromList) $ compileProgList prog
