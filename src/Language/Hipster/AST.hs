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
     deriving (Show)

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
    deriving (Show)

-- | MIPS assembly is essentially a list of instructions, which is what
-- the Free monad gives us.
type MipsASM a = FreeT ((,) Inst) SimpleUniqueMonad a


-- | Convert a MIPS program to a list of instructions.
compile :: MipsASM a -> [Inst]
compile = runSimpleUniqueMonad . compile'
  where compile' free =
               do x <- runFreeT free
                  case x of
                    (Pure a) -> return []
                    (Free (inst, fs)) -> fmap (inst :) $ compile' fs


-- | Allocate a new 32-bit value (any register will do).
newVar :: MipsASM Register
newVar = do unique <- lift $ freshUnique
            return $ Var unique


add :: Register -> Register -> Register -> MipsASM Register
add d s t = liftF (ADD d s t, d)

sub :: Register -> Register -> Register -> MipsASM Register
sub d s t = liftF (SUB d s t, d)


-- | An actual MIPS program contains labels.
type LabelMap = M.Map String Integer

type MipsProgram a = FreeT ((,) )
