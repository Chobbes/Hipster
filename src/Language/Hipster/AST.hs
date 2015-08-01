module AST where

-- | All of the MIPS 32 hardware registers.
data MipsRegister
    = Zero
    | AT
    | V0
    | V1
    | A0
    | A1
    | A2
    | A3
    | T0
    | T1
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | S0
    | S1
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | T8
    | T9
    | K0
    | K1
    | GP
    | SP
    | FP
    | RA

type Id = Integer

-- | Data type for register allocation.
data Register
     = Unique Id  -- ^ Any general purpose MIPS 32 register can be allocated for this.
     | Specific MipsRegister -- ^ Specific register reserved.

data AST = Add Register Register
