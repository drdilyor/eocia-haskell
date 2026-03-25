module Target.Asm where

import Pre hiding (show)
import Data.Kind
import Data.Ix

data Argtype = Src | Dst deriving (Eq, Show, Read)
data Vartype = Avar | Aint deriving (Eq, Show, Read)

{- FOURMOLU_DISABLE -}
type AsmB :: Vartype -> Type
data AsmB v
  = Addq  (Arg Dst v) (Arg Src v)
  | Subq  (Arg Dst v) (Arg Src v)
  | Negq  (Arg Dst v)
  | Movq  (Arg Dst v) (Arg Src v)
  | Pushq (Arg Dst v)
  | Popq  (Arg Dst v)
  | Callq Text
  | Retq
  deriving (Eq, Show)

type Asm = AsmB Aint
type AsmVar = AsmB Avar

deriving instance Read Asm
deriving instance Read AsmVar

type Arg :: Argtype -> Vartype -> Type
data Arg atype vartype where
  Imm :: Int ->          Arg Src v
  Var :: Text ->         Arg a   Avar
  Reg :: Reg ->          Arg a   v
  Deref :: Reg -> Int -> Arg a   v

deriving instance Eq (Arg a v)
deriving instance Show (Arg a v)
{- FOURMOLU_ENABLE -}

data Reg
  = Rax
  | Rsp
  | Rbp
  | Rbx
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Show, Read, Enum, Ord, Ix)

rax, rsp, rbp, rbx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: Arg a v
rax = Reg Rax
rsp = Reg Rsp
rbp = Reg Rbp
rbx = Reg Rbx
rsi = Reg Rsi
rdi = Reg Rdi
r8 = Reg R8
r9 = Reg R9
r10 = Reg R10
r11 = Reg R11
r12 = Reg R12
r13 = Reg R13
r14 = Reg R14
r15 = Reg R15

instance Read (Arg Src Aint) where
  readPrec = parens $ choice
    [ prec 10 do
        Ident "Imm" <- lexP
        i <- step readPrec
        pure (Imm i)
    , prec 10 do
        Ident "Reg" <- lexP
        r <- step readPrec
        pure (Reg r)
    , prec 10 do
        Ident "Deref" <- lexP
        i <- step readPrec
        r <- step readPrec
        pure (Deref i r)
    ]

instance Read (Arg Dst Aint) where
  readPrec = parens $ choice
    [ prec 10 do
        Ident "Reg" <- lexP
        r <- step readPrec
        pure (Reg r)
    , prec 10 do
        Ident "Deref" <- lexP
        i <- step readPrec
        r <- step readPrec
        pure (Deref i r)
    ]

instance Read (Arg Src Avar) where
  readPrec = parens $ choice
    [ prec 10 do
        Ident "Imm" <- lexP
        i <- step readPrec
        pure (Imm i)
    , prec 10 do
        Ident "Var" <- lexP
        t <- step readPrec
        pure (Var t)
    , prec 10 do
        Ident "Reg" <- lexP
        r <- step readPrec
        pure (Reg r)
    , prec 10 do
        Ident "Deref" <- lexP
        i <- step readPrec
        r <- step readPrec
        pure (Deref i r)
    ]

instance Read (Arg Dst Avar) where
  readPrec = parens $ choice
    [ prec 10 do
        Ident "Var" <- lexP
        t <- step readPrec
        pure (Var t)
    , prec 10 do
        Ident "Reg" <- lexP
        r <- step readPrec
        pure (Reg r)
    , prec 10 do
        Ident "Deref" <- lexP
        i <- step readPrec
        r <- step readPrec
        pure (Deref i r)
    ]
