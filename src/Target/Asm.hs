module Target.Asm where

import Data.Hashable
import Data.Ix
import Data.Kind
import Pre

data Argtype = Src | Dst deriving (Eq, Show, Read)
data Vartype = Avar | Aint deriving (Eq, Show, Read)

{- FOURMOLU_DISABLE -}
type AsmB :: Vartype -> Type
data AsmB v
  = Addq  (Arg Dst v) (Arg Src v)
  | Subq  (Arg Dst v) (Arg Src v)
  | Negq  (Arg Dst v)
  | Movq  (Arg Dst v) (Arg Src v)
  | Pushq (Arg Src v)
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

instance IsString (Arg a Avar) where
  fromString = Var . pack

data Reg
  = Rax
  | Rcx
  | Rdx
  | Rbx
  | Rsp
  | Rbp
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
  deriving (Eq, Show, Read, Enum, Ord, Ix, Generic, Hashable)

rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: Arg a v
(rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15) =
  (Reg Rax, Reg Rcx, Reg Rdx, Reg Rbx, Reg Rsp, Reg Rbp, Reg Rsi, Reg Rdi, Reg R8, Reg R9, Reg R10, Reg R11, Reg R12, Reg R13, Reg R14, Reg R15)

data SaveType = SaveOuter | SaveInner
  deriving (Eq, Show, Read, Enum)

saveType :: Reg -> SaveType
saveType = \case
  Rax -> SaveOuter
  Rcx -> SaveOuter
  Rdx -> SaveOuter
  Rbx -> SaveInner
  Rsp -> SaveInner
  Rbp -> SaveInner
  Rsi -> SaveOuter
  Rdi -> SaveOuter
  R8 -> SaveOuter
  R9 -> SaveOuter
  R10 -> SaveOuter
  R11 -> SaveOuter
  R12 -> SaveInner
  R13 -> SaveInner
  R14 -> SaveInner
  R15 -> SaveInner

outerSavedRegs, innerSavedRegs :: [Reg]
(outerSavedRegs, innerSavedRegs) =
  partition ((SaveOuter ==) . saveType) [Rax .. R15]

argumentRegs :: [Reg]
argumentRegs = [Rdi, Rsi, Rdx, Rcx, R8, R9]

returnRegs :: [Reg]
returnRegs = [Rax, Rdx]

printAsm :: [AsmB v] -> Text
printAsm = unlines . map each
 where
  each = \case
    Addq a b -> "add " <> printArg a <> ", " <> printArg b
    Subq a b -> "sub " <> printArg a <> ", " <> printArg b
    Negq a -> "neg " <> printArg a
    Movq a b -> "mov " <> printArg a <> ", " <> printArg b
    Pushq b -> "push " <> printArg b
    Popq a -> "pop " <> printArg a
    Callq n -> "call " <> n
    Retq -> "ret"

printArg :: Arg a v -> Text
printArg (Imm x) = show x
printArg (Reg x) = toLower $ show x
printArg (Deref x o) = "qword ptr [" <> toLower (show x) <> offsetText <> "]"
 where
  offsetText
    | o > 0 = " + " <> show o
    | o < 0 = " - " <> show (-o)
    | otherwise = ""
printArg (Var x) = show x

instance Read (Arg Src Aint) where
  readPrec =
    parens . choice $
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
  readPrec =
    parens . choice $
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
  readPrec =
    parens . choice $
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
  readPrec =
    parens . choice $
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
