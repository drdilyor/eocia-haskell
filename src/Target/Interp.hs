-- | slop
module Target.Interp (
  interpAsm,
  interpAsmVar,
  InterpAsmError (..),
) where

import Data.HashMap.Strict qualified as Map
import Data.Vector qualified as V
import Effects.Lio
import Pre
import Target.Asm
import Target.Program

data InterpAsmError
  = UnknownLabel Text
  | StackUnderflow
  | InvalidMemoryAccess Int
  | UnimplementedInstruction String
  deriving (Eq, Show)

data InterpAsmState = InterpAsmState
  { regs :: V.Vector Int
  , mem :: Map.HashMap Int Int
  , vars :: Map.HashMap Text Int
  , pc :: Int
  }
  deriving (Show)

interpAsm :: (Lio :> es, Error InterpAsmError :> es) => Program -> Eff es Int
interpAsm (Program _ blocks) = interpAsmB blocks "_start"

interpAsmVar :: (Lio :> es, Error InterpAsmError :> es) => [(Text, [AsmVar])] -> Text -> Eff es Int
interpAsmVar blocks start = interpAsmB blocks start

interpAsmB
  :: (Lio :> es, Error InterpAsmError :> es)
  => [(Text, [AsmB v])]
  -> Text
  -> Eff es Int
interpAsmB blocks start = do
  let (asms, labels) = flatten blocks
  let initState =
        InterpAsmState
          { regs = V.replicate 16 0 V.// [(fromEnum Rsp, 1000000), (fromEnum Rbp, 1000000)]
          , mem = Map.fromList [(1000000, -1)] -- return address to halt
          , vars = Map.empty
          , pc = fromMaybe 0 (Map.lookup start labels)
          }
  evalState initState (execute asms labels)

flatten :: [(Text, [AsmB v])] -> (V.Vector (AsmB v), Map.HashMap Text Int)
flatten blocks = (V.fromList (concatMap snd blocks), Map.fromList labelOffsets)
  where
    offsets = scanl (+) 0 (map (length . snd) blocks)
    labelOffsets = zip (map fst blocks) offsets

execute
  :: (State InterpAsmState :> es, Lio :> es, Error InterpAsmError :> es)
  => V.Vector (AsmB v)
  -> Map.HashMap Text Int
  -> Eff es Int
execute asms labels = do
  currPc <- gets (.pc)
  if currPc < 0 || currPc >= V.length asms
    then getReg Rax
    else do
      let inst = asms V.! currPc
      modify $ \s -> s{pc = currPc + 1}
      case inst of
        Addq dst src -> do
          vSrc <- evalArg src
          vDst <- evalArg dst
          writeArg dst (vDst + vSrc)
          execute asms labels
        Subq dst src -> do
          vSrc <- evalArg src
          vDst <- evalArg dst
          writeArg dst (vDst - vSrc)
          execute asms labels
        Negq dst -> do
          vDst <- evalArg dst
          writeArg dst (-vDst)
          execute asms labels
        Movq dst src -> do
          vSrc <- evalArg src
          writeArg dst vSrc
          execute asms labels
        Pushq src -> do
          val <- evalArg src
          rspVal <- getReg Rsp
          let newRsp = rspVal - 8
          setReg Rsp newRsp
          writeMem newRsp val
          execute asms labels
        Popq dst -> do
          rspVal <- getReg Rsp
          val <- readMem rspVal
          let newRsp = rspVal + 8
          setReg Rsp newRsp
          writeArg dst val
          execute asms labels
        Callq label -> case label of
          "print_int" -> do
            val <- getReg Rdi
            lioPrintLine (show val)
            execute asms labels
          "input_int" -> do
            line <- lioInputLine
            case readMaybe (unpack line) of
              Just i -> setReg Rax i >> execute asms labels
              Nothing -> throwError $ UnimplementedInstruction "Invalid input for input_int"
          _ -> case Map.lookup label labels of
            Just target -> do
              retPc <- gets (.pc)
              rspVal <- getReg Rsp
              let newRsp = rspVal - 8
              setReg Rsp newRsp
              writeMem newRsp retPc
              modify $ \s -> s{pc = target}
              execute asms labels
            Nothing -> throwError $ UnknownLabel label
        Retq -> do
          rspVal <- getReg Rsp
          retPc <- readMem rspVal
          setReg Rsp (rspVal + 8)
          if retPc == -1
            then getReg Rax
            else do
              modify $ \s -> s{pc = retPc}
              execute asms labels

getReg :: (State InterpAsmState :> es) => Reg -> Eff es Int
getReg r = gets (\s -> s.regs V.! fromEnum r)

setReg :: (State InterpAsmState :> es) => Reg -> Int -> Eff es ()
setReg r v = modify $ \s -> s{regs = s.regs V.// [(fromEnum r, v)]}

readMem :: (State InterpAsmState :> es, Error InterpAsmError :> es) => Int -> Eff es Int
readMem addr = do
  m <- gets (.mem)
  case Map.lookup addr m of
    Just v -> pure v
    Nothing -> throwError $ InvalidMemoryAccess addr

writeMem :: (State InterpAsmState :> es) => Int -> Int -> Eff es ()
writeMem addr v = modify $ \s -> s{mem = Map.insert addr v s.mem}

evalArg :: (State InterpAsmState :> es, Error InterpAsmError :> es) => Arg a v -> Eff es Int
evalArg = \case
  Imm i -> pure i
  Reg r -> getReg r
  Deref r off -> do
    base <- getReg r
    readMem (base + off)
  Var x -> gets (\s -> Map.lookupDefault 0 x s.vars)

writeArg :: (State InterpAsmState :> es, Error InterpAsmError :> es) => Arg Dst v -> Int -> Eff es ()
writeArg arg v = case arg of
  Reg r -> setReg r v
  Deref r off -> do
    base <- getReg r
    writeMem (base + off) v
  Var x -> modify $ \s -> s{vars = Map.insert x v s.vars}
