module Pipeline where

import Data.HashMap.Strict qualified as Map
import Effects.Gensym
import Lang
import Pre
import Target.Asm

msbind :: MStmt -> Text -> MStmt -> MStmt
msbind s x j = flip cata s \case
  MExprF e -> MLet x e j
  s' -> embed s'

removeComplexOperands :: forall es. (Gensym :> es) => L -> Eff es ML
removeComplexOperands (Module ss) = MModule <$> rcoStmt ss
 where
  rcoExpr :: (Gensym :> es) => Exp -> Eff es MStmt
  rcoExpr = \case
    Atom x -> pure $ MExpr (MAtom x)
    InputInt -> pure $ MExpr MInputInt
    UnaryOp op (Atom x) -> pure $ MExpr (MUnaryOp op x)
    UnaryOp op e -> do
      t <- gensym "t"
      s <- rcoExpr e
      pure $ msbind s t $ MExpr (MUnaryOp op (Name t))
    BinOp op (Atom x) (Atom y) -> pure $ MExpr (MBinOp op x y)
    BinOp op (Atom x) e2 -> do
      t <- gensym "t"
      s <- rcoExpr e2
      pure $ msbind s t $ MExpr (MBinOp op x (Name t))
    BinOp op e1 (Atom y) -> do
      t <- gensym "t"
      s <- rcoExpr e1
      pure $ msbind s t $ MExpr (MBinOp op (Name t) y)
    BinOp op e1 e2 -> do
      t1 <- gensym "t"
      t2 <- gensym "t"
      s1 <- rcoExpr e1
      s2 <- rcoExpr e2
      pure $ msbind s1 t1 $ msbind s2 t2 $ MExpr (MBinOp op (Name t1) (Name t2))

  rcoStmt :: Stmt -> Eff es MStmt
  rcoStmt (Expr e) = rcoExpr e
  rcoStmt (Print (Atom a) k) = MPrint a <$> rcoStmt k
  rcoStmt (Print e k) = do
    t <- gensym "t"
    s <- rcoExpr e
    k' <- rcoStmt k
    pure $ msbind s t $ MPrint (Name t) k'
  rcoStmt (Let x (Atom a) k) = MLet x (MAtom a) <$> rcoStmt k
  rcoStmt (Let x e k) = do
    s <- rcoExpr e
    k' <- rcoStmt k
    pure $ msbind s x k'

selectInstructions :: forall es. (Gensym :> es) => ML -> Eff es [AsmVar]
selectInstructions (MModule ss) = fmap reverse $ execState [] $ siStmt ss
 where
  emit :: [AsmVar] -> Eff (State [AsmVar] : es) ()
  emit x = modify (reverse x <>)

  siArg :: Atom -> Arg Src Avar
  siArg (Lit x) = Imm x
  siArg (Name x) = Var x

  siBinOp Add = Addq
  siBinOp Sub = Subq
  siUnaryOp USub = Negq

  siStmt (MLet x (MAtom y) k) = do
    emit [Movq (Var x) (siArg y)]
    siStmt k
  siStmt (MLet x (MBinOp op (Name y) (Name z)) k)
    | x == y = do
        emit [siBinOp op (Var x) (Var z)]
        siStmt k
    | x == z = do
        t <- gensym "t"
        emit
          [ Movq (Var t) (Var y)
          , siBinOp op (Var t) (Var z)
          , Movq (Var x) (Var t)
          ]
        siStmt k
  siStmt (MLet x (MBinOp op y z) k) = do
    emit
      [ Movq (Var x) (siArg y)
      , siBinOp op (Var x) (siArg z)
      ]
    siStmt k
  siStmt (MLet x (MUnaryOp op (Name y)) k)
    | x == y = do
        emit [siUnaryOp op (Var x)]
        siStmt k
  siStmt (MLet x (MUnaryOp op y) k) = do
    emit
      [ Movq (Var x) (siArg y)
      , siUnaryOp op (Var x)
      ]
    siStmt k
  siStmt (MLet x MInputInt k) = do
    emit
      [ Callq "input_int"
      , Movq (Var x) (Reg Rax)
      ]
    siStmt k
  siStmt (MPrint e k) = do
    emit
      [ Movq (Reg Rax) (siArg e)
      , Callq "print_int"
      ]
    siStmt k
  siStmt (MExpr (MAtom x)) = do
    emit [Movq (Reg Rax) (siArg x)]
  siStmt (MExpr e) = do
    t <- gensym "t"
    siStmt (MLet t e (MExpr (MAtom (Name t))))

data StackFrame = StackFrame
  { offsets :: Map.HashMap Text Int
  , size :: Int
  }

assignHomes :: forall es. [AsmVar] -> Eff es (Int, [Asm])
assignHomes asmvar = mdo
  (program, StackFrame{size}) <-
    runState (StackFrame Map.empty 0) $
      mapM instruction asmvar
  pure (size, program)
 where
  instruction :: AsmVar -> Eff (State StackFrame : es) Asm
  instruction (Movq a b) = Movq <$> argument a <*> argument b
  instruction (Addq a b) = Addq <$> argument a <*> argument b
  instruction (Subq a b) = Subq <$> argument a <*> argument b
  instruction (Negq a) = Negq <$> argument a
  instruction (Callq x) = pure $ Callq x
  instruction Retq = pure Retq

  argument :: Arg a Avar -> Eff (State StackFrame : es) (Arg a Aint)
  argument (Var x) = do
    sf <- get
    case Map.lookup x sf.offsets of
      Nothing -> do
        let o = - (sf.size + 8)
        put $ StackFrame (Map.insert x o sf.offsets) (sf.size + 8)
        pure (Deref Rbp o)
      Just o ->
        pure (Deref Rbp o)
  argument (Imm x) = pure $ Imm x
  argument (Reg x) = pure $ Reg x
  argument (Deref o x) = pure $ Deref o x

patchInstructions :: [Asm] -> [Asm]
patchInstructions =
  let patch inst (Deref a x) (Deref b y) =
        [ Movq (Reg Rax) (Deref b y)
        , inst (Deref a x) (Reg Rax)
        ]
      patch f x y = [f x y]
   in concatMap \case
        Movq a b -> patch Movq a b
        Addq a b -> patch Addq a b
        Subq a b -> patch Subq a b
        Callq x -> [Callq x]
        Negq a -> [Negq a]
        Retq -> [Retq]
