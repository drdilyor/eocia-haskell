module Pipeline where

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
selectInstructions (MModule ss) = execState [] $ siStmt ss
 where
  atomToArg :: Atom -> Arg Src Avar
  atomToArg (Lit x) = Imm x
  atomToArg (Name x) = Var x

  siBinOp Add = Addq
  siBinOp Sub = Subq
  siUnaryOp USub = Negq

  emit :: [AsmVar] -> Eff (State [AsmVar] : es) ()
  emit x = modify (<> x)

  siStmt (MLet x (MAtom y) k) = do
    emit [Movq (Var x) (atomToArg y)]
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
          , siBinOp op (Var x) (Var t)
          ]
        siStmt k
  siStmt (MLet x (MBinOp op y z) k) = do
    emit
      [ Movq (Var x) (atomToArg y)
      , siBinOp op (Var x) (atomToArg z)
      ]
    siStmt k
  siStmt (MLet x (MUnaryOp op (Name y)) k)
    | x == y = do
        emit [siUnaryOp op (Var x)]
        siStmt k
  siStmt (MLet x (MUnaryOp op y) k) = do
    t <- gensym "t"
    emit
      [ Movq (Var t) (atomToArg y)
      , siUnaryOp op (Var x)
      ]
    siStmt k
  siStmt (MLet x MInputInt k) = do
    emit
      [ Callq "read_int"
      , Movq (Var x) (Reg Rax)
      ]
    siStmt k
  siStmt (MPrint e k) = do
    emit
      [ Callq "read_int"
      , Movq (Reg Rax) (atomToArg e)
      ]
    siStmt k
  siStmt (MExpr (MAtom (Name t))) = do
    emit [Movq (Reg Rax) (Var t)]
  siStmt (MExpr e) = do
    t <- gensym "t"
    siStmt (MLet t e (MExpr (MAtom (Name t))))
