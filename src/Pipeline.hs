module Pipeline where

import Effects.Gensym
import Lang
import Pre

msbind :: MStmt -> Text -> MStmt -> MStmt
msbind s x j = flip cata s \case
  MExprF e -> MLet x e j
  s' -> embed s'

removeComplexOperands :: forall es. (Gensym :> es) => L -> Eff es ML
removeComplexOperands (Module ss) = MModule <$> rcoStmt ss
 where
  rcoExpr :: Gensym :> es => Exp -> Eff es MStmt
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
