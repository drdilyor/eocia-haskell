{-# LANGUAGE TemplateHaskell #-}

module Lang where

import Effects.Lio
import Pre

newtype L = Module [Stmt]
  deriving (Eq, Show, Read)

data Stmt
  = Print Exp
  | Expr Exp
  deriving (Eq, Show, Read)

data Exp
  = Constant Int
  | InputInt
  | UnaryOp UnaryOp Exp
  | BinOp BinOp Exp Exp
  deriving (Eq, Show, Read)

data UnaryOp
  = USub
  deriving (Eq, Show, Read)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show, Read)

newtype InterpError
  = InvalidInput Text
  deriving (Eq, Show)

makeBaseFunctor ''L
makeBaseFunctor ''Stmt
makeBaseFunctor ''Exp

interpL :: (Lio :> es, Error InterpError :> es) => L -> Eff es ()
interpL (Module ss) = mapM_ interpStmt ss

interpStmt :: (Lio :> es, Error InterpError :> es) => Stmt -> Eff es ()
interpStmt (Print e) = lioPrintLine . show =<< interpExp e
interpStmt (Expr e) = void $ interpExp e

interpExp :: forall es. (Lio :> es, Error InterpError :> es) => Exp -> Eff es Int
interpExp = cata \case
  (ConstantF x) -> pure x
  (UnaryOpF op x) -> interpUnaryOp op <$> x
  (BinOpF op x y) -> interpBinOp op <$> x <*> y
  InputIntF ->
    lioInputLine >>= \case
      (readMaybe . unpack -> Just x) -> pure x
      s -> throwError . InvalidInput $ "couldn't parse " <> show s

interpUnaryOp :: UnaryOp -> Int -> Int
interpUnaryOp USub = negate

interpBinOp :: BinOp -> Int -> Int -> Int
interpBinOp Add = (+)
interpBinOp Sub = (-)

peL :: L -> L
peL (Module ss) = Module (map peStmt ss)

peStmt :: Stmt -> Stmt
peStmt (Print e) = Print (peExp e)
peStmt (Expr e) = Expr (peExp e)

peExp :: Exp -> Exp
peExp = cata \case
  (ConstantF x) -> Constant x
  InputIntF -> InputInt
  (UnaryOpF op (Constant x)) -> Constant $ interpUnaryOp op x
  (UnaryOpF op e) -> UnaryOp op e
  (BinOpF op (Constant x) (Constant y)) -> Constant $ interpBinOp op x y
  (BinOpF op e1 e2) -> BinOp op e1 e2
