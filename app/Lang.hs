module Lang where

import Effects.Lio
import Pre

newtype L = Module [Stmt]

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

interpL :: (Lio :> es, Error InterpError :> es) => L -> Eff es ()
interpL (Module ss) = mapM_ interpStmt ss

interpStmt :: (Lio :> es, Error InterpError :> es) => Stmt -> Eff es ()
interpStmt (Print e) = lioPrintLine . show =<< interpExp e
interpStmt (Expr e) = void $ interpExp e

interpExp :: (Lio :> es, Error InterpError :> es) => Exp -> Eff es Int
interpExp (Constant x) = pure x
interpExp (UnaryOp op e) = interpUnaryOp op <$> interpExp e
interpExp (BinOp op e1 e2) = interpBinOp op <$> interpExp e1 <*> interpExp e2
interpExp InputInt =
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
peStmt (Expr e) = Print (peExp e)

peExp :: Exp -> Exp
peExp (Constant e) = Constant e
peExp InputInt = InputInt
peExp (UnaryOp op (peExp -> Constant x)) = Constant $ interpUnaryOp op x
peExp (UnaryOp op (peExp -> e)) = UnaryOp op e
peExp (BinOp op (peExp -> Constant x) (peExp -> Constant y)) = Constant $ interpBinOp op x y
peExp (BinOp op (peExp -> e1) (peExp -> e2)) = BinOp op e1 e2
