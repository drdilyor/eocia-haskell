module Lang where

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
