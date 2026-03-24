{-# LANGUAGE TemplateHaskell #-}

module Lang where

import Data.HashMap.Strict qualified as Map
import Data.String
import Effects.Lio
import Pre

newtype L = Module Stmt
  deriving (Eq, Show, Read)

data Stmt
  = Expr Exp
  | Print Exp Stmt
  | Let Text Exp Stmt
  deriving (Eq, Show, Read)

data Exp
  = Constant Int
  | Name Text
  | InputInt
  | UnaryOp UnaryOp Exp
  | BinOp BinOp Exp Exp
  deriving (Eq, Show, Read)

instance IsString Exp where
  fromString = Name . fromString

data UnaryOp
  = USub
  deriving (Eq, Show, Read)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show, Read)

data InterpError
  = InvalidInput Text
  | UnboundVariable Text
  deriving (Eq, Show)

makeBaseFunctor ''L
makeBaseFunctor ''Stmt
makeBaseFunctor ''Exp

newtype InterpEnv
  = InterpEnv {bindings :: Map.HashMap Text Int}
  deriving (Eq, Show, Read)

initialInterpEnv :: InterpEnv
initialInterpEnv = InterpEnv Map.empty

lookupBinding :: (State InterpEnv :> es) => Text -> Eff es (Maybe Int)
lookupBinding v = Map.lookup v . (.bindings) <$> get

interpL :: (Lio :> es, Error InterpError :> es) => L -> Eff es Int
interpL (Module ss) = evalState initialInterpEnv $ interpStmt ss

interpStmt :: (Lio :> es, State InterpEnv :> es, Error InterpError :> es) => Stmt -> Eff es Int
interpStmt = cata \case
  ExprF e -> interpExp e
  PrintF e k -> (lioPrintLine . show =<< interpExp e) >> k
  LetF v e k -> do
    x <- interpExp e
    modify \env -> env{bindings = Map.insert v x env.bindings}
    k

interpExp :: forall es. (Lio :> es, State InterpEnv :> es, Error InterpError :> es) => Exp -> Eff es Int
interpExp = cata \case
  (ConstantF x) -> pure x
  (NameF v) ->
    lookupBinding v >>= \case
      Nothing -> throwError (UnboundVariable v)
      Just x -> pure x
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
peL (Module ss) = Module (peStmt ss)

peStmt :: Stmt -> Stmt
peStmt = cata \case
  ExprF e -> Expr (peExp e)
  PrintF e k -> Print (peExp e) k
  LetF x e k -> Let x (peExp e) k

peExp :: Exp -> Exp
peExp = cata \case
  (ConstantF x) -> Constant x
  (NameF v) -> Name v
  InputIntF -> InputInt
  (UnaryOpF op (Constant x)) -> Constant $ interpUnaryOp op x
  (UnaryOpF op e) -> UnaryOp op e
  (BinOpF op (Constant x) (Constant y)) -> Constant $ interpBinOp op x y
  (BinOpF op e1 e2) -> BinOp op e1 e2
