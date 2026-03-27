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
  = Atom Atom
  | InputInt
  | UnaryOp UnaryOp Exp
  | BinOp BinOp Exp Exp
  deriving (Eq, Show, Read)

data Atom = LitInt Int | LitBool Bool | Name Text
  deriving (Eq, Show, Read)

data V = LitIntV Int | LitBoolV Bool
  deriving (Eq, Show, Read)

vOfExp :: Exp -> Maybe V
vOfExp (Atom (LitInt x)) = Just (LitIntV x)
vOfExp (Atom (LitBool b)) = Just (LitBoolV b)
vOfExp _ = Nothing

expOfV :: V -> Exp
expOfV (LitIntV x) = Atom (LitInt x)
expOfV (LitBoolV b) = Atom (LitBool b)

instance IsString Exp where
  fromString = Atom . fromString

instance IsString Atom where
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

lint :: Int -> Exp
lint = Atom . LitInt

lbool :: Bool -> Exp
lbool = Atom . LitBool

newtype InterpEnv
  = InterpEnv {bindings :: Map.HashMap Text V}
  deriving (Eq, Show, Read)

initialInterpEnv :: InterpEnv
initialInterpEnv = InterpEnv Map.empty

lookupBinding :: (State InterpEnv :> es) => Text -> Eff es (Maybe V)
lookupBinding v = Map.lookup v . (.bindings) <$> get

interpL :: (Lio :> es, Error InterpError :> es) => L -> Eff es V
interpL (Module ss) = evalState initialInterpEnv $ interpStmt ss

interpStmt :: (Lio :> es, State InterpEnv :> es, Error InterpError :> es) => Stmt -> Eff es V
interpStmt = cata \case
  ExprF e -> interpExp e
  -- TODO: move this printing and parsing elsewhere
  PrintF e k -> do
    v <- interpExp e
    lioPrintLine case v of
      LitIntV x -> show x
      LitBoolV False -> "false"
      LitBoolV True -> "true"
    k
  LetF v e k -> do
    x <- interpExp e
    modify \env -> env{bindings = Map.insert v x env.bindings}
    k

interpExp :: forall es. (Lio :> es, State InterpEnv :> es, Error InterpError :> es) => Exp -> Eff es V
interpExp = cata \case
  (AtomF (LitInt x)) -> pure (LitIntV x)
  (AtomF (LitBool x)) -> pure (LitBoolV x)
  (AtomF (Name v)) ->
    lookupBinding v >>= \case
      Nothing -> throwError (UnboundVariable v)
      Just x -> pure x
  (UnaryOpF op x) -> interpUnaryOp op <$> x
  (BinOpF op x y) -> interpBinOp op <$> x <*> y
  InputIntF ->
    lioInputLine >>= \case
      (readMaybe . unpack -> Just x) -> pure (LitIntV x)
      s -> throwError . InvalidInput $ "couldn't parse " <> show s

interpUnaryOp :: UnaryOp -> V -> V
interpUnaryOp USub (LitIntV x) = LitIntV (negate x)
interpUnaryOp USub _ = error ""

interpBinOp :: BinOp -> V -> V -> V
interpBinOp Add (LitIntV x) (LitIntV y) = LitIntV (x + y)
interpBinOp Add _ _ = error ""
interpBinOp Sub (LitIntV x) (LitIntV y) = LitIntV (x - y)
interpBinOp Sub _ _ = error ""

peL :: L -> L
peL (Module ss) = Module (peStmt ss)

peStmt :: Stmt -> Stmt
peStmt = cata \case
  ExprF e -> Expr (peExp e)
  PrintF e k -> Print (peExp e) k
  LetF x e k -> Let x (peExp e) k

peExp :: Exp -> Exp
peExp = cata \case
  (AtomF x) -> Atom x
  InputIntF -> InputInt
  (UnaryOpF op (vOfExp -> Just x)) -> expOfV $ interpUnaryOp op x
  (UnaryOpF op e) -> UnaryOp op e
  (BinOpF op (vOfExp -> Just x) (vOfExp -> Just y)) -> expOfV $ interpBinOp op x y
  (BinOpF op e1 e2) -> BinOp op e1 e2

newtype ML = MModule MStmt
  deriving (Eq, Show, Read)

data MStmt
  = MExpr MExp
  | MLet Text MExp MStmt
  | MPrint Atom MStmt
  deriving (Eq, Show, Read)

data MExp
  = MAtom Atom
  | MInputInt
  | MUnaryOp UnaryOp Atom
  | MBinOp BinOp Atom Atom
  deriving (Eq, Show, Read)

instance IsString MExp where
  fromString = MAtom . Name . fromString

makeBaseFunctor ''ML
makeBaseFunctor ''MStmt
makeBaseFunctor ''MExp

mlint :: Int -> MExp
mlint x = MAtom (LitInt x)

mlbool :: Bool -> Exp
mlbool = Atom . LitBool
