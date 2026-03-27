{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use newtype instead of data" -}

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
  | CmpOp CmpOp Exp Exp
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
  | Not
  deriving (Eq, Show, Read)

data BinOp
  = Add
  | Sub
  | And
  | Or
  deriving (Eq, Show, Read)

data CmpOp
  = Eq
  | Neq
  | Lt
  | Le
  deriving (Eq, Show, Read)

makeBaseFunctor ''L
makeBaseFunctor ''Stmt
makeBaseFunctor ''Exp

lint :: Int -> Exp
lint = Atom . LitInt

lbool :: Bool -> Exp
lbool = Atom . LitBool

data T = IntT | BoolT
  deriving (Eq, Show, Read)

-- TODO: better error reporting
data TypeCheckerError
  = TypeMismatch Text
  | UnboundVariable Text
  deriving (Eq, Show)

newtype TypeCheckerEnv
  = TypeCheckerEnv {tyEnv :: Map.HashMap Text T}
  deriving (Eq, Show, Read)

initialTypeCheckerEnv :: TypeCheckerEnv
initialTypeCheckerEnv = TypeCheckerEnv Map.empty

lookupTyEnv :: ( State TypeCheckerEnv :> es) => Text -> Eff es (Maybe T)
lookupTyEnv v = Map.lookup v . (.tyEnv) <$> get

newtype TypeChecked a = TypeChecked a
  deriving (Eq, Show)

-- might have to use Reader or pass it manually instead of state in the future
typeCheckL :: (Error TypeCheckerError :> es) => L -> Eff es (TypeChecked L)
typeCheckL (Module ss) =
  evalState initialTypeCheckerEnv $
    typeCheckStmt ss >> pure (TypeChecked (Module ss))

typeCheckStmt
  :: (State TypeCheckerEnv :> es, Error TypeCheckerError :> es)
  => Stmt -> Eff es ()
typeCheckStmt (Expr e) = void $ typeCheckExp e
typeCheckStmt (Let x e k) = do
  et <- typeCheckExp e
  modify \env -> env{tyEnv = Map.insert x et env.tyEnv}
  typeCheckStmt k
typeCheckStmt (Print e k) = do
  et <- typeCheckExp e
  typeCheckEqual IntT et  -- currently int only
  typeCheckStmt k

typeCheckEqual :: (Error TypeCheckerError :> es) => T -> T -> Eff es ()
typeCheckEqual expected actual =
  when (expected /= actual) . throwError . TypeMismatch $
    "expected " <> show expected <> ", found " <> show actual

typeCheckExp
  :: (State TypeCheckerEnv :> es, Error TypeCheckerError :> es)
  => Exp -> Eff es T
typeCheckExp (Atom (LitInt _)) = pure IntT
typeCheckExp (Atom (LitBool _)) = pure BoolT
typeCheckExp (Atom (Name x)) = do
  env <- get
  case env.tyEnv Map.!? x of
    Nothing -> throwError $ UnboundVariable x
    Just xt -> pure xt
typeCheckExp InputInt = pure IntT
typeCheckExp (UnaryOp op e) = typeCheckExp e >>= typeCheckEqual argt >> pure rett
  where
   (argt, rett) = case op of
     USub -> (IntT, IntT)
     Not -> (BoolT, BoolT)
typeCheckExp (BinOp op e1 e2) =
  typeCheckExp e1 >>= typeCheckEqual arg1t >>
  typeCheckExp e2 >>= typeCheckEqual arg2t >>
  pure rett
 where
  (arg1t, arg2t, rett) = case op of
    Add; Sub -> (IntT, IntT, IntT)
    And; Or -> (BoolT, BoolT, BoolT)
typeCheckExp (CmpOp (Eq; Neq) e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  typeCheckEqual t1 t2 >> pure BoolT
typeCheckExp (CmpOp (Le; Lt) e1 e2) =
  typeCheckExp e1 >>= typeCheckEqual IntT >>
  typeCheckExp e2 >>= typeCheckEqual IntT >>
  pure BoolT

data InterpError
  = InvalidInput Text
  deriving (Eq, Show)

newtype InterpEnv
  = InterpEnv {bindings :: Map.HashMap Text V}
  deriving (Eq, Show, Read)

initialInterpEnv :: InterpEnv
initialInterpEnv = InterpEnv Map.empty

lookupBinding :: (State InterpEnv :> es) => Text -> Eff es (Maybe V)
lookupBinding v = Map.lookup v . (.bindings) <$> get

interpL :: (Lio :> es, Error InterpError :> es) => TypeChecked L -> Eff es V
interpL (TypeChecked (Module ss)) = evalState initialInterpEnv $ interpStmt ss

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
      Nothing -> error "infallible"
      Just x -> pure x
  (UnaryOpF op x) -> interpUnaryOp op <$> x
  (BinOpF op x y) -> interpBinOp op <$> x <*> y
  (CmpOpF op x y) -> interpCmpOp op <$> x <*> y
  InputIntF ->
    lioInputLine >>= \case
      (readMaybe . unpack -> Just x) -> pure (LitIntV x)
      s -> throwError . InvalidInput $ "couldn't parse " <> show s

interpUnaryOp :: UnaryOp -> V -> V
interpUnaryOp USub (LitIntV x) = LitIntV (negate x)
interpUnaryOp USub _ = error ""
interpUnaryOp Not (LitBoolV x) = LitBoolV (not x)
interpUnaryOp Not _ = error ""

interpBinOp :: BinOp -> V -> V -> V
interpBinOp Add (LitIntV x) (LitIntV y) = LitIntV (x + y)
interpBinOp Add _ _ = error ""
interpBinOp Sub (LitIntV x) (LitIntV y) = LitIntV (x - y)
interpBinOp Sub _ _ = error ""
interpBinOp And (LitBoolV x) (LitBoolV y) = LitBoolV (x && y)
interpBinOp And _ _ = error ""
interpBinOp Or (LitBoolV x) (LitBoolV y) = LitBoolV (x || y)
interpBinOp Or _ _ = error ""

interpCmpOp :: CmpOp -> V -> V -> V
interpCmpOp Eq (LitIntV x) (LitIntV y) = LitBoolV (x == y)
interpCmpOp Eq (LitBoolV x) (LitBoolV y) = LitBoolV (x == y)
interpCmpOp Eq _ _ = error ""
interpCmpOp Neq (LitIntV x) (LitIntV y) = LitBoolV (x /= y)
interpCmpOp Neq (LitBoolV x) (LitBoolV y) = LitBoolV (x /= y)
interpCmpOp Neq _ _ = error ""
interpCmpOp Lt (LitIntV x) (LitIntV y) = LitBoolV (x < y)
interpCmpOp Lt _ _ = error ""
interpCmpOp Le (LitIntV x) (LitIntV y) = LitBoolV (x <= y)
interpCmpOp Le _ _ = error ""

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
  (CmpOpF op (vOfExp -> Just x) (vOfExp -> Just y)) -> expOfV $ interpCmpOp op x y
  (CmpOpF op e1 e2) -> CmpOp op e1 e2

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
  | MCmpOp CmpOp Atom Atom
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
