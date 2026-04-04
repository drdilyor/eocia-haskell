{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use newtype instead of data" -}

module Lang where

import Data.HashMap.Strict qualified as Map
import Data.String
import Effects.Lio
import Pre

newtype L = Module Exp
  deriving (Eq, Show, Read)

data Exp
  = Atom Atom
  | InputInt
  | UnaryOp UnaryOp Exp
  | BinOp BinOp Exp Exp
  | CmpOp CmpOp Exp Exp
  | Print Exp Exp
  | Let Text Exp Exp
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

newtype TypeChecked a = TypeChecked a
  deriving (Eq, Show)

-- might have to use Reader or pass it manually instead of state in the future
typeCheckL :: (Error TypeCheckerError :> es) => L -> Eff es (TypeChecked L)
typeCheckL (Module ss) =
  runReader initialTypeCheckerEnv $
    typeCheckExp ss >> pure (TypeChecked (Module ss))

typeCheckEqual :: (Error TypeCheckerError :> es) => T -> T -> Eff es ()
typeCheckEqual expected actual =
  when (expected /= actual) . throwError . TypeMismatch $
    "expected " <> show expected <> ", found " <> show actual

typeCheckExp
  :: (Reader TypeCheckerEnv :> es, Error TypeCheckerError :> es)
  => Exp -> Eff es T
typeCheckExp (Atom (LitInt _)) = pure IntT
typeCheckExp (Atom (LitBool _)) = pure BoolT
typeCheckExp (Atom (Name x)) = do
  env <- ask
  case env.tyEnv Map.!? x of
    Nothing -> throwError $ UnboundVariable x
    Just xt -> pure xt
typeCheckExp InputInt = pure IntT
typeCheckExp (UnaryOp op e) = typeCheckExp e >>= typeCheckEqual argt >> pure rett
 where
  (argt, rett) = case op of
    USub -> (IntT, IntT)
    Not -> (BoolT, BoolT)
typeCheckExp (BinOp op e1 e2) = do
  typeCheckExp e1 >>= typeCheckEqual arg1t
  typeCheckExp e2 >>= typeCheckEqual arg2t
  pure rett
 where
  (arg1t, arg2t, rett) = case op of
    Add; Sub -> (IntT, IntT, IntT)
    And; Or -> (BoolT, BoolT, BoolT)
typeCheckExp (CmpOp (Eq; Neq) e1 e2) = do
  t1 <- typeCheckExp e1
  t2 <- typeCheckExp e2
  typeCheckEqual t1 t2 >> pure BoolT
typeCheckExp (CmpOp (Le; Lt) e1 e2) = do
  typeCheckExp e1 >>= typeCheckEqual IntT
  typeCheckExp e2 >>= typeCheckEqual IntT
  pure BoolT
typeCheckExp (Let x e k) = do
  et <- typeCheckExp e
  local (\env -> env{tyEnv = Map.insert x et env.tyEnv}) do
    typeCheckExp k
typeCheckExp (Print e k) = do
  et <- typeCheckExp e
  typeCheckEqual IntT et -- currently int only
  typeCheckExp k
typeCheckExp (If cond csq alt) = do
  typeCheckExp cond >>= typeCheckEqual BoolT
  csqt <- typeCheckExp csq
  altt <- typeCheckExp alt
  typeCheckEqual csqt altt
  pure altt

data InterpError
  = InvalidInput Text
  deriving (Eq, Show)

newtype InterpEnv
  = InterpEnv {bindings :: Map.HashMap Text V}
  deriving (Eq, Show, Read)

initialInterpEnv :: InterpEnv
initialInterpEnv = InterpEnv Map.empty

lookupBinding :: (Reader InterpEnv :> es) => Text -> Eff es (Maybe V)
lookupBinding v = Map.lookup v . (.bindings) <$> ask

interpL :: (Lio :> es, Error InterpError :> es) => TypeChecked L -> Eff es V
interpL (TypeChecked (Module ss)) = runReader initialInterpEnv $ interpExp ss

interpExp :: forall es. (Lio :> es, Reader InterpEnv :> es, Error InterpError :> es) => Exp -> Eff es V
interpExp = cata \case
  AtomF (LitInt x) -> pure (LitIntV x)
  AtomF (LitBool x) -> pure (LitBoolV x)
  AtomF (Name v) ->
    lookupBinding v >>= \case
      Nothing -> error "infallible"
      Just x -> pure x
  UnaryOpF op x -> interpUnaryOp op <$> x
  BinOpF op x y -> interpBinOp op <$> x <*> y
  CmpOpF op x y -> interpCmpOp op <$> x <*> y
  LetF v e k -> do
    x <- e
    local (\env -> env{bindings = Map.insert v x env.bindings}) do
      k
  InputIntF ->
    lioInputLine >>= \case
      (readMaybe . unpack -> Just x) -> pure (LitIntV x)
      s -> throwError . InvalidInput $ "couldn't parse " <> show s
  -- TODO: move this printing and parsing elsewhere
  PrintF e k -> do
    v <- e
    lioPrintLine case v of
      LitIntV x -> show x
      LitBoolV False -> "false"
      LitBoolV True -> "true"
    k

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
peL (Module ss) = Module (peExp ss)

peExp :: Exp -> Exp
peExp = cata \case
  (UnaryOpF op (vOfExp -> Just x)) -> expOfV $ interpUnaryOp op x
  (BinOpF op (vOfExp -> Just x) (vOfExp -> Just y)) -> expOfV $ interpBinOp op x y
  (CmpOpF op (vOfExp -> Just x) (vOfExp -> Just y)) -> expOfV $ interpCmpOp op x y
  x -> embed x

newtype ML = MModule MExp
  deriving (Eq, Show, Read)

data MExp
  = MAtom Atom
  | MInputInt
  | MUnaryOp UnaryOp Atom
  | MBinOp BinOp Atom Atom
  | MCmpOp CmpOp Atom Atom
  | MLet Text MExp MExp
  | MPrint Atom MExp
  deriving (Eq, Show, Read)

instance IsString MExp where
  fromString = MAtom . Name . fromString

makeBaseFunctor ''ML
makeBaseFunctor ''MExp

mlint :: Int -> MExp
mlint x = MAtom (LitInt x)

mlbool :: Bool -> Exp
mlbool = Atom . LitBool
