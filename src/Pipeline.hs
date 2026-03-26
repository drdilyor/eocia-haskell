{- HLINT ignore "Evaluate" -}
module Pipeline where

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Int
import Data.List.NonEmpty qualified as NE
import Effects.Gensym
import Lang
import Pre
import Target.Asm
import Target.Program

msbind :: MStmt -> Text -> MStmt -> MStmt
msbind s x j = flip cata s \case
  MExprF e -> MLet x e j
  s' -> embed s'

-- >>> msbind (MExpr (MBinOp Add (Lit 1) (Lit 2))) "x" (MExpr (MBinOp Add "x" (Lit 3)))
-- MLet "x" (MBinOp Add (Lit 1) (Lit 2)) (MExpr (MBinOp Add (Name "x") (Lit 3)))
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
      , Movq (Var x) rax
      ]
    siStmt k
  siStmt (MPrint e k) = do
    emit
      [ Movq rdi (siArg e)
      , Callq "print_int"
      ]
    siStmt k
  siStmt (MExpr (MAtom x)) = do
    emit [Movq rax (siArg x)]
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
  instruction (Pushq a) = Pushq <$> argument a
  instruction (Popq a) = Popq <$> argument a
  instruction Retq = pure Retq

  argument :: Arg a Avar -> Eff (State StackFrame : es) (Arg a Aint)
  argument (Var x) = do
    sf <- get
    case Map.lookup x sf.offsets of
      Nothing -> do
        let o = -(sf.size + 8)
        put $ StackFrame (Map.insert x o sf.offsets) (sf.size + 8)
        pure (Deref Rbp o)
      Just o ->
        pure (Deref Rbp o)
  argument (Imm x) = pure $ Imm x
  argument (Reg x) = pure $ Reg x
  argument (Deref o x) = pure $ Deref o x

collectVars :: [AsmVar] -> [Text]
collectVars = concatMap vars
 where
  arg :: Arg a v -> [Text]
  arg (Var x) = [x]
  arg _ = []

  vars (Addq a b) = arg a <> arg b
  vars (Subq a b) = arg a <> arg b
  vars (Negq a) = arg a
  vars (Movq a b) = arg a <> arg b
  vars (Pushq a) = arg a
  vars (Popq a) = arg a
  vars (Callq _) = mempty
  vars Retq = mempty

uncoverLive :: [AsmVar] -> NonEmpty (Set.HashSet (Either Reg Text))
uncoverLive [] = NE.singleton Set.empty
uncoverLive (inst : rest) =
  let restLiveness@(after :| _) = uncoverLive rest
      before = (after `Set.difference` write inst) `Set.union` read' inst
   in before `NE.cons` restLiveness
 where
  arg :: Arg a v -> Set.HashSet (Either Reg Text)
  arg (Var x) = Set.singleton (Right x)
  arg (Reg x) = Set.singleton (Left x)
  arg (Deref x _) = Set.singleton (Left x)
  arg _ = Set.empty

  write (Movq a _) = arg a
  write (Addq a _) = arg a
  write (Subq a _) = arg a
  write (Negq a) = arg a
  write (Pushq _) = mempty
  write (Popq a) = arg a
  write (Callq _) = Set.fromList $ Left <$> [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]
  write Retq = mempty

  read' (Movq _ b) = arg b
  read' (Addq a b) = arg a <> arg b
  read' (Subq a b) = arg a <> arg b
  read' (Negq a) = arg a
  read' (Pushq a) = arg a
  read' (Popq _) = mempty
  read' (Callq "input_int") = Set.empty
  read' (Callq "print_int") = Set.fromList [Left Rdi]
  read' (Callq _) = Set.fromList $ Left <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]
  read' Retq = mempty

patchInstructions :: [Asm] -> [Asm]
patchInstructions =
  let patch2 :: (Arg Dst Aint -> Arg Src Aint -> Asm) -> Arg Dst Aint -> Arg Src Aint -> [Asm]
      patch2 inst (Deref a x) (Deref b y) =
        [ Movq (Reg Rax) (Deref b y)
        , inst (Deref a x) (Reg Rax)
        ]
      patch2 inst a@(Deref _ _) b@(Imm _) = patchImm (inst a) b
      patch2 f x y = [f x y]

      patchImm :: (Arg Src Aint -> Asm) -> Arg Src Aint -> [Asm]
      patchImm inst (Imm y)
        -- y doesn't fit in 32-bit signed integer
        | y /= fromIntegral @Int32 @Int (fromIntegral y) =
            [ Movq (Reg Rax) (Imm y)
            , inst (Reg Rax)
            ]
      patchImm f x = [f x]
   in concatMap \case
        Movq a b -> patch2 Movq a b
        Addq a b -> patch2 Addq a b
        Subq a b -> patch2 Subq a b
        Callq x -> [Callq x]
        Negq a -> [Negq a]
        Pushq a -> patchImm Pushq a
        Popq a -> [Popq a]
        Retq -> [Retq]

preludeAndConclusion :: (Int, [Asm]) -> Program
preludeAndConclusion (fs, asm) =
  Program
    { globals = ["_start"]
    , asm =
        [ ("_start",)
            [ Pushq rbp
            , Movq rbp rsp
            , Subq rsp (Imm fs)
            ]
        , ("main",) asm
        , ("conclusion",)
            [ Movq rsp rbp
            , Popq rbp
            , Retq
            ]
        ]
    }

compile :: L -> Program
compile l = runPureEff . runGensym $ do
  ml <- removeComplexOperands l
  asmvar <- selectInstructions ml
  (size, asm) <- assignHomes asmvar
  pure $ preludeAndConclusion (size, patchInstructions asm)
