{- HLINT ignore "Evaluate" -}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pipeline where

import Data.Foldable qualified as Foldable
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as OrdSet
import Effects.Gensym
import Lang
import Pre
import Target.Asm
import Target.Program

-- | simplifies the tree by removing the @And@ and @Or@ operators.
shrink :: L -> L
shrink (Module ss) = Module $ flip ana ss \case
  If (BinOp And e1 e2) csq alt -> IfF (If e1 e2 (lbool False)) csq alt
  If (BinOp Or e1 e2) csq alt -> IfF (If e1 (lbool True) e2) csq alt
  x -> project x

uniquify :: forall es. Gensym :> es => L -> Eff es L
uniquify (Module ss) = Module <$> cata go ss Set.empty
 where
  go :: ExpF (Set.HashSet Text -> Eff es Exp) -> Set.HashSet Text -> Eff es Exp
  go (LetF x e k) used
    | x `Set.member` used =
        -- no need to insert x'
        Let <$> gensym x <*> e used <*> k used
    | otherwise = Let x <$> e (Set.insert x used) <*> k (Set.insert x used)
  go e used = embed <$> traverse ($ used) e

removeComplexOperands :: forall es. (Gensym :> es) => L -> Eff es ML
removeComplexOperands (Module ss) = MModule <$> rco ss
 where
  rco = \case
    Atom x -> pure $ MAtom x
    InputInt -> pure MInputInt
    UnaryOp op e -> do
      (t, m) <- liftA2 (,) (gensym "t") (rco e)
      pure $ inlineAtom t m \m -> MUnaryOp op m
    BinOp op e1 e2 -> do
      (t1, m1) <- liftA2 (,) (gensym "t") (rco e1)
      (t2, m2) <- liftA2 (,) (gensym "t") (rco e2)
      pure $ inlineAtom t1 m1 \m1 -> inlineAtom t2 m2 \m2 -> MBinOp op m1 m2
    CmpOp op e1 e2 -> do
      (t1, m1) <- liftA2 (,) (gensym "t") (rco e1)
      (t2, m2) <- liftA2 (,) (gensym "t") (rco e2)
      pure $ inlineAtom t1 m1 \m1 -> inlineAtom t2 m2 \m2 -> MCmpOp op m1 m2
    Print e k -> do
      (t, me) <- liftA2 (,) (gensym "t") (rco e)
      mk <- rco k
      pure $ inlineAtom t me \me -> MPrint me mk
    Let x e k -> MLet x <$> rco e <*> rco k
    If (CmpOp op e1 e2) csq alt -> do
      (t1, m1) <- liftA2 (,) (gensym "t") (rco e1)
      (t2, m2) <- liftA2 (,) (gensym "t") (rco e2)
      csq' <- rco csq
      alt' <- rco alt
      pure $ inlineAtom t1 m1 \m1 -> inlineAtom t2 m2 \m2 -> MIf (MCmpOp op m1 m2) csq' alt'
    If cond csq alt ->
      MIf <$> rco cond <*> rco csq <*> rco alt

  inlineAtom :: Text -> MExp -> (Atom -> MExp) -> MExp
  inlineAtom _ (MAtom atom) k = k atom
  inlineAtom t e k = MLet t e (k (Name t))

explicateControl :: forall es. (Gensym :> es) => ML -> Eff es A
explicateControl (MModule ss) = do
  (root, blocks) <- runState Map.empty $ mkBlock =<< ecTail ss
  pure $ Cfg {root, blocks}
 where
  mkBlock :: AStmt -> Eff (State (Map.HashMap Label AStmt) : es) Label
  mkBlock (Goto label) =
    pure label
  mkBlock s = do
    label <- gensym "b"
    modify $ Map.insert label s
    pure label

  ecAssign :: Text -> MExp -> AStmt -> Eff (State (Map.HashMap Label AStmt) : es) AStmt
  ecAssign t e k = case e of
    MAtom x -> pure $ Assign t (AAtom x) k
    MInputInt -> pure $ Assign t AInputInt k
    MUnaryOp op x -> pure $ Assign t (AUnaryOp op x) k
    MBinOp op x1 x2 -> pure $ Assign t (ABinOp op x1 x2) k
    MCmpOp op x1 x2 -> pure $ Assign t (ACmpOp op x1 x2) k
    MLet t' e' k' -> ecAssign t' e' =<< ecAssign t k' k
    MPrint x k' -> Expr (APrint x) <$> ecAssign t k' k
    MIf cond csq alt -> do
      (t1, t2) <- liftA2 (,) (gensym "t") (gensym "t")
      cont' <- mkBlock k
      csq' <- mkBlock =<< ecAssign t1 csq (Goto cont')
      alt' <- mkBlock =<< ecAssign t2 alt (Goto cont')
      ecPred cond csq' alt'

  ecTail :: MExp -> Eff (State (Map.HashMap Label AStmt) : es) AStmt
  ecTail = \case
    MAtom x -> pure $ Return x
    MInputInt -> returnify AInputInt
    MUnaryOp op x -> returnify (AUnaryOp op x)
    MBinOp op x1 x2 -> returnify (ABinOp op x1 x2)
    MCmpOp op x1 x2 -> returnify (ACmpOp op x1 x2)
    MLet x e k -> ecAssign x e =<< ecTail k
    MPrint x k -> Expr (APrint x) <$> ecTail k
    MIf cond csq alt -> do
      csq' <- mkBlock =<< ecTail csq
      alt' <- mkBlock =<< ecTail alt
      ecPred cond csq' alt'
   where
    returnify e = do
      t <- gensym "t"
      pure $ Assign t e (Return (Name t))

  ecPred :: MExp -> Label -> Label -> Eff (State (Map.HashMap Label AStmt) : es) AStmt
  ecPred (MCmpOp cmp e1 e2) csq alt =
    pure $ AIf cmp e1 e2 csq alt
  ecPred (MLet t e k) csq alt =
    ecAssign t e =<< ecPred k csq alt
  ecPred (MIf cond e1 e2) csq alt = do
    csq' <- mkBlock =<< ecPred e1 csq alt
    alt' <- mkBlock =<< ecPred e2 csq alt
    ecPred cond csq' alt'
  ecPred (MAtom (LitBool x)) csq alt =
    case x of
      True -> pure (Goto csq)
      False -> pure (Goto alt)
  ecPred cond csq alt = do
    c <- gensym "c"
    ecAssign c cond (AIf Eq (Name c) (LitBool True) csq alt)

selectInstructions :: forall es. (Gensym :> es) => AStmt -> Eff es  [AsmVar]
selectInstructions ss = fmap reverse $ execState [] $ siStmt ss
 where
  emit :: [AsmVar] -> Eff (State [AsmVar] : es) ()
  emit x = modify (reverse x <>)

  siArg :: Atom -> Arg Src Avar
  siArg (LitInt x) = Imm x
  siArg (LitBool x) = Imm (fromEnum x)
  siArg (Name x) = Var x

  siBinOp Add = Addq
  siBinOp Sub = Subq
  siBinOp And = Andq
  siBinOp Or = Orq
  siUnaryOp USub = Negq
  siUnaryOp Not = \dst -> Xorq dst (Imm 1)
  siCmpOp Eq = Ce
  siCmpOp Neq = Cne
  siCmpOp Lt = Cl
  siCmpOp Le = Cle

  siStmt (Expr e k) = gensym "t" >>= \t -> siStmt (Assign t e k)
  siStmt (Assign x (AAtom y) k) = do
    emit [Movq (Var x) (siArg y)]
    siStmt k
  siStmt (Assign x (ABinOp op (Name y) (Name z)) k)
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
  siStmt (Assign x (ABinOp op y z) k) = do
    emit
      [ Movq (Var x) (siArg y)
      , siBinOp op (Var x) (siArg z)
      ]
    siStmt k
  siStmt (Assign x (ACmpOp op y z) k) = do
    emit
      [ Cmpq (siArg y) (siArg z)
      , Setal (siCmpOp op)
      , Movzbqal (Var x)
      ]
    siStmt k
  siStmt (Assign x (AUnaryOp op (Name y)) k)
    | x == y = do
        emit [siUnaryOp op (Var x)]
        siStmt k
  siStmt (Assign x (AUnaryOp op y) k) = do
    emit
      [ Movq (Var x) (siArg y)
      , siUnaryOp op (Var x)
      ]
    siStmt k
  siStmt (Assign x AInputInt k) = do
    emit
      [ Callq "input_int"
      , Movq (Var x) rax
      ]
    siStmt k
  siStmt (Assign _ (APrint e) k) = do
    emit
      [ Movq rdi (siArg e)
      , Callq "print_int"
      ]
    siStmt k

  siStmt (AIf op e1 e2 csq alt) = do
    emit
      [ Cmpq (siArg e1) (siArg e2)
      , Jc (siCmpOp op) csq
      , Jmp alt
      ]
  siStmt (Return e) = do
    emit
      [ Movq (Reg Rax) (siArg e)
      , Jmp "main_conclusion"
      ]
  siStmt (Goto label) = do
    emit [ Jmp label ]

data StackFrame = StackFrame
  { offsets :: Map.HashMap Text Int
  , size :: Int
  }

assignHomes :: forall es. Cfg [AsmVar] -> Eff es (Int, Cfg [Asm])
assignHomes asmvar = mdo
  (program, StackFrame{size}) <-
    runState (StackFrame Map.empty 0) do
      traverse (traverse instruction) asmvar
  pure (size, program)
 where
  instruction :: AsmVar -> Eff (State StackFrame : es) Asm
  instruction (Movq a b) = Movq <$> argument a <*> argument b
  instruction (Movzbqal a) = Movzbqal <$> argument a
  instruction (Addq a b) = Addq <$> argument a <*> argument b
  instruction (Subq a b) = Subq <$> argument a <*> argument b
  instruction (Negq a) = Negq <$> argument a
  instruction (Andq a b) = Andq <$> argument a <*> argument b
  instruction (Orq a b) = Orq <$> argument a <*> argument b
  instruction (Xorq a b) = Xorq <$> argument a <*> argument b
  instruction (Cmpq a b) = Cmpq <$> argument a <*> argument b
  instruction (Jc c label) = pure $ Jc c label
  instruction (Jmp label) = pure $ Jmp label
  instruction (Setal c) = pure $ Setal c
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
  argument (Deref x o) = pure $ Deref x o

type LivenessItem = Either Reg Text
type LivenessTrace = NonEmpty (Set.HashSet LivenessItem)

uncoverLive :: Cfg [AsmVar] -> Cfg ([AsmVar], LivenessTrace)
buildInterferenceGraph :: Cfg ([AsmVar], LivenessTrace) -> Set.HashSet (LivenessItem, LivenessItem)
(uncoverLive, buildInterferenceGraph) = (uncoverLive, buildInterferenceGraph)
 where
  -- perhaps use Phases functor
  uncoverLive Cfg {root, blocks} = Cfg {root, blocks = go $ topsort root}
   where
    go [] = Map.empty
    go (v : (go -> rest)) = Map.insert v (blocks Map.! v, uncoverBlock rest $ blocks Map.! v) rest

    topsort :: Label -> [Label]
    topsort v | Nothing <- blocks Map.!? v = []
    topsort v = foldl1' (\a b -> nubOrd (a <> b)) (map topsort $ jumps $ blocks Map.! v)
    jumps =
      nubOrd . foldMap \case
        Jmp label -> [label]
        Jc _ label -> [label]
        _ -> []

    uncoverBlock _ [] = NE.singleton Set.empty
    uncoverBlock blocks (inst : rest) =
      let restLiveness@(after :| _) = uncoverBlock blocks rest
          beforeIfContinue = (after `Set.difference` write inst) `Set.union` read' inst
          before =
            case inst of
              Jmp label -> NE.head (snd $ blocks Map.! label)
              Jc _ label -> NE.head (snd $ blocks Map.! label) <> beforeIfContinue
              _ -> beforeIfContinue
       in before `NE.cons` restLiveness
  buildInterferenceGraph = Foldable.fold . fmap (uncurry doBlock)
   where
    doBlock asm (_ :| liveness) | length asm /= length liveness = error "buildInterferenceGraph: invalid list sizes"
    doBlock asm (_ :| liveness) =
      -- ignore special registers while building the graph,
      -- therefore we don't need to ignore them while doing colorGraph
      let instruction (Movq dst src) after = instructionMov dst src after
          instruction (Movzbqal dst) after = instructionMov dst (Reg Rax) after
          instruction i after =
            Set.fromList
              [ edge
              | v <- Set.toList after
              , d <- Set.toList (write i)
              , v /= d
              , not (isSpecial v || isSpecial d)
              , edge <- [(v, d), (d, v)]
              ]
          instructionMov (arg' -> Just d) (arg' -> s) after =
            Set.fromList
              [ edge
              | v <- Set.toList after
              , v /= d && v `notElem` s
              , not (isSpecial v || isSpecial d)
              , edge <- [(v, d), (d, v)]
              ]
          instructionMov _ _ _ = error "infallible"
       in Set.unions (zipWith instruction asm liveness)

  -- TODO: somehow refactor
  -- TODO: Deref is completely broken because deref actually reads even if it is dst
  arg' :: Arg a v -> Maybe LivenessItem
  arg' (Var x) = Just (Right x)
  -- maybe we should check for the speciality here, though doing so breaks killing the register when written, in `write`
  arg' (Reg x) = Just (Left x)
  arg' (Deref _ _) = error "TODO"
  arg' _ = Nothing

  arg :: Arg a v -> Set.HashSet LivenessItem
  arg = Set.fromList . maybeToList . arg'

  write :: AsmVar -> Set.HashSet LivenessItem
  write (Movq a _) = arg a
  write (Movzbqal a) = arg a
  write (Addq a _) = arg a
  write (Subq a _) = arg a
  write (Negq a) = arg a
  write (Andq a _) = arg a
  write (Orq a _) = arg a
  write (Xorq a _) = arg a
  write (Cmpq _ _) = mempty
  write (Setal _) = Set.singleton (Left Rax)
  write (Jc _ _) = error "write: doesn't support jumps"
  write (Jmp _) = error "write: doesn't support jumps"
  write (Pushq _) = mempty
  write (Popq a) = arg a
  write (Callq _) = Set.fromList $ Left <$> returnRegs
  write Retq = mempty

  read' :: AsmVar -> Set.HashSet LivenessItem
  read' (Movq _ b) = arg b
  read' (Movzbqal _) = Set.singleton (Left Rax)
  read' (Addq a b) = arg a <> arg b
  read' (Subq a b) = arg a <> arg b
  read' (Negq a) = arg a
  read' (Andq a b) = arg a <> arg b
  read' (Orq a b) = arg a <> arg b
  read' (Xorq a b) = arg a <> arg b
  read' (Cmpq a b) = arg a <> arg b
  read' (Setal _) = mempty
  read' (Jc _ _) = error "read': doesn't support jumps"
  read' (Jmp _) = error "read': doesn't support jumps"
  read' (Pushq a) = arg a
  read' (Popq _) = mempty
  read' (Callq "input_int") = Set.empty
  read' (Callq "print_int") = Set.fromList [Left Rdi]
  read' (Callq _) = Set.fromList $ Left <$> argumentRegs
  read' Retq = mempty

{- | special registers always map to the same register after assignRegisters.

note that assignRegisters also rewrites the registers, not just vars
-}
isSpecial :: LivenessItem -> Bool
isSpecial (Left r) = r `elem` argumentRegs || r `elem` returnRegs
isSpecial (Right _) = False

-- | if a node has no edges, then it won't be present in the graph. in that case, the color can simply be defaulted to 0
colorGraph :: Set.HashSet (LivenessItem, LivenessItem) -> Map.HashMap LivenessItem Int
colorGraph g =
  let adj = Set.foldl' (\acc (from, to) -> Map.insertWith (++) from [to] acc) Map.empty g
      vertices = Map.keys adj
      go
        :: OrdSet.Set (Int, LivenessItem)
        -> Map.HashMap LivenessItem (OrdSet.Set Int)
        -> [(LivenessItem, Int)]
      go (OrdSet.maxView -> Nothing) _ = []
      go (OrdSet.maxView -> Just ((_, v), nco)) nc =
        let ns = fromMaybe OrdSet.empty $ nc Map.!? v
            neighbors = fromMaybe [] $ adj Map.!? v
            color = findColor ns 0
            nc' =
              Map.delete v
                $ flip Map.union nc
                  . Map.fromList
                $ flip mapMaybe neighbors \to ->
                  (to,) . OrdSet.insert color <$> (nc Map.!? to)
            getNco ncc =
              OrdSet.fromList $
                flip mapMaybe neighbors \to ->
                  (,to) . OrdSet.size <$> (ncc Map.!? to)
            nco' = (nco `OrdSet.difference` getNco nc) `OrdSet.union` getNco nc'
         in (v, color) : go nco' nc'
      -- aka MEX
      findColor set i
        | OrdSet.member i set = findColor set (i + 1)
        | otherwise = i
   in Map.fromList $
        go
          (OrdSet.fromList $ (0,) <$> vertices)
          (Map.fromList $ (,mempty) <$> vertices)

assignRegisters :: Cfg [AsmVar] -> Cfg [AsmVar]
assignRegisters asm =
  let graph = buildInterferenceGraph (uncoverLive asm)
      colors = colorGraph graph
      registers = [Rcx, Rdx, Rbx]
      assignRegister :: LivenessItem -> Maybe (Arg a v)
      assignRegister v@(Left a)
        | isSpecial v = Just (Reg a)
      assignRegister v = Reg <$> registers !? fromMaybe 0 (colors Map.!? v)
      eachArg :: Arg a v -> Arg a v
      eachArg a@(Reg x) = fromMaybe a $ assignRegister (Left x)
      eachArg a@(Var x) = fromMaybe a $ assignRegister (Right x)
      eachArg (Deref _ _) = error "TODO"
      eachArg (Imm x) = Imm x
   in flip fmap asm $ map \case
        Movq a b -> Movq (eachArg a) (eachArg b)
        Movzbqal a -> Movzbqal (eachArg a)
        Addq a b -> Addq (eachArg a) (eachArg b)
        Subq a b -> Subq (eachArg a) (eachArg b)
        Negq a -> Negq (eachArg a)
        Andq a b -> Andq (eachArg a) (eachArg b)
        Orq a b -> Orq (eachArg a) (eachArg b)
        Xorq a b -> Xorq (eachArg a) (eachArg b)
        Cmpq a b -> Cmpq (eachArg a) (eachArg b)
        Setal c -> Setal c
        Jc c label -> Jc c label
        Jmp label -> Jmp label
        Pushq a -> Pushq (eachArg a)
        Popq a -> Popq (eachArg a)
        Callq x -> Callq x
        Retq -> Retq

patchInstructions :: Cfg [Asm] -> Cfg [Asm]
patchInstructions =
  let patch2 :: (Arg a Aint -> Arg b Aint -> Asm) -> Arg a Aint -> Arg b Aint -> [Asm]
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

      patchCmpq :: Arg Src Aint -> Arg Src Aint -> [Asm]
      patchCmpq a (Imm x) =
        [ Movq (Reg Rax) (Imm x)
        ] <> patch2 Cmpq a (Reg Rax)
      patchCmpq a b = [Cmpq a b]
   in fmap $ concatMap \case
        Movq a b -> patch2 Movq a b
        Movzbqal a -> [Movzbqal a]
        Addq a b -> patch2 Addq a b
        Subq a b -> patch2 Subq a b
        Negq a -> [Negq a]
        Andq a b -> patch2 Andq a b
        Orq a b -> patch2 Orq a b
        Xorq a b -> patch2 Xorq a b
        Cmpq a b -> patchCmpq a b
        Setal c -> [Setal c]
        Jc c label -> [Jc c label]
        Jmp label -> [Jmp label]
        Callq x -> [Callq x]
        Pushq a -> patchImm Pushq a
        Popq a -> [Popq a]
        Retq -> [Retq]

preludeAndConclusion :: Int -> Cfg [Asm] -> Program
preludeAndConclusion fs asm =
  Program
    { globals = ["main", "print_int", "input_int"]
    , asm =
        [ ("main",)
            [ Pushq rbp
            , Movq rbp rsp
            , Subq rsp (Imm $ ((fs + 15) `div` 16) * 16)
            , Jmp asm.root
            ]
        ] <> linearizedBlocks <> [
          ("main_conclusion",)
            [ Movq rsp rbp
            , Popq rbp
            , Retq
            ]
        ]
    }
  where
    -- optimizations are possible. also, this step should be moved somewhere else
    linearizedBlocks = sortBy (flip compare `on` fst) (Map.toList asm.blocks)

compile :: L -> Either TypeCheckerError Program
compile l = runPureEff . runErrorNoCallStack @TypeCheckerError . runGensym $ do
  ml <- removeComplexOperands <=< uniquify . shrink . peL $ l
  anf <- explicateControl ml
  asmvar <- traverse selectInstructions anf
  (size, asm) <- assignHomes (assignRegisters asmvar)
  pure $ preludeAndConclusion size (patchInstructions asm)
