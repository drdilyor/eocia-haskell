module Main (main) where

import Data.HashSet qualified as Set
import Data.Text qualified as T
import Effects.Gensym
import Effects.Lio
import Lang
import Pipeline
import Pre
import Target.Asm
import Target.Interp
import Target.InterpTests qualified as InterpTests
import Target.Program
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{- FOURMOLU_DISABLE -}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [tcTests, peTests, interpTests, gensymTests, rcoTests, siTests, arTests, ahTests, piTests, ulTests, InterpTests.interpAsmTests]

peTests :: TestTree
peTests =
  testGroup
    "peExp"
    [ testCase "pe does evaluate" do
        peExp (BinOp Add (lint 1) (lint 2))
          @?= lint 3
    , testCase "pe stops at InputInt" do
        peExp (BinOp Add InputInt (BinOp Add (lint 1) (lint 2)))
          @?= BinOp Add InputInt (lint 3)
    ]

runInterpL :: L -> [Text] -> Either InterpError (V, [Text])
runInterpL program input =
  runPureEff
    . runErrorNoCallStackWith @InterpError (pure . Left)
    . runErrorNoCallStackWith @TypeCheckerError (\e -> error . unpack $ "runInterpL: TypeCheckerError during testign: " <> show e)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "runInterpL: LioError during testing: " <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ interpL =<< typeCheckL program

runInterpSimple :: L -> Either InterpError V
runInterpSimple program = fst <$> runInterpL program []

runInterpAsm :: Program -> [Text] -> Either InterpAsmError (Int, [Text])
runInterpAsm program input =
  runPureEff
    . runErrorNoCallStackWith @InterpAsmError (pure . Left)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "LioError during testing" <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ interpAsm program

runInterpAsmSimple :: [Asm] -> [Text] -> Either InterpAsmError (Int, [Text])
runInterpAsmSimple asm = runInterpAsm (Program{globals = ["_start"], asm = [("_start", asm)]})

runInterpAsmVarSimple :: [AsmVar] -> [Text] -> Either InterpAsmError (Int, [Text])
runInterpAsmVarSimple asmvar input =
  runPureEff
    . runErrorNoCallStackWith @InterpAsmError (pure . Left)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "LioError during testing" <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ interpAsmVar [("_start", asmvar)] "_start"

interpTests :: TestTree
interpTests =
  testGroup
    "interpTests"
    [ testCase "basic arithmetic" do
        ( runInterpSimple
            . Module
            $ BinOp Add (lint 1) (lint 2)
          )
          @?= Right (LitIntV 3)
    , testCase "variables work" do
        ( runInterpSimple
            . Module
            . Let "x" (lint 1)
            $ BinOp Add "x" (lint 2)
          )
          @?= Right (LitIntV 3)
    , testCase "input and output" do
        ( runInterpL
            . Module
            . Print (BinOp Add (lint 1) InputInt)
            $ lint 0
          )
          ["2"]
          @?= Right (LitIntV 0, ["3"])
    ]

genPrefixes :: Gen [Text]
genPrefixes = listOf . oneof . map (pure . pack . singleton) $ ['a' .. 'k']

gensymTests :: TestTree
gensymTests =
  testGroup
    "gensym"
    [ testProperty "respects prefixes" $
        forAll genPrefixes \prefixes ->
            classify (compareLength (nubOrd prefixes) 1 == GT) "distinct horses" $
        let syms = runPureEff (runGensym (mapM gensym prefixes))
         in conjoin $ flip map (zip prefixes syms) \(prefix, sym) ->
            counterexample (unpack $ "gensym " <> show prefix <> " -> " <> show sym) $
            T.isPrefixOf prefix sym

    , testProperty "distinct prefixes" $
        forAll genPrefixes \prefixes ->
            classify (nubOrd prefixes /= prefixes) "duplicate prefixes" $
            counterexample (unpack $ show prefixes) $
        let syms = runPureEff (runGensym (mapM gensym prefixes))
         in nubOrd syms == syms
    ]

rcoTests :: TestTree
rcoTests =
  testGroup
    "removeComplexOperands"
    [ testCase "already monadic" do
        let program =
              Module
                . Let "x" (UnaryOp USub (lint 1))
                $ BinOp Add "x" (lint 2)
            expected =
              MModule
                . MLet "x" (MUnaryOp USub (LitInt 1))
                $ MBinOp Add "x" (LitInt 2)
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "let with an atom" do
        let program =
              Module
                . Let "x" (lint 1)
                $ lint 2
            expected =
              MModule
                . MLet "x" (mlint 1)
                $ mlint 2
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "binop with 2 complex operands" do
        let program =
              Module $
                BinOp Add (UnaryOp USub (lint 1)) InputInt
            expected =
              MModule
                . MLet "t1" (MUnaryOp USub (LitInt 1))
                . MLet "t3" MInputInt
                $ MBinOp Add "t1" "t3"
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "binop with 1 complex operand" do
        let program =
              Module $
                BinOp Add (UnaryOp USub (lint 1)) (lint 2)
            expected =
              MModule
                . MLet "t1" (MUnaryOp USub (LitInt 1))
                $ MBinOp Add "t1" (LitInt 2)
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "print with complex expr" do
        let program =
              Module
                . Print (BinOp Add (lint 1) (lint 2))
                $ lint 0
            expected =
              MModule
                . MLet "t1" (MBinOp Add (LitInt 1) (LitInt 2))
                . MPrint (Name "t1")
                $ mlint 0
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "nested let" do
        let program =
              Module
                . Let "x" (Let "y" (BinOp Add (lint 1) (lint 1)) (BinOp Add "y" (lint 1)))
                $ BinOp Add "x" (lint 10)
            expected =
              MModule
                . MLet "x" (MLet "y" (MBinOp Add (LitInt 1) (LitInt 1)) (MBinOp Add "y" (LitInt 1)))
                $ MBinOp Add "x" (LitInt 10)
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    ]

siTests :: TestTree
siTests =
  testGroup
    "selectInstructions"
    [ testCase "basic move" do
        let program = MModule (MLet "x" (mlint 1) (MAtom (Name "x")))
            expected =
              [ Movq (Var "x") (Imm 1)
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "binop x = x + y" do
        let program = MModule (MLet "x" (MBinOp Add (Name "x") (Name "y")) (MAtom (Name "x")))
            expected =
              [ Addq (Var "x") (Var "y")
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "binop x = y + x" do
        let program = MModule (MLet "x" (MBinOp Sub (Name "y") (Name "x")) (MAtom (Name "x")))
            expected =
              [ Movq (Var "t1") (Var "y")
              , Subq (Var "t1") (Var "x")
              , Movq (Var "x") (Var "t1")
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "unary op x = -y" do
        let program = MModule (MLet "x" (MUnaryOp USub (Name "y")) (MAtom (Name "x")))
            expected =
              [ Movq (Var "x") (Var "y")
              , Negq (Var "x")
              , Movq (Reg Rax) (Var "x")
              ]

        runPureEff (runGensym (selectInstructions program)) @?= expected
    ]

{- FOURMOLU_DISABLE -}
ulTests :: TestTree
ulTests =
  testGroup
    "uncoverLive"
    [ testCase "empty program" do
        uncoverLive [] @?= (Set.empty :| [])
    , testCase "single move" do
        let program =
              [ Movq (Var "x") (Imm 1)
              , Movq (Var "y") (Var "x")
              ]
            expected = Set.empty :| [Set.singleton (Right "x"), Set.empty]
        uncoverLive program @?= expected
    , testCase "complex case" do
        let (program, expected) = unzip
              [ (Movq "v" (Imm 1),  Set.fromList $ Right <$> ["v"])
              , (Movq "w" (Imm 42), Set.fromList $ Right <$> ["w", "v"])
              , (Movq "x" "v",      Set.fromList $ Right <$> ["w", "x"])
              , (Addq "x" (Imm 7),  Set.fromList $ Right <$> ["w", "x"])
              , (Movq "y" "x",      Set.fromList $ Right <$> ["w", "x", "y"])
              , (Movq "z" "x",      Set.fromList $ Right <$> ["w", "y", "z"])
              , (Addq "z" "w",      Set.fromList $ Right <$> ["y", "z"])
              , (Movq "t0" "y",     Set.fromList $ Right <$> ["t0", "z"])
              , (Negq "t0",         Set.fromList $ Right <$> ["t0", "z"])
              , (Movq "t1" "z",     Set.fromList $ Right <$> ["t0", "t1"])
              , (Addq "t1" "t0",    Set.fromList $ Right <$> ["t1"])
              , (Movq rdi "t1",     Set.fromList [Left Rdi])
              , (Callq "print_int", Set.fromList [])
              ]
        uncoverLive program @?= (Set.empty :| expected)
    ]

genInt :: Gen Int
genInt = let bounds = 10 ^ (3 :: Int) in chooseInt (-bounds, bounds)

genInput :: Gen [Text]
genInput = map show <$> infiniteListOf genInt

genAsm :: Gen [AsmVar]
genAsm = sublistOf ['a' .. 'h'] `suchThat` (not . null) >>= (\vars ->
  let weights = iterate (\x -> x - x `div` 3) 1000
      genVar :: Gen (Arg a Avar)
      genVar = frequency . zip weights . map (pure . Var . pack . singleton) $ vars
      genImm = Imm <$> genInt
      genSrc = frequency [(2, genImm), (1, genVar)]
      genDst = genVar
      genArith =
        oneof
          [ Movq <$> genDst <*> genSrc
          , Addq <$> genDst <*> genSrc
          , Subq <$> genDst <*> genSrc
          , Negq <$> genDst
          ]
      genIO =
        oneof
          [ do
              v <- genVar
              pure [Movq rdi v, Callq "print_int"]
          , do
              v <- genVar
              pure [Callq "input_int", Movq rax v]
          ]
      genInst = frequency [(4, singleton <$> genArith), (1, genIO)]
      genInit = forM vars \v -> (Movq . Var . pack . singleton $ v) <$> genImm
   in concat <$> sequence [genInit, concat <$> listOf1 genInst])

arTests :: TestTree
arTests =
  testGroup
    "assignRegister"
    [ testProperty "matches the behaviour of spilling" $
        forAllShow genAsm (unpack . printAsm) \asm ->
        forAllShow genInput (unpack . show . take 20) \input ->
        let asm1 = runPureEff $ assignHomes (assignRegisters asm)
            asm2 = runPureEff $ assignHomes asm
            mkProgram = preludeAndConclusion . second patchInstructions
         in counterexample (unpack . printAsm $ snd asm1) $
            counterexample (unpack . printAsm $ snd asm2) $
        let output1 = fmap snd (runInterpAsm (mkProgram asm1) input)
            output2 = fmap snd (runInterpAsm (mkProgram asm2) input)
         in label (unpack $ case output2 of Right _ -> "execution: successful"; Left e -> "execution: failed with " <> show e) $
            label (unpack $ "has " <> show (length $ fromRight [] output2) <> " lines of output") $
            label (unpack $ "spilled " <> show (fst asm1 `div` 8) <> " vars") $
            label (unpack $ "unspilled " <> show ((fst asm2 - fst asm1) `div` 8) <> " vars") $
            output1 === output2
    ]

ahTests :: TestTree
ahTests =
  testGroup
    "assignHomes"
    [ testCase "spills" do
        let program = [Movq (Var "x") (Imm 0), Movq (Var "y") (Var "x")]
            expected =
              [ Movq (Deref Rbp (-8)) (Imm 0)
              , Movq (Deref Rbp (-16)) (Deref Rbp (-8))
              ]
        runPureEff (assignHomes program) @?= (16, expected)
    , testProperty "behaviour is correct" $
        forAllShow genAsm (unpack . printAsm) \asm ->
        forAllShow genInput (unpack . show . take 20) \input ->
        let asm' = runPureEff $ assignHomes asm
            mkProgram = preludeAndConclusion . second patchInstructions
         in counterexample (unpack . ("assignHomes asm = \n" <>) . printAsm $ snd asm') $
            fmap snd (runInterpAsm (mkProgram asm') input) === fmap snd (runInterpAsmVarSimple asm input)
 ]

piTests :: TestTree
piTests =
  testGroup
    "patchInstructions"
    [ testCase "no patching needed" do
        let program =
              [ Movq (Deref Rbp 0) (Imm 1)
              , Addq (Reg Rax) (Imm 2)
              ]
            expected =
              [ Movq (Deref Rbp 0) (Imm 1)
              , Addq (Reg Rax) (Imm 2)
              ]
        patchInstructions program @?= expected
    , testCase "patches memory-to-memory mov" do
        let program = [Movq (Deref Rbp (-8)) (Deref Rbp (-16))]
            expected =
              [ Movq (Reg Rax) (Deref Rbp (-16))
              , Movq (Deref Rbp (-8)) (Reg Rax)
              ]
        patchInstructions program @?= expected
    , testCase "patches memory + big immediate" do
        let program = [Movq (Deref Rbp (-8)) (Deref Rbp (-16))]
            expected =
              [ Movq (Reg Rax) (Deref Rbp (-16))
              , Movq (Deref Rbp (-8)) (Reg Rax)
              ]
        patchInstructions program @?= expected
    ]

runTypeCheck :: Exp -> Either TypeCheckerError ()
runTypeCheck = runPureEff . runErrorNoCallStack . void . typeCheckL . Module

tcTests :: TestTree
tcTests =
  testGroup
    "typeCheckL"
    [ testCase "unary operations" do
        runTypeCheck (UnaryOp USub (lint 1)) @?= Right ()
        runTypeCheck (UnaryOp Not (lbool True)) @?= Right ()
        assertBool "" (runTypeCheck (UnaryOp USub (lbool True)) & isLeft)
        assertBool "" (runTypeCheck (UnaryOp Not (lint 1)) & isLeft)
    , testCase "binary arithmetic operations" do
        runTypeCheck (BinOp Add (lint 1) (lint 2)) @?= Right ()
        runTypeCheck (BinOp Sub (lint 1) (lint 2)) @?= Right ()
        assertBool "" (runTypeCheck (BinOp Add (lint 1) (lbool True)) & isLeft)
        assertBool "" (runTypeCheck (BinOp Sub (lbool False) (lint 2)) & isLeft)
    , testCase "binary logical operations" do
        runTypeCheck (BinOp And (lbool False) (lbool True)) @?= Right ()
        runTypeCheck (BinOp Or (lbool False) (lbool True)) @?= Right ()
        assertBool "" (runTypeCheck (BinOp And (lint 1) (lbool True)) & isLeft)
        assertBool "" (runTypeCheck (BinOp Or (lbool False) (lint 2)) & isLeft)
    , testCase "equality is polymorphic" do
        runTypeCheck (CmpOp Eq (lbool False) (lbool True)) @?= Right ()
        runTypeCheck (CmpOp Neq (lint 1) (lint 2)) @?= Right ()
        assertBool "" (runTypeCheck (CmpOp Eq (lbool False) (lint 2)) & isLeft)
        assertBool "" (runTypeCheck (CmpOp Neq (lint 1) (lbool True)) & isLeft)
    , testCase "comparison operators" do
        runTypeCheck (CmpOp Lt (lint 1) (lint 2)) @?= Right ()
        runTypeCheck (CmpOp Le (lint 1) (lint 2)) @?= Right ()
        assertBool "" (runTypeCheck (CmpOp Lt (lbool False) (lint 2)) & isLeft)
        assertBool "" (runTypeCheck (CmpOp Le (lint 1) (lbool True)) & isLeft)
    , testCase "let binding" do
        runTypeCheck (Let "x" (lint 1) (BinOp Add "x" (lint 1))) @?= Right ()
        runTypeCheck (Let "x" (lbool True) (BinOp Or "x" (lbool False))) @?= Right ()
    , testCase "unbound variables" do
        let secret = "gcrst"
            result = runTypeCheck (Let "x" (lint 1) (Atom (Name secret)))
        case result of
          Left (UnboundVariable msg)
            | secret `T.isInfixOf` msg -> pure ()
            | otherwise -> assertFailure "the error must contain the variable name"
          Left _ -> assertFailure "the error must be an instance of UnboundVariable"
          Right () -> assertFailure "unbound variables must lead to an error"
    , testCase "shadowing" do
        runTypeCheck (Let "x" (lbool False) $ Let "x" (lint 2) $ BinOp Add "x" (lint 1)) @?= Right ()
        assertBool "" (runTypeCheck (Let "x" (lint 1) $ Let "x" (lbool True) $ BinOp Add "x" (lint 1)) & isLeft)
    , testCase "print" do
        runTypeCheck (Print (lint 1) (lint 0)) @?= Right ()
        assertBool "" (runTypeCheck (Print (lbool False) (lint 0)) & isLeft)
    , testCase "last expression can be any type" do
        runTypeCheck (lint 0) @?= Right ()
        runTypeCheck (lbool True) @?= Right ()
    ]
