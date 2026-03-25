module Main (main) where

import Data.Text qualified as T
import Effects.Gensym
import Effects.Lio
import Lang
import Pipeline
import Pre
import Target.Asm
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [peTests, interpTests, gensymTests, rcoTests, siTests, ahTests, piTests]

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

runInterpL :: L -> [Text] -> Either InterpError (Int, [Text])
runInterpL program input =
  runPureEff
    . runErrorNoCallStackWith @InterpError (pure . Left)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "LioError during testing" <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ interpL program

runInterpSimple :: L -> Either InterpError Int
runInterpSimple program = fst <$> runInterpL program []

interpTests :: TestTree
interpTests =
  testGroup
    "interpTests"
    [ testCase "basic arithmetic" do
        ( runInterpSimple
            . Module
            $ Expr (BinOp Add (lint 1) (lint 2))
          )
          @?= Right 3
    , testCase "variables work" do
        ( runInterpSimple
            . Module
            . Let "x" (lint 1)
            $ Expr (BinOp Add "x" (lint 2))
          )
          @?= Right 3
    , testCase "input and output" do
        ( runInterpL
            . Module
            . Print (BinOp Add (lint 1) InputInt)
            $ Expr (lint 0)
          )
          ["2"]
          @?= Right (0, ["3"])
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
            let prop = conjoin $ zipWith checkPrefix prefixes syms
                syms = runPureEff (runGensym (mapM gensym prefixes))
                checkPrefix prefix sym =
                  counterexample (unpack $ "gensym " <> show prefix <> " -> " <> show sym) $
                    T.isPrefixOf prefix sym
             in prop
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
                $ Expr (BinOp Add "x" (lint 2))
            expected =
              MModule
                . MLet "x" (MUnaryOp USub (Lit 1))
                $ MExpr (MBinOp Add "x" (Lit 2))
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "let with an atom" do
        let program =
              Module
                . Let "x" (lint 1)
                $ Expr (lint 2)
            expected =
              MModule
                . MLet "x" (mlint 1)
                $ MExpr (mlint 2)
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "binop with 2 complex operands" do
        let program =
              Module $
                Expr (BinOp Add (UnaryOp USub (lint 1)) InputInt)
            expected =
              MModule
                . MLet "t1" (MUnaryOp USub (Lit 1))
                . MLet "t2" MInputInt
                $ MExpr (MBinOp Add "t1" "t2")
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "binop with 1 complex operand" do
        let program =
              Module $
                Expr (BinOp Add (UnaryOp USub (lint 1)) (lint 2))
            expected =
              MModule
                . MLet "t1" (MUnaryOp USub (Lit 1))
                $ MExpr (MBinOp Add "t1" (Lit 2))
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    , testCase "print with complex expr" do
        let program =
              Module
                . Print (BinOp Add (lint 1) (lint 2))
                $ Expr (lint 0)
            expected =
              MModule
                . MLet "t1" (MBinOp Add (Lit 1) (Lit 2))
                . MPrint (Name "t1")
                $ MExpr (mlint 0)
        runPureEff (runGensym (removeComplexOperands program)) @?= expected
    ]

siTests :: TestTree
siTests =
  testGroup
    "selectInstructions"
    [ testCase "basic move" do
        let program = MModule (MLet "x" (mlint 1) (MExpr (MAtom (Name "x"))))
            expected =
              [ Movq (Var "x") (Imm 1)
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "binop x = x + y" do
        let program = MModule (MLet "x" (MBinOp Add (Name "x") (Name "y")) (MExpr (MAtom (Name "x"))))
            expected =
              [ Addq (Var "x") (Var "y")
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "binop x = y + x" do
        let program = MModule (MLet "x" (MBinOp Sub (Name "y") (Name "x")) (MExpr (MAtom (Name "x"))))
            expected =
              [ Movq (Var "t1") (Var "y")
              , Subq (Var "t1") (Var "x")
              , Movq (Var "x") (Var "t1")
              , Movq (Reg Rax) (Var "x")
              ]
        runPureEff (runGensym (selectInstructions program)) @?= expected
    , testCase "unary op x = -y" do
        let program = MModule (MLet "x" (MUnaryOp USub (Name "y")) (MExpr (MAtom (Name "x"))))
            expected =
              [ Movq (Var "x") (Var "y")
              , Negq (Var "x")
              , Movq (Reg Rax) (Var "x")
              ]

        runPureEff (runGensym (selectInstructions program)) @?= expected
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
    ]
