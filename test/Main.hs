module Main (main) where

import Data.Text qualified as T
import Effects.Gensym
import Effects.Lio
import Lang
import Pipeline
import Pre
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [peTests, interpTests, gensymTests]

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
        runInterpSimple (Module (Expr (BinOp Add (lint 1) (lint 2))))
          @?= Right 3
    , testCase "variables work" do
        runInterpSimple (Module (Let "x" (lint 1) (Expr (BinOp Add "x" (lint 2)))))
          @?= Right 3
    , testCase "input and output" do
        runInterpL (Module (Print (BinOp Add (lint 1) InputInt) (Expr (lint 0)))) ["2"]
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
                    prefix `T.isPrefixOf` sym
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
        runPureEff (runGensym (removeComplexOperands (Module (Let "x" (UnaryOp USub (lint 1)) (Expr (BinOp Add "x" (lint 2)))))))
          @?= MModule (MLet "x" (MUnaryOp USub (Lit 1)) (MExpr (MBinOp Add "x" (Lit 2))))
    , testCase "let with an atom" do
        runPureEff (runGensym (removeComplexOperands (Module (Let "x" (lint 1) (Expr (lint 2))))))
          @?= MModule (MLet "x" (mlint 1) (MExpr (mlint 2)))
    , testCase "print with complex expr" do
        runPureEff (runGensym (removeComplexOperands (Module (Print (BinOp Add (lint 1) (lint 2)) (Expr (lint 0))))))
          @?= MModule (MLet "t1" (MBinOp Add (Lit 1) (Lit 2)) (MPrint (Name "t1") (MExpr (MAtom (Lit 0)))))
    ]
