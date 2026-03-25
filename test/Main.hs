module Main (main) where

import Data.Text qualified as T
import Effects.Gensym
import Effects.Lio
import Lang
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
        peExp (BinOp Add (Constant 1) (Constant 2))
          @?= Constant 3
    , testCase "pe stops at InputInt" do
        peExp (BinOp Add InputInt (BinOp Add (Constant 1) (Constant 2)))
          @?= BinOp Add InputInt (Constant 3)
          -- , testCase "pe understands associativity" $
          --     peExp (BinOp Add (Constant 1) (BinOp Add (Constant 2) InputInt))
          --       @?= BinOp Add (Constant 3) (InputInt)
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
        runInterpSimple (Module (Expr (BinOp Add (Constant 1) (Constant 2))))
          @?= Right 3
    , testCase "variables work" do
        runInterpSimple (Module (Let "x" (Constant 1) (Expr (BinOp Add "x" (Constant 2)))))
          @?= Right 3
    , testCase "input and output" do
        runInterpL (Module (Print (BinOp Add (Constant 1) InputInt) (Expr (Constant 0)))) ["2"]
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
