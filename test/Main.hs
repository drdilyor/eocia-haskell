module Main (main) where

import Lang
import Pre
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [peTests]

peTests :: TestTree
peTests =
  testGroup
    "peExp"
    [ testCase "pe does evaluate" $
        peExp (BinOp Add (Constant 1) (Constant 2))
          @?= Constant 3
    , testCase "pe stops at InputInt" $
        peExp (BinOp Add InputInt (BinOp Add (Constant 1) (Constant 2)))
          @?= BinOp Add InputInt (Constant 3)
    -- , testCase "pe understands associativity" $
    --     peExp (BinOp Add (Constant 1) (BinOp Add InputInt (Constant 2)))
    --       @?= BinOp Add InputInt (Constant 3)
    ]
