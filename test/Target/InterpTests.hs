-- | slop
module Target.InterpTests (interpAsmTests) where

import Effects.Lio
import Pre
import Target.Asm
import Target.Interp qualified as TI
import Target.Program
import Test.Tasty
import Test.Tasty.HUnit

-- | Helper to run an assembly program and return the result or an error.
runInterpAsm :: Program -> [Text] -> Either TI.InterpAsmError (Int, [Text])
runInterpAsm program input =
  runPureEff
    . runErrorNoCallStackWith @TI.InterpAsmError (pure . Left)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "LioError during testing" <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ TI.interpAsm program

runInterpAsmVar :: [(Text, [AsmVar])] -> [Text] -> Either TI.InterpAsmError (Int, [Text])
runInterpAsmVar blocks input =
  runPureEff
    . runErrorNoCallStackWith @TI.InterpAsmError (pure . Left)
    . runErrorNoCallStackWith @LioError (\e -> error . unpack $ "LioError during testing" <> show e)
    . fmap (\(v, (_, output)) -> Right (v, output))
    . runLioPure input
    $ TI.interpAsmVar blocks "_start"

-- | Helper for simple single-block programs starting at _start.
mkSimpleProg :: [Asm] -> Program
mkSimpleProg asms = Program ["_start"] [("_start", asms)]

-- | Helper for defining an assertion on an assembly program.
testAsm :: String -> Program -> [Text] -> Either TI.InterpAsmError (Int, [Text]) -> TestTree
testAsm name prog input expected = testCase name $ runInterpAsm prog input @?= expected

interpAsmTests :: TestTree
interpAsmTests =
  testGroup
    "interpAsmTests"
    [ arithmeticTests
    , memoryTests
    , stackTests
    , controlFlowTests
    , ioTests
    , errorTests
    , edgeCaseTests
    , asmVarTests
    ]

asmVarTests :: TestTree
asmVarTests =
  testGroup
    "AsmVar (Variables)"
    [ testCase "basic variable mov" do
        let blocks = [("_start", [Movq (Var "x") (Imm 42), Movq rax (Var "x")])]
        runInterpAsmVar blocks [] @?= Right (42, [])
    , testCase "variable arithmetic" do
        let blocks =
              [ ( "_start"
                , [ Movq (Var "x") (Imm 10)
                  , Movq (Var "y") (Imm 20)
                  , Addq (Var "x") (Var "y")
                  , Movq rax (Var "x")
                  ]
                )
              ]
        runInterpAsmVar blocks [] @?= Right (30, [])
    , testCase "mixing variables and registers" do
        let blocks =
              [ ( "_start"
                , [ Movq (Var "x") (Imm 5)
                  , Movq rbx (Imm 7)
                  , Addq (Var "x") rbx
                  , Movq rax (Var "x")
                  ]
                )
              ]
        runInterpAsmVar blocks [] @?= Right (12, [])
    ]

arithmeticTests :: TestTree
arithmeticTests =
  testGroup
    "Arithmetic"
    [ testAsm
        "Addq: register to register"
        (mkSimpleProg [Movq rax (Imm 10), Movq rbx (Imm 20), Addq rax rbx])
        []
        (Right (30, []))
    , testAsm
        "Subq: immediate from register"
        (mkSimpleProg [Movq rax (Imm 100), Subq rax (Imm 30)])
        []
        (Right (70, []))
    , testAsm
        "Negq: register"
        (mkSimpleProg [Movq rax (Imm 42), Negq rax])
        []
        (Right (-42, []))
    , testAsm
        "Subq: resulting in negative"
        (mkSimpleProg [Movq rax (Imm 10), Subq rax (Imm 20)])
        []
        (Right (-10, []))
    ]

memoryTests :: TestTree
memoryTests =
  testGroup
    "Memory (Deref)"
    [ testAsm
        "Movq: Imm to Deref and back"
        (mkSimpleProg [Movq (Deref Rsp 0) (Imm 123), Movq rax (Deref Rsp 0)])
        []
        (Right (123, []))
    , testAsm
        "Addq: Deref to register"
        (mkSimpleProg [Movq (Deref Rsp 0) (Imm 10), Movq rax (Imm 20), Addq rax (Deref Rsp 0)])
        []
        (Right (30, []))
    , testAsm
        "Subq: register to Deref"
        (mkSimpleProg [Movq (Deref Rsp 0) (Imm 100), Movq rbx (Imm 40), Subq (Deref Rsp 0) rbx, Movq rax (Deref Rsp 0)])
        []
        (Right (60, []))
    ]

stackTests :: TestTree
stackTests =
  testGroup
    "Stack Operations"
    [ testAsm
        "Pushq/Popq LIFO"
        ( mkSimpleProg
            [ Pushq (Imm 1)
            , Pushq (Imm 2)
            , Pushq (Imm 3)
            , Popq rbx
            , Popq rcx
            , Popq rax
            , Addq rax rbx
            , Addq rax rcx
            ]
        )
        []
        (Right (6, []))
    , testAsm
        "Pushq from register"
        (mkSimpleProg [Movq rbx (Imm 99), Pushq rbx, Movq rbx (Imm 0), Popq rax])
        []
        (Right (99, []))
    , testAsm
        "Pushq/Popq with Deref Rsp"
        ( mkSimpleProg
            [ Pushq (Imm 42)
            , Popq (Deref Rsp 0) -- This is tricky, but in x86 it works. Pop reads [rsp], then increments rsp, then writes to dst.
            , Popq rax
            ]
        )
        []
        (Right (42, []))
    ]

controlFlowTests :: TestTree
controlFlowTests =
  testGroup
    "Control Flow (Call/Ret)"
    [ testAsm
        "Nested calls"
        ( Program
            ["_start"]
            [ ("_start", [Callq "f1", Retq])
            , ("f1", [Callq "f2", Addq rax (Imm 1), Retq])
            , ("f2", [Movq rax (Imm 10), Retq])
            ]
        )
        []
        (Right (11, []))
    , testAsm
        "Call with arguments in registers"
        ( Program
            ["_start"]
            [ ("_start", [Movq rdi (Imm 10), Movq rsi (Imm 20), Callq "add", Retq])
            , ("add", [Movq rax rdi, Addq rax rsi, Retq])
            ]
        )
        []
        (Right (30, []))
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "I/O (print_int, input_int)"
    [ testAsm
        "Multiple print_int order"
        ( mkSimpleProg
            [ Movq rdi (Imm 1)
            , Callq "print_int"
            , Movq rdi (Imm 2)
            , Callq "print_int"
            , Movq rdi (Imm 3)
            , Callq "print_int"
            , Movq rax (Imm 0)
            ]
        )
        []
        (Right (0, ["1", "2", "3"]))
    , testAsm
        "input_int consumption"
        ( mkSimpleProg
            [ Callq "input_int"
            , Movq rbx rax
            , Callq "input_int"
            , Addq rax rbx
            ]
        )
        ["5", "7"]
        (Right (12, []))
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "Errors"
    [ testAsm
        "Unknown label"
        (mkSimpleProg [Callq "foo"])
        []
        (Left (TI.UnknownLabel "foo"))
    , testAsm
        "Uninitialized memory access"
        (mkSimpleProg [Movq rax (Deref Rsp (-8))])
        []
        (Left (TI.InvalidMemoryAccess 999992))
    ]

edgeCaseTests :: TestTree
edgeCaseTests =
  testGroup
    "Edge Cases"
    [ testAsm
        "Large immediate"
        (mkSimpleProg [Movq rax (Imm 0x7FFFFFFFFFFFFFFF)])
        []
        (Right (0x7FFFFFFFFFFFFFFF, []))
    , testAsm
        "Self-addition"
        (mkSimpleProg [Movq rax (Imm 21), Addq rax rax])
        []
        (Right (42, []))
    , testAsm
        "Push and Pop same register"
        (mkSimpleProg [Movq rax (Imm 1), Pushq rax, Movq rax (Imm 2), Popq rax])
        []
        (Right (1, []))
    ]
