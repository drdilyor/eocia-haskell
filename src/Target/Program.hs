module Target.Program where

import Pre
import Target.Asm
import Data.Text qualified as T

data Program = Program
  { globals :: [Text]
  , asm :: [(Text, [Asm])]
  }
  deriving (Eq, Show, Read)

printProgram :: Program -> Text
printProgram Program{..} =
  unlines
    [ ".globl " <> T.intercalate ", " globals
    , unlines $ map (\(label, asm') -> "" <> label <> ":\n" <> printAsm asm') asm
    ]
