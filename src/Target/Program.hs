module Target.Program where

import Pre
import Target.Asm

data Program = Program
  { globals :: [Text]
  , asm :: [(Text, [Asm])]
  }
  deriving (Eq, Show, Read)
