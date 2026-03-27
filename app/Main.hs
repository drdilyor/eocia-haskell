module Main (main) where

import Parser
import Pipeline
import Pre
import Target.Program
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  source <- getContents
  case runParser parseL "stdin" source of
    Left e -> putStr . pack $ errorBundlePretty e
    Right l ->
      case compile l of
        Left e -> print e
        Right program -> putStr . printProgram $ program
