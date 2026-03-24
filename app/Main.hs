module Main (main) where

import Pre

main :: IO ()
main = runEff do
  liftIO $ putStrLn "hello"
