{-# LANGUAGE TemplateHaskell #-}

module Effects.Gensym where

import Effectful.TH
import Pre

data Gensym :: Effect where
  Gensym :: Text -> Gensym m Text

type instance DispatchOf Gensym = Dynamic

makeEffect ''Gensym

runGensym :: Eff (Gensym : es) a -> Eff es a
runGensym = reinterpret (evalState (0 :: Int)) \_ (Gensym prefix) -> do
  modify (+ 1)
  (prefix <>) . show <$> get
