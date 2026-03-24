{-# LANGUAGE TemplateHaskell #-}

module Effects.Lio (
  Lio,
  lioInputLine,
  lioPrintLine,
  runLioIO,
  runLioPure,
  LioError (..),
) where

import Effectful.State.Static.Local
import Effectful.TH
import Pre

data Lio :: Effect where
  LioInputLine :: Lio m Text
  LioPrintLine :: Text -> Lio m ()

type instance DispatchOf Lio = Dynamic

newtype LioError = LioError Text
  deriving (Eq, Show)

makeEffect ''Lio

runLioIO :: (IOE :> es, Error LioError :> es) => Eff (Lio : es) a -> Eff es a
runLioIO = interpret \_ -> \case
  LioInputLine -> adapt getLine
  LioPrintLine line -> adapt $ putStrLn line
 where
  adapt m = liftIO m `catchIO` \e -> throwError . LioError $ show e

-- | a bit dirty
runLioPure
  :: (Error LioError :> es)
  => [Text]
  -> Eff (Lio : es) a
  -> Eff es (a, ([Text], [Text]))
runLioPure inputs = reinterpret (runState (inputs, [])) \_ -> \case
  LioInputLine ->
    gets fst >>= \case
      [] -> throwError . LioError $ "No more input"
      input : rest -> input <$ modify (second (const rest))
  LioPrintLine line -> modify (second (line :))
