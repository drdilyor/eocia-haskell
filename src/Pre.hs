module Pre (
  module Prelude,
  module Data.List,
  module Data.List.NonEmpty,
  module Data.Containers.ListUtils,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Void,
  module Data.Function,
  module Data.Functor,
  module Data.Functor.Foldable,
  module Data.Functor.Foldable.TH,
  module Control.Applicative,
  module Control.Monad,
  module Control.Comonad.Cofree,
  module Control.Arrow,
  module GHC.Generics,
  module Text.Read,
  module Data.String,
  module Data.Text,
  module Data.Text.Encoding,
  module Data.Text.IO,
  module Effectful,
  module Effectful.Dispatch.Dynamic,
  module Effectful.State.Static.Local,
  module Effectful.Error.Static,
  module Effectful.Exception,
) where

import Control.Applicative
import Control.Arrow
import Control.Comonad.Cofree hiding (unfold)
import Control.Monad
import Data.Containers.ListUtils
import Data.Function
import Data.Functor hiding (unzip)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List hiding (lines, unlines, unwords, words)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text, lines, pack, show, unlines, unpack, unwords, words)
import Data.Text.Encoding
import Data.Text.IO
import Data.Void
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Effectful.State.Static.Local
import GHC.Generics (Generic, Generic1, Generically, Generically1)
import Text.Read hiding (get, (+++))
import Prelude hiding (appendFile, getContents, getLine, interact, lines, print, putStr, putStrLn, readFile, show, unlines, unwords, words, writeFile)
