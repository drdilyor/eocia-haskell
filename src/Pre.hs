module Pre (
  module Prelude,
  module Data.List,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Function,
  module Data.Functor,
  module Data.Functor.Base,
  module Data.Functor.Foldable,
  module Data.Functor.Foldable.TH,
  module Control.Applicative,
  module Control.Monad,
  module Control.Arrow,
  module Text.Read,
  module Data.Text,
  module Data.Text.Encoding,
  module Data.Text.IO,
  module Effectful,
  module Effectful.Dispatch.Dynamic,
  module Effectful.Error.Static,
  module Effectful.Exception,
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Functor hiding (unzip)
import Data.Functor.Base
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, show, unpack)
import Data.Text.Encoding
import Data.Text.IO
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Text.Read hiding ((+++))
import Prelude hiding (appendFile, getContents, getLine, interact, print, putStr, putStrLn, readFile, show, writeFile)
