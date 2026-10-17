module Data.Enum.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Enum (class BoundedEnum, enumFromTo, succ)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))

-- | Create a random generator for a finite enumeration.
genBoundedEnum :: forall m a. MonadGen m => BoundedEnum a => m a
genBoundedEnum =
  case succ bottom of
    Just a -> do
      let possibilities = enumFromTo a top :: Array a
      elements (bottom :| possibilities)
    Nothing ->
      pure bottom
