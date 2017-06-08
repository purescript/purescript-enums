module Data.Enum.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, elements)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.NonEmpty ((:|))

-- | Create a random generator for a finite enumeration.
genBoundedEnum :: forall m a. MonadGen m => BoundedEnum a => m a
genBoundedEnum =
  let possibilities = enumFromTo (bottom :: a) (top :: a) :: Array a
  in elements (bottom :| possibilities)
