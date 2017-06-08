module Data.Enum.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

-- | Create a random generator for a finite enumeration. `toEnum i` must be
-- | well-behaved: it must return a `Just` value for all `Int`s between
-- | `fromEnum bottom` and `fromEnum top`.
genBoundedEnum :: forall m a. MonadGen m => BoundedEnum a => m a
genBoundedEnum =
  unsafePartial fromJust <<< toEnum
    <$> chooseInt (fromEnum (bottom :: a)) (fromEnum (top :: a))
