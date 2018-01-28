module Data.Enum.Range
  ( Range
  , range
  , toNonEmptyArray
  , cardinalityFromRange
  , toEnumFromRange
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty, index, length)
import Data.Enum (class Enum, Cardinality(..), upFromIncluding)
import Data.Maybe (Maybe)

newtype Range a = Range (NonEmptyArray a)

-- smart constructor for Range
range :: forall a. Bounded a => Enum a => Range a
range = Range $ fromNonEmpty $ upFromIncluding bottom

toNonEmptyArray :: forall a. Range a -> NonEmptyArray a
toNonEmptyArray (Range xs) = xs

-- | Runs in `O(1)`
cardinalityFromRange :: forall a. Range a -> Cardinality a
cardinalityFromRange = Cardinality <<< length <<< toNonEmptyArray

-- | Runs in `O(1)`
toEnumFromRange :: forall a. Range a -> Int -> Maybe a
toEnumFromRange = index <<< toNonEmptyArray
