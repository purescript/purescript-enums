module Test.Data.Enum.Range (testRange) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array.NonEmpty (toArray)
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), cardinality, defaultFromEnum, toEnum)
import Data.Enum.Range (Range, range, toNonEmptyArray, cardinalityFromRange, toEnumFromRange)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)

data T = A | B | C | D | E

derive instance eqT  :: Eq  T
derive instance ordT :: Ord T

instance enumT :: Enum T where
  succ A = Just B
  succ B = Just C
  succ C = Just D
  succ D = Just E
  succ E = Nothing

  pred A = Nothing
  pred B = Just A
  pred C = Just B
  pred D = Just C
  pred E = Just D

instance boundedT :: Bounded T where
  bottom = A
  top = E

instance boundedEnumT :: BoundedEnum T where
  cardinality = cardinalityFromRange rangeT
  toEnum = toEnumFromRange rangeT
  fromEnum = defaultFromEnum

rangeT :: Range T
rangeT = range

testRange :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
testRange = do
  log "toNonEmptyArray"
  assert $ toArray (toNonEmptyArray rangeT) == [A, B, C, D, E]

  log "cardinalityFromRange"
  assert $ cardinality == (Cardinality 5 :: Cardinality T)

  log "toEnumFromRange"
  assert $ toEnum 0 == Just A
  assert $ toEnum 1 == Just B
  assert $ toEnum 2 == Just C
  assert $ toEnum 3 == Just D
  assert $ toEnum 4 == Just E
