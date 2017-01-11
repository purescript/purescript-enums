module Test.Data.Generic.Rep.Enum(testGeneric) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericSucc, genericPred,
                              genericFromEnum, genericToEnum,
			      genericCardinality)
import Data.Enum (class Enum, class BoundedEnum, succ, pred,
                  Cardinality(..), cardinality, fromEnum, toEnum)
import Data.Maybe (Maybe(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Assert (ASSERT, assert)

data Small = A | B
derive instance eqSmall  :: Eq  Small
derive instance ordSmall :: Ord Small
derive instance genericSmall :: Generic Small _
instance boundedSmall :: Bounded Small where
  bottom = A
  top = B
instance enumSmall :: Enum Small where
  succ = genericSucc
  pred = genericPred
instance boundedEnumSmall :: BoundedEnum Small where
  fromEnum = genericFromEnum
  toEnum = genericToEnum
  cardinality = genericCardinality

data Big = C | D | E | F | G
derive instance eqBig  :: Eq  Big
derive instance ordBig :: Ord Big
derive instance genericBig :: Generic Big _
instance boundedBig :: Bounded Big where
  bottom = C
  top = G
instance enumBig :: Enum Big where
  succ = genericSucc
  pred = genericPred
instance boundedEnumBig :: BoundedEnum Big where
  fromEnum = genericFromEnum
  toEnum = genericToEnum
  cardinality = genericCardinality

testGeneric :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
testGeneric = do
  log "2-constructor Generic Enum"
  assert $ succ A == Just B
  assert $ succ B == Nothing

  assert $ pred A == Nothing
  assert $ pred B == Just A

  assert $ cardinality == Cardinality 2 :: Cardinality Small

  assert $ fromEnum A == 0
  assert $ fromEnum B == 1

  assert $ toEnum 0 == Just A
  assert $ toEnum 1 == Just B
  assert $ (toEnum 2 :: Maybe Small) == Nothing
  assert $ (toEnum (-1) :: Maybe Small) == Nothing

  log "5-constructor Generic Enum"
  assert $ succ C == Just D
  assert $ succ D == Just E
  assert $ succ E == Just F
  assert $ succ F == Just G
  assert $ succ G == Nothing

  assert $ pred C == Nothing
  assert $ pred D == Just C
  assert $ pred E == Just D
  assert $ pred F == Just E
  assert $ pred G == Just F

  assert $ cardinality == Cardinality 5 :: Cardinality Big

  assert $ fromEnum C == 0
  assert $ fromEnum D == 1
  assert $ fromEnum E == 2
  assert $ fromEnum F == 3
  assert $ fromEnum G == 4

  assert $ toEnum 0 == Just C
  assert $ toEnum 1 == Just D
  assert $ toEnum 2 == Just E
  assert $ toEnum 3 == Just F
  assert $ toEnum 4 == Just G
  assert $ (toEnum 5 :: Maybe Big) == Nothing
  assert $ (toEnum (-1) :: Maybe Big) == Nothing
