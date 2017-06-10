module Test.Data.Enum (testEnum) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Newtype (unwrap)
import Data.Enum (class Enum, class BoundedEnum, defaultToEnum, defaultFromEnum,
                  defaultCardinality, enumFromTo, enumFromThenTo, upFrom, upFromIncluding,
                  downFrom, toEnum, fromEnum, Cardinality, cardinality)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Either (Either(..))

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
  cardinality = defaultCardinality
  toEnum = defaultToEnum
  fromEnum = defaultFromEnum

testEnum :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
testEnum = do
  log "enumFromTo"
  assert $ enumFromTo A A == [A]
  assert $ enumFromTo B A == []
  assert $ enumFromTo A C == [A, B, C]
  assert $ enumFromTo A E == [A, B, C, D, E]

  log "enumFromThenTo"
  assert $ enumFromThenTo A B E == [A, B, C, D, E]
  assert $ enumFromThenTo A C E == [A,    C,    E]
  assert $ enumFromThenTo A E E == [A,          E]
  assert $ enumFromThenTo A C C == [A,    C      ]
  assert $ enumFromThenTo A C D == [A,    C      ]

  log "upFrom"
  assert $ upFrom B == [C, D, E]
  assert $ upFrom D == [      E]
  assert $ upFrom E == [       ]

  log "upFromIncluding"
  assert $ upFromIncluding B == B :| [C, D, E]
  assert $ upFromIncluding D == D :| [      E]
  assert $ upFromIncluding E == E :| [       ]

  log "downFrom"
  assert $ downFrom D == [C, B, A]
  assert $ downFrom B == [      A]
  assert $ downFrom A == [       ]

  log "BoundedEnum (Maybe Boolean)"
  assert $ toEnum (-1) == Nothing :: Maybe (Maybe Boolean)
  assert $ toEnum 0 == Just Nothing :: Maybe (Maybe Boolean)
  assert $ toEnum 1 == Just (Just false) :: Maybe (Maybe Boolean)
  assert $ toEnum 2 == Just (Just true) :: Maybe (Maybe Boolean)
  assert $ toEnum 3 == Nothing :: Maybe (Maybe Boolean)

  log "BoundedEnum (Either _ _)"
  assert $ unwrap (cardinality :: Cardinality (Either Boolean Boolean)) == 4
  assert $ toEnum 0 == Just (Left false :: Either Boolean T)
  assert $ toEnum 1 == Just (Left true :: Either Boolean T)
  assert $ toEnum 3 == Just (Right B :: Either Boolean T)
  assert $ fromEnum (Left false :: Either Boolean T) == 0
  assert $ fromEnum (Left true :: Either Boolean T) == 1
  assert $ fromEnum (Right B :: Either Boolean T) == 3
