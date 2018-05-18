module Test.Data.Enum (testEnum) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, defaultCardinality, defaultFromEnum, defaultToEnum, downFrom, enumFromThenTo, enumFromTo, upFrom, upFromIncluding)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

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

testEnum :: Effect Unit
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
