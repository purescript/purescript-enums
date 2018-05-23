module Test.Data.Enum (testEnum) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, defaultCardinality, defaultFromEnum, defaultToEnum, downFrom, downFromIncluding, enumFromThenTo, enumFromTo, upFrom, upFromIncluding)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

data T = A | B | C | D | E

derive instance eqT  :: Eq  T
derive instance ordT :: Ord T

instance showT :: Show T where
  show = case _ of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"

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
  assertEqual
    { actual: enumFromTo A A
    , expected: [A]
    }
  assertEqual
    { actual: enumFromTo B A
    , expected: [B, A]
    }
  assertEqual
    { actual: enumFromTo A C
    , expected: [A, B, C]
    }
  assertEqual
    { actual: enumFromTo A E
    , expected: [A, B, C, D, E]
    }
  assertEqual
    { actual: enumFromTo 0 3
    , expected: [0, 1, 2, 3]
    }
  assertEqual
    { actual: enumFromTo 'c' 'a'
    , expected: ['c', 'b', 'a']
    }

  log "enumFromThenTo"
  assertEqual
    { actual: enumFromThenTo A B E
    , expected: [A, B, C, D, E]
    }
  assertEqual
    { actual: enumFromThenTo A C E
    , expected: [A, C, E]
    }
  assertEqual
    { actual: enumFromThenTo A E E
    , expected: [A, E]
    }
  assertEqual
    { actual: enumFromThenTo A C C
    , expected: [A, C]
    }
  assertEqual
    { actual: enumFromThenTo A C D
    , expected: [A, C]
    }

  log "upFrom"
  assertEqual
    { actual: upFrom B
    , expected: [C, D, E]
    }
  assertEqual
    { actual: upFrom D
    , expected: [E]
    }
  assertEqual
    { actual: upFrom E
    , expected: []
    }

  log "upFromIncluding"
  assertEqual
    { actual: upFromIncluding B
    , expected: [B, C, D, E]
    }
  assertEqual
    { actual: upFromIncluding B
    , expected: B :| [C, D, E]
    }
  assertEqual
    { actual: upFromIncluding D
    , expected: D :| [E]
    }
  assertEqual
    { actual: upFromIncluding E
    , expected: E :| []
    }

  log "downFrom"
  assertEqual
    { actual: downFrom D
    , expected: [C, B, A]
    }
  assertEqual
    { actual: downFrom B
    , expected: [A]
    }
  assertEqual
    { actual: downFrom A
    , expected: []
    }

  log "downFromIncluding"
  assertEqual
    { actual: downFromIncluding D
    , expected: [D, C, B, A]
    }
  assertEqual
    { actual: downFromIncluding B
    , expected: [B, A]
    }
  assertEqual
    { actual: downFromIncluding A
    , expected: [A]
    }
