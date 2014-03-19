module Data.Enum where

import Data.Maybe

class Enum a where
  toEnum :: Number -> Maybe a
  fromEnum :: a -> Number
