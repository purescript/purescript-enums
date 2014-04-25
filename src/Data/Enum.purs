module Data.Enum where

import Data.Maybe

class Enum a where
  toEnum :: Number -> Maybe a
  fromEnum :: a -> Number

succ :: forall a. (Enum a) => a -> Maybe a
succ x = toEnum (fromEnum x + 1)

pred :: forall a. (Enum a) => a -> Maybe a
pred x = toEnum (fromEnum x - 1)