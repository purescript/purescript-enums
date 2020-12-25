module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Enum (testEnum)
import Test.Data.Enum.Generic (testGenericEnum)

main :: Effect Unit
main = do
  testEnum
  testGenericEnum
