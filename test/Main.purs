module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)
import Test.Data.Enum (testEnum)
import Test.Data.Enum.Range (testRange)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  testEnum
  testRange
