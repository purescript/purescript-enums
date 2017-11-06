module Data.Enum.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, elements)
import Data.Enum (class BoundedEnum, Cardinality, cardinality, enumFromTo, fromEnum, succ, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Partial.Unsafe (unsafePartial)

-- | Create a random generator for a finite enumeration.
genBoundedEnum :: forall m a. MonadGen m => BoundedEnum a => m a
genBoundedEnum =
  let topInt = fromEnum (top :: a)
      bottomInt = fromEnum (bottom :: a)
      enumRange = topInt - bottomInt
  in if enumRange == unwrap (cardinality :: Cardinality a)
       then unsafePartial $ fromJust <<< toEnum <$> chooseInt bottomInt topInt
       else case succ bottom of
         Just a →
           let possibilities = enumFromTo a top :: Array a
           in elements (bottom :| possibilities)
         Nothing →
           pure bottom
