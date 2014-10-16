# Module Documentation

## Module Data.Enum

### Types

    newtype Cardinality a where
      Cardinality :: Number -> Cardinality a


### Type Classes

    class (Ord a) <= Enum a where
      cardinality :: Cardinality a
      firstEnum :: a
      lastEnum :: a
      succ :: a -> Maybe a
      pred :: a -> Maybe a


### Type Class Instances

    instance enumBoolean :: Enum Boolean

    instance enumChar :: Enum Char

    instance enumMaybe :: (Enum a) => Enum (Maybe a)

    instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b)


### Values

    fromEnum :: forall a. (Enum a) => a -> Number

    runCardinality :: forall a. Cardinality a -> Number

    toEnum :: forall a. (Enum a) => Number -> Maybe a