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
      toEnum :: Number -> Maybe a
      fromEnum :: a -> Number


### Type Class Instances

    instance enumBoolean :: Enum Boolean

    instance enumChar :: Enum Char

    instance enumEither :: (Enum a, Enum b) => Enum (Either a b)

    instance enumMaybe :: (Enum a) => Enum (Maybe a)

    instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b)


### Values

    defaultFromEnum :: forall a. (a -> Maybe a) -> a -> Number

    defaultPred :: forall a. (Number -> Maybe a) -> (a -> Number) -> a -> Maybe a

    defaultSucc :: forall a. (Number -> Maybe a) -> (a -> Number) -> a -> Maybe a

    defaultToEnum :: forall a. (a -> Maybe a) -> a -> Number -> Maybe a

    enumFromThenTo :: forall a. (Enum a) => a -> a -> a -> [a]

    enumFromTo :: forall a. (Enum a) => a -> a -> [a]

    intFromTo :: Number -> Number -> [Number]

    intStepFromTo :: Number -> Number -> Number -> [Number]

    runCardinality :: forall a. Cardinality a -> Number