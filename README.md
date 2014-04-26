# Module Documentation

## Module Data.Enum

### Type Classes

    class Enum a where
      toEnum :: Prim.Number -> Maybe a
      fromEnum :: a -> Prim.Number


### Values

    pred :: forall a. (Enum a) => a -> Maybe a

    succ :: forall a. (Enum a) => a -> Maybe a