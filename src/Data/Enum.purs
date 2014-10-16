module Data.Enum 
  ( Enum
  , Cardinality(..)
  , cardinality
  , firstEnum
  , fromEnum
  , lastEnum
  , pred
  , runCardinality
  , succ
  , toEnum
  ) where

  import Data.Maybe
  import Data.Tuple
  import Data.Char
  import Data.Maybe.Unsafe

  newtype Cardinality a = Cardinality Number 

  runCardinality :: forall a. Cardinality a -> Number
  runCardinality (Cardinality a) = a

  -- | Type class for enumerations. This should not be considered a part of a
  -- | numeric hierarchy, ala Haskell. Rather, this is a type class for small,
  -- | ordered sum types with statically-determined cardinality and the ability 
  -- | to easily compute successor and predecessor elements. e.g. DayOfWeek, etc.
  -- |
  -- | Laws:
  -- |   succ firstEnum >>= succ >>= succ ... succ [cardinality times] == lastEnum
  -- |   pred lastEnum  >>= pred >>= pred ... pred [cardinality times] == firstEnum
  -- |
  -- |   Just $ e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2
  -- |
  -- |   for all a > firstEnum: pred a >>= succ == Just a
  -- |   for all a < lastEnum:  succ a >>= pred == Just a
  class (Ord a) <= Enum a where
    cardinality :: Cardinality a

    firstEnum :: a

    lastEnum :: a

    succ :: a -> Maybe a

    pred :: a -> Maybe a

  toEnum :: forall a. (Enum a) => Number -> Maybe a
  toEnum n | n < 0 = Nothing
  toEnum 0         = Just firstEnum
  toEnum n         = toEnum (n - 1) >>= succ

  fromEnum :: forall a. (Enum a) => a -> Number
  fromEnum e = maybe 0 ((+) 1 <<< fromEnum) (pred e)

  maybeCardinality :: forall a. (Enum a) => Cardinality a -> Cardinality (Maybe a)
  maybeCardinality c = Cardinality $ 1 + (runCardinality c)

  instance enumChar :: Enum Char where
    cardinality = Cardinality (65535 + 1)

    firstEnum = fromCharCode 0

    lastEnum = fromCharCode 65535

    succ c = if c == lastEnum then Nothing else Just $ (fromCharCode <<< ((+) 1) <<< toCharCode) c

    pred c = if c == firstEnum then Nothing else Just $ (fromCharCode <<< ((+) (-1)) <<< toCharCode) c

  instance enumMaybe :: (Enum a) => Enum (Maybe a) where
    cardinality = maybeCardinality cardinality

    firstEnum = Nothing

    lastEnum = Just $ lastEnum

    succ Nothing = Just $ firstEnum
    succ (Just a) = Just <$> succ a

    pred Nothing = Nothing
    pred (Just a) = Just <$> pred a

  instance enumBoolean :: Enum Boolean where
    cardinality = Cardinality 2
   
    firstEnum = false

    lastEnum = true

    succ false = Just true
    succ _     = Nothing

    pred true  = Just false
    pred _     = Nothing

  instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b) where
    cardinality = tupleCardinality cardinality cardinality

    firstEnum = Tuple firstEnum firstEnum

    lastEnum = Tuple lastEnum lastEnum

    succ (Tuple a b) = maybe (flip Tuple firstEnum <$> succ a) (Just <<< Tuple a) (succ b)

    pred (Tuple a b) = maybe (flip Tuple firstEnum <$> pred a) (Just <<< Tuple a) (pred b)

  tupleCardinality :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Cardinality (Tuple a b)
  tupleCardinality l r = Cardinality $ (runCardinality l) * (runCardinality r)