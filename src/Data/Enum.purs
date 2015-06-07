module Data.Enum
  ( Enum
  , Cardinality(..)
  , cardinality
  , fromEnum
  , pred
  , runCardinality
  , succ
  , toEnum
  , defaultSucc
  , defaultPred
  , defaultToEnum
  , defaultFromEnum
  , intFromTo
  , intStepFromTo
  , enumFromTo
  , enumFromThenTo
  ) where

import Prelude
import Data.Char (fromCharCode, toCharCode)
import Data.Either
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Data.Unfoldable

newtype Cardinality a = Cardinality Int

runCardinality :: forall a. Cardinality a -> Int
runCardinality (Cardinality a) = a

-- | Type class for enumerations. This should not be considered a part of a
-- | numeric hierarchy, ala Haskell. Rather, this is a type class for small,
-- | ordered sum types with statically-determined cardinality and the ability
-- | to easily compute successor and predecessor elements. e.g. `DayOfWeek`, etc.
-- |
-- | Laws:
-- |
-- | - ```succ bottom >>= succ >>= succ ... succ [cardinality - 1 times] == top```
-- | - ```pred top    >>= pred >>= pred ... pred [cardinality - 1 times] == bottom```
-- | - ```e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2```
-- | - ```forall a > bottom: pred a >>= succ == Just a```
-- | - ```forall a < top:  succ a >>= pred == Just a```
-- | - ```pred >=> succ >=> pred = pred```
-- | - ```succ >=> pred >=> succ = succ```
-- | - ```toEnum (fromEnum a) = Just a```
-- | - ```forall a > bottom: fromEnum <$> pred a = Just (fromEnum a - 1)```
-- | - ```forall a < top:  fromEnum <$> succ a = Just (fromEnum a + 1)```


class (Bounded a) <= Enum a where
  cardinality :: Cardinality a
  succ :: a -> Maybe a
  pred :: a -> Maybe a
  toEnum :: Int -> Maybe a
  fromEnum :: a -> Int

-- | ```defaultSucc toEnum fromEnum = succ```
defaultSucc :: forall a. (Int -> Maybe a) -> (a -> Int) -> (a -> Maybe a)
defaultSucc toEnum' fromEnum' a = toEnum' (fromEnum' a + one)

-- | ```defaultPred toEnum fromEnum = pred```
defaultPred :: forall a. (Int -> Maybe a) -> (a -> Int) -> (a -> Maybe a)
defaultPred toEnum' fromEnum' a = toEnum' (fromEnum' a - one)

-- | Runs in `O(n)` where `n` is `fromEnum a`
-- |
-- | ```defaultToEnum succ bottom = toEnum```
defaultToEnum :: forall a. (a -> Maybe a) -> a -> (Int -> Maybe a)
defaultToEnum succ' bottom' n | n < zero = Nothing
                              | n == zero = Just bottom'
                              | otherwise = defaultToEnum succ' bottom' (n - one) >>= succ'

-- | Runs in `O(n)` where `n` is `fromEnum a`
-- |
-- | ```defaultFromEnum pred = fromEnum```
defaultFromEnum :: forall a. (a -> Maybe a) -> (a -> Int)
defaultFromEnum pred' e = maybe zero (\prd -> defaultFromEnum pred' prd + one) (pred' e)

-- | Property: ```fromEnum a = a', fromEnum b = b' => forall e', a' <= e' <= b': Exists e: toEnum e' = Just e```
-- |
-- | Following from the propery of `intFromTo`, we are sure all elements in `intFromTo (fromEnum a) (fromEnum b)` are `Just`s.
enumFromTo :: forall a. (Enum a) => a -> a -> Array a
enumFromTo a b = (toEnum >>> fromJust) <$> intFromTo a' b'
  where a' = fromEnum a
        b' = fromEnum b

-- | `[a,b..c]`
-- |
-- | Correctness for using `fromJust` is the same as for `enumFromTo`.
enumFromThenTo :: forall a. (Enum a) => a -> a -> a -> Array a
enumFromThenTo a b c = (toEnum >>> fromJust) <$> intStepFromTo (b' - a') a' c'
  where a' = fromEnum a
        b' = fromEnum b
        c' = fromEnum c

-- | Property: ```forall e in intFromTo a b: a <= e <= b```
intFromTo :: Int -> Int -> Array Int
intFromTo = intStepFromTo one

-- | Property: ```forall e in intStepFromTo step a b: a <= e <= b```
intStepFromTo :: Int -> Int -> Int -> Array Int
intStepFromTo step from to =
  unfoldr (\e ->
            if e <= to
            then Just $ Tuple e (e + step)  -- Output the value e, set the next state to (e + step)
            else Nothing                    -- End of the collection.
          ) from

-- | ## Instances

instance enumChar :: Enum Char where
  cardinality = Cardinality 65536
  succ = defaultSucc charToEnum charFromEnum
  pred = defaultPred charToEnum charFromEnum
  toEnum = charToEnum
  fromEnum = charFromEnum

-- | To avoid a compiler bug - can't pass self-class functions, workaround: need to make a concrete function.
charToEnum :: Int -> Maybe Char
charToEnum n | n >= 0 && n <= 65535 = Just $ fromCharCode n
charToEnum _ = Nothing

charFromEnum :: Char -> Int
charFromEnum = toCharCode

instance enumMaybe :: (Enum a) => Enum (Maybe a) where
  cardinality = maybeCardinality cardinality
  succ Nothing = Just $ bottom
  succ (Just a) = Just <$> succ a
  pred Nothing = Nothing
  pred (Just a) = Just <$> pred a
  toEnum = maybeToEnum cardinality
  fromEnum Nothing = zero
  fromEnum (Just e) = fromEnum e + one

maybeToEnum :: forall a. (Enum a) => Cardinality a -> Int -> Maybe (Maybe a)
maybeToEnum carda n | n <= runCardinality (maybeCardinality carda) =
  if n == zero
  then Just $ Nothing
  else Just $ toEnum (n - one)
maybeToEnum _    _ = Nothing

maybeCardinality :: forall a. (Enum a) => Cardinality a -> Cardinality (Maybe a)
maybeCardinality c = Cardinality $ one + (runCardinality c)

instance enumBoolean :: Enum Boolean where
  cardinality = Cardinality 2
  succ = booleanSucc
  pred = booleanPred
  toEnum = defaultToEnum booleanSucc bottom
  fromEnum = defaultFromEnum booleanPred

booleanSucc :: Boolean -> Maybe Boolean
booleanSucc false = Just true
booleanSucc _     = Nothing

booleanPred :: Boolean -> Maybe Boolean
booleanPred true  = Just false
booleanPred _     = Nothing

instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b) where
  cardinality = tupleCardinality cardinality cardinality
  succ (Tuple a b) = maybe (flip Tuple bottom <$> succ a) (Just <<< Tuple a) (succ b)
  pred (Tuple a b) = maybe (flip Tuple bottom <$> pred a) (Just <<< Tuple a) (pred b)
  toEnum = tupleToEnum cardinality
  fromEnum = tupleFromEnum cardinality

-- | All of these are as a workaround for `ScopedTypeVariables`. (not yet supported in Purescript)
tupleToEnum :: forall a b. (Enum a, Enum b) => Cardinality b -> Int -> Maybe (Tuple a b)
tupleToEnum cardb n = Tuple <$> (toEnum (n / (runCardinality cardb))) <*> (toEnum (n `mod` (runCardinality cardb)))

tupleFromEnum :: forall a b. (Enum a, Enum b) => Cardinality b -> Tuple a b -> Int
tupleFromEnum cardb (Tuple a b) = (fromEnum a) * runCardinality cardb + fromEnum b

tupleCardinality :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Cardinality (Tuple a b)
tupleCardinality l r = Cardinality $ (runCardinality l) * (runCardinality r)

instance enumEither :: (Enum a, Enum b) => Enum (Either a b) where
  cardinality = eitherCardinality cardinality cardinality
  succ (Left a) = maybe (Just $ Right bottom) (Just <<< Left) (succ a)
  succ (Right b) = maybe (Nothing) (Just <<< Right) (succ b)
  pred (Left a) = maybe (Nothing) (Just <<< Left) (pred a)
  pred (Right b) = maybe (Just $ Left top) (Just <<< Right) (pred b)
  toEnum = eitherToEnum cardinality cardinality
  fromEnum = eitherFromEnum cardinality

eitherToEnum :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Int -> Maybe (Either a b)
eitherToEnum carda cardb n =
      if n >= zero && n < runCardinality carda
      then Left <$> toEnum n
      else if n >= (runCardinality carda) && n < runCardinality (eitherCardinality carda cardb)
           then Right <$> toEnum (n - runCardinality carda)
           else Nothing

eitherFromEnum :: forall a b. (Enum a, Enum b) => Cardinality a -> (Either a b -> Int)
eitherFromEnum carda (Left a) = fromEnum a
eitherFromEnum carda (Right b) = fromEnum b + runCardinality carda

eitherCardinality :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Cardinality (Either a b)
eitherCardinality l r = Cardinality $ (runCardinality l) + (runCardinality r)
