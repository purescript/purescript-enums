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
  , defaultSucc
  , defaultPred
  , defaultToEnum
  , defaultFromEnum
  ) where

  import Data.Maybe
  import Data.Either
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
  -- |   succ firstEnum >>= succ >>= succ ... succ [cardinality - 1 times] == lastEnum
  -- |   pred lastEnum  >>= pred >>= pred ... pred [cardinality - 1 times] == firstEnum
  -- |   
  -- |   e1 `compare` e2 == fromEnum e1 `compare` fromEnum e2
  -- |   
  -- |   for all a > firstEnum: pred a >>= succ == Just a
  -- |   for all a < lastEnum:  succ a >>= pred == Just a
  -- |   
  -- |   pred >=> succ >=> pred = pred
  -- |   succ >=> pred >=> succ = succ
  -- |   
  -- |   toEnum (fromEnum a) = Just a
  -- |   
  -- |   for all a > firstEnum: fromEnum <$> pred a = Just (fromEnum a - 1)
  -- |   for all a < lastEnum:  fromEnum <$> succ a = Just (fromEnum a + 1)


  class (Ord a) <= Enum a where
    cardinality :: Cardinality a

    firstEnum :: a

    lastEnum :: a

    succ :: a -> Maybe a

    pred :: a -> Maybe a

    toEnum :: Number -> Maybe a
    
    fromEnum :: a -> Number

  -- | defaultSucc toEnum fromEnum = succ
  defaultSucc :: forall a. (Number -> Maybe a) -> (a -> Number) -> (a -> Maybe a)
  defaultSucc toEnum' fromEnum' a = toEnum' (fromEnum' a + 1)

  -- | defaultPred toEnum fromEnum = pred
  defaultPred :: forall a. (Number -> Maybe a) -> (a -> Number) -> (a -> Maybe a)
  defaultPred toEnum' fromEnum' a = toEnum' (fromEnum' a - 1)

  -- | Runs in O(n) where n is (fromEnum a)
  -- | defaultToEnum succ firstEnum = toEnum
  defaultToEnum :: forall a. (a -> Maybe a) -> a -> (Number -> Maybe a)
  defaultToEnum succ' firstEnum' n | n < 0 = Nothing
  defaultToEnum succ' firstEnum' 0         = Just firstEnum'
  defaultToEnum succ' firstEnum' n         = defaultToEnum succ' firstEnum' (n - 1) >>= succ'

  -- | Runs in O(n) where n is (fromEnum a)
  -- | defaultFromEnum pred = fromEnum
  defaultFromEnum :: forall a. (a -> Maybe a) -> (a -> Number)
  defaultFromEnum pred' e = maybe 0 (\prd -> defaultFromEnum pred' prd + 1) (pred' e)

  instance enumChar :: Enum Char where
    cardinality = Cardinality (65535 + 1)

    firstEnum = fromCharCode 0

    lastEnum = fromCharCode 65535

    succ = defaultSucc charToEnum charFromEnum

    pred = defaultPred charToEnum charFromEnum

    toEnum = charToEnum

    fromEnum = charFromEnum

  -- To avoid a compiler bug - can't pass self-class functions, workaround: need to make a concrete function.
  charToEnum :: Number -> Maybe Char
  charToEnum n | n >= 0 && n <= 65535 = Just $ fromCharCode n
  charToEnum _ = Nothing

  charFromEnum :: Char -> Number
  charFromEnum = toCharCode

  instance enumMaybe :: (Enum a) => Enum (Maybe a) where
    cardinality = maybeCardinality cardinality

    firstEnum = Nothing

    lastEnum = Just $ lastEnum

    succ Nothing = Just $ firstEnum
    succ (Just a) = Just <$> succ a

    pred Nothing = Nothing
    pred (Just a) = Just <$> pred a

    toEnum = maybeToEnum cardinality

    fromEnum Nothing = 0
    fromEnum (Just e) = fromEnum e + 1
  
  maybeToEnum :: forall a. (Enum a) => Cardinality a -> Number -> Maybe (Maybe a)
  maybeToEnum carda n | n <= runCardinality (maybeCardinality carda) = 
    if n == 0 
    then Just $ Nothing
    else Just $ toEnum (n - 1)
  maybeToEnum _    _ = Nothing

  maybeCardinality :: forall a. (Enum a) => Cardinality a -> Cardinality (Maybe a)
  maybeCardinality c = Cardinality $ 1 + (runCardinality c)

  instance enumBoolean :: Enum Boolean where
    cardinality = Cardinality 2
   
    firstEnum = booleanFirstEnum

    lastEnum = true

    succ = booleanSucc

    pred = booleanPred

    toEnum = defaultToEnum booleanSucc booleanFirstEnum

    fromEnum = defaultFromEnum booleanPred

  booleanFirstEnum :: Boolean
  booleanFirstEnum = false

  booleanSucc :: Boolean -> Maybe Boolean
  booleanSucc false = Just true
  booleanSucc _     = Nothing

  booleanPred :: Boolean -> Maybe Boolean
  booleanPred true  = Just false
  booleanPred _     = Nothing

  -- Until we get Int, floor and div in the prelude
  foreign import floor "function floor(n){ return Math.floor(n); }" :: Number -> Number
  div a b = floor (a / b)

  instance enumTuple :: (Enum a, Enum b) => Enum (Tuple a b) where
    cardinality = tupleCardinality cardinality cardinality

    firstEnum = Tuple firstEnum firstEnum

    lastEnum = Tuple lastEnum lastEnum

    succ (Tuple a b) = maybe (flip Tuple firstEnum <$> succ a) (Just <<< Tuple a) (succ b)

    pred (Tuple a b) = maybe (flip Tuple firstEnum <$> pred a) (Just <<< Tuple a) (pred b)

    toEnum = tupleToEnum cardinality

    fromEnum = tupleFromEnum cardinality

  -- All of these are as a workaround for ScopedTypeVariables. (not yet supported in Purescript)
  tupleToEnum :: forall a b. (Enum a, Enum b) => Cardinality b -> Number -> Maybe (Tuple a b)
  tupleToEnum cardb n = Tuple <$> (toEnum (n `div` (runCardinality cardb))) <*> (toEnum (n % (runCardinality cardb)))

  tupleFromEnum :: forall a b. (Enum a, Enum b) => Cardinality b -> Tuple a b -> Number
  tupleFromEnum cardb (Tuple a b) = (fromEnum a) * runCardinality cardb + fromEnum b

  tupleCardinality :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Cardinality (Tuple a b)
  tupleCardinality l r = Cardinality $ (runCardinality l) * (runCardinality r)

  instance enumEither :: (Enum a, Enum b) => Enum (Either a b) where
    cardinality = eitherCardinality cardinality cardinality

    firstEnum = Left firstEnum

    lastEnum = Right lastEnum

    succ (Left a) = maybe (Just $ Right firstEnum) (Just <<< Left) (succ a)
    succ (Right b) = maybe (Nothing) (Just <<< Right) (succ b)

    pred (Left a) = maybe (Nothing) (Just <<< Left) (pred a)
    pred (Right b) = maybe (Just $ Left lastEnum) (Just <<< Right) (pred b)

    toEnum = eitherToEnum cardinality cardinality

    fromEnum = eitherFromEnum cardinality

  eitherToEnum :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Number -> Maybe (Either a b)
  eitherToEnum carda cardb n = 
        if n >= 0 && n < runCardinality carda
        then Left <$> toEnum n
        else if n >= (runCardinality carda) && n < runCardinality (eitherCardinality carda cardb)
             then Right <$> toEnum (n - runCardinality carda)
             else Nothing

  eitherFromEnum :: forall a b. (Enum a, Enum b) => Cardinality a -> (Either a b -> Number)
  eitherFromEnum carda (Left a) = fromEnum a
  eitherFromEnum carda (Right b) = fromEnum b + runCardinality carda
            
  eitherCardinality :: forall a b. (Enum a, Enum b) => Cardinality a -> Cardinality b -> Cardinality (Either a b)
  eitherCardinality l r = Cardinality $ (runCardinality l) + (runCardinality r)