module Data.Generic.Rep.Enum
  ( genericSucc
  , class GenericSucc
  , succ'
  , construct

  , genericPred
  , class GenericPred
  , pred'

  , genericFromEnum
  , class GenericFromEnum
  , fromEnum'

  , genericToEnum
  , class GenericToEnum
  , toEnum'

  , genericCardinality
  , class GenericCardinality
  , cardinality'
  ) where

import Data.Generic.Rep
import Data.Maybe (Maybe(..))
import Data.Enum (Cardinality(Cardinality))
import Data.Newtype (unwrap)
import Prelude ((<$>), (+), (-))

class GenericSucc a where
  succ' :: a -> Maybe a
  construct :: a

instance genericSuccConstructor :: GenericSucc (Constructor n NoArguments) where
succ' _ = Nothing
construct = Constructor NoArguments

instance genericSuccSum ::
  GenericSucc a =>
    GenericSucc (
      Sum
        (Constructor n NoArguments)
        a
    ) where
succ' (Inl _) = Just (Inr construct)
succ' (Inr x) = Inr <$> succ' x
construct = Inl (Constructor NoArguments)

class GenericPred a where
  pred' :: a -> Maybe a

instance genericPredConstructor :: GenericPred (Constructor n NoArguments) where
pred' _ = Nothing

instance genericPredSum ::
  GenericPred a =>
    GenericPred (
      Sum
        (Constructor n NoArguments)
        a
    ) where
pred' (Inl _) = Nothing
pred' (Inr x) = case pred' x of
    Nothing   -> Just (Inl (Constructor NoArguments))
    Just pred -> Just (Inr pred)

class GenericFromEnum a where
  fromEnum' :: a -> Int

instance genericFromEnumConstructor ::
  GenericFromEnum (Constructor n NoArguments) where
  fromEnum' _ = 0

instance genericFromEnumSum ::
  GenericFromEnum a
    => GenericFromEnum (
      Sum
        (Constructor n1 NoArguments)
        a
    ) where
  fromEnum' (Inl _) = 0
  fromEnum' (Inr x) = 1 + (fromEnum' x)

class GenericToEnum a where
  toEnum' :: Int -> Maybe a

instance genericToEnumConstructor ::
  GenericToEnum (Constructor n NoArguments) where
  toEnum' 0 = Just (Constructor NoArguments)
  toEnum' _ = Nothing

instance genericToEnumSum ::
  GenericToEnum a
    => GenericToEnum (
      Sum
        (Constructor n1 NoArguments)
        a
    ) where
  toEnum' 0 = Just (Inl (Constructor NoArguments))
  toEnum' x = Inr <$> toEnum' (x - 1)

class GenericCardinality a where
  cardinality' :: Cardinality a

instance genericCardinalityConstructor ::
  GenericCardinality (Constructor name NoArguments) where
  cardinality' = Cardinality 1

instance genericCardinalitySum ::
  GenericCardinality a =>
    GenericCardinality (Sum (Constructor name NoArguments) a) where
  cardinality' = Cardinality (1 + (unwrap (cardinality' :: Cardinality a)))

genericSucc :: forall a rep. (Generic a rep, GenericSucc rep)
  => a -> Maybe a
genericSucc x = to <$> succ' (from x)

genericPred :: forall a rep. (Generic a rep, GenericPred rep)
  => a -> Maybe a
genericPred x = to <$> pred' (from x)

genericFromEnum :: forall a rep. (Generic a rep, GenericFromEnum rep)
  => a -> Int
genericFromEnum x = fromEnum' (from x)

genericToEnum :: forall a rep. (Generic a rep, GenericToEnum rep)
  => Int -> Maybe a
genericToEnum x = to <$> toEnum' x

genericCardinality :: forall a rep. (Generic a rep, GenericCardinality rep)
  => Cardinality a
genericCardinality = Cardinality (unwrap (cardinality' :: Cardinality rep))
