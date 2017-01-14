module Data.Generic.Rep.Enum
  ( class GenericEnum
  , genericSucc
  , genericPred
  , genericFromEnum
  , genericToEnum
  , genericCardinality
  , succ'
  , pred'
  , construct
  , fromEnum'
  , toEnum'
  , cardinality'
  ) where

import Data.Generic.Rep
import Data.Maybe (Maybe(..))
import Data.Enum (Cardinality(Cardinality))
import Data.Newtype (unwrap)
import Prelude ((<$>), (+), (-))

class GenericEnum a where
  succ' :: a -> Maybe a
  pred' :: a -> Maybe a
  fromEnum' :: a -> Int
  toEnum' :: Int -> Maybe a
  cardinality' :: Cardinality a
  construct :: a

instance genericEnumConstructor :: GenericEnum (Constructor n NoArguments) where
  succ' _ = Nothing
  pred' _ = Nothing
  fromEnum' _ = 0
  toEnum' 0 = Just (Constructor NoArguments)
  toEnum' _ = Nothing
  cardinality' = Cardinality 1
  construct = Constructor NoArguments

instance genericEnumSum ::
  (GenericEnum a, GenericEnum b) => GenericEnum (Sum a b) where
  succ' (Inl _) = Just (Inr construct)
  succ' (Inr x) = Inr <$> succ' x
  pred' (Inl _) = Nothing
  pred' (Inr x) = case pred' x of
      Nothing   -> Just (Inl construct)
      Just pred -> Just (Inr pred)
  construct = Inl construct
  fromEnum' (Inl _) = 0
  fromEnum' (Inr x) = 1 + (fromEnum' x)
  toEnum' 0 = Just construct
  toEnum' x = Inr <$> toEnum' (x - 1)
  cardinality' = Cardinality (1 + (unwrap (cardinality' :: Cardinality b)))

genericSucc :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericSucc x = to <$> succ' (from x)

genericPred :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericPred x = to <$> pred' (from x)

genericFromEnum :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Int
genericFromEnum x = fromEnum' (from x)

genericToEnum :: forall a rep. (Generic a rep, GenericEnum rep)
  => Int -> Maybe a
genericToEnum x = to <$> toEnum' x

genericCardinality :: forall a rep. (Generic a rep, GenericEnum rep)
  => Cardinality a
genericCardinality = Cardinality (unwrap (cardinality' :: Cardinality rep))
