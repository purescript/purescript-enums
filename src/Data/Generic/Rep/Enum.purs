module Data.Generic.Rep.Enum
  ( class GenericEnum
  , genericSucc
  , genericPred
  , succ'
  , pred'
  ) where

import Data.Generic.Rep
import Data.Maybe (Maybe(..), maybe)
import Data.Generic.Rep.Bounded (class GenericBottom, genericBottom',
                                 class GenericTop, genericTop')
import Prelude ((<$>))

class GenericEnum a where
  succ' :: a -> Maybe a
  pred' :: a -> Maybe a

instance genericEnumNoConstructors :: GenericEnum NoConstructors where
  succ' _ = Nothing
  pred' _ = Nothing

instance genericEnumConstructor :: GenericEnum (Constructor n NoArguments) where
  succ' _ = Nothing
  pred' _ = Nothing

instance genericEnumSum ::
  (GenericEnum a, GenericTop a, GenericEnum b, GenericBottom b)
  => GenericEnum (Sum a b) where
  succ' (Inl x) = Just (maybe (Inr genericBottom') Inl (succ' x))
  succ' (Inr x) = Inr <$> succ' x

  pred' (Inl x) = Inl <$> pred' x
  pred' (Inr x) = Just (maybe (Inl genericTop') Inr (pred' x))

genericSucc :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericSucc x = to <$> succ' (from x)

genericPred :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericPred x = to <$> pred' (from x)
