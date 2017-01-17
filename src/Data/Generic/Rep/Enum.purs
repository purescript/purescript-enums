module Data.Generic.Rep.Enum
  ( class GenericEnum
  , genericSucc
  , genericPred
  , succ'
  , pred'
  ) where

import Data.Generic.Rep
import Data.Maybe (Maybe(..))
import Data.Generic.Rep.Eq (class GenericEq, genericEq')
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

instance genericEnumSum :: (
  GenericEnum a, GenericEq a, GenericTop a,
  GenericEnum b, GenericEq b, GenericBottom b
) => GenericEnum (Sum a b) where
  succ' (Inl x) = if genericEq' x genericTop'
                    then Just (Inr genericBottom')
                    else Inl <$> succ' x
  succ' (Inr x) = Inr <$> succ' x

  pred' (Inl x) = Inl <$> pred' x
  pred' (Inr x) = if genericEq' x genericBottom'
                   then Just (Inl genericTop')
                   else Inr <$> pred' x

genericSucc :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericSucc x = to <$> succ' (from x)

genericPred :: forall a rep. (Generic a rep, GenericEnum rep)
  => a -> Maybe a
genericPred x = to <$> pred' (from x)
