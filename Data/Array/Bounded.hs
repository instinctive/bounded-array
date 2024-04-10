{-# LANGUAGE TypeFamilies #-}

module Data.Array.Bounded
    ( BoundedArray
    , (!)
    , (//)
    , listArray
    , listArrayFill
    , elt
    ) where

import Control.Lens
import GHC.Arr        ( unsafeArray, unsafeAt, unsafeIndex, unsafeReplace )
import Data.Array     ( Array, Ix, inRange, range )
import Data.Bifunctor ( first )
import Data.Functor   ( (<&>) )
import qualified Data.Array as A

newtype BoundedIndex i = BoundedIndex i
    deriving ( Eq, Ord, Bounded, Enum, Show )

instance ( Enum i, Ord i ) => Ix (BoundedIndex i) where
    range (a, b)                   = [a..b]
    inRange _ i                    = True
    unsafeIndex _ (BoundedIndex i) = fromEnum i

newtype BoundedArray i e = BoundedArray (Array (BoundedIndex i) e)
    deriving ( Eq, Ord, Show, Functor, Foldable, Traversable )

type instance Index (BoundedArray i e) = i
type instance IxValue (BoundedArray i e) = e

instance ( Enum i, Ord i) => Ixed (BoundedArray i e) where
    ix i f a = f (a!i) <&> \e -> a // [(i,e)]

(!) :: ( Enum i, Ord i ) => BoundedArray i e -> i -> e
BoundedArray a ! i = a `unsafeAt` fromEnum i

(//) :: ( Enum i, Ord i ) => BoundedArray i e -> [(i, e)] -> BoundedArray i e
BoundedArray a // ies =
    BoundedArray . unsafeReplace a $ fmap (first fromEnum) ies

listArray :: ( Bounded i, Enum i, Ord i ) => [e] -> BoundedArray i e
listArray = listArrayFill undefined

listArrayFill :: ( Bounded i, Enum i, Ord i ) => e -> [e] -> BoundedArray i e
listArrayFill e = BoundedArray . A.listArray ( minBound, maxBound ) . (++ repeat e)

elt :: ( Enum i, Ord i ) => Functor f => i -> (a -> f a) -> BoundedArray i a -> f (BoundedArray i a)
elt i f a = f (a!i) <&> \e -> a // [(i,e)]
