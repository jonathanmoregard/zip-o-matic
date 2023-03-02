{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Data.List.Zipper
  ( ListZipper,
    (|:),
    zmap,
    ix,
    ixzmap,
    toList,
    reverse,
    next,
    singleton,
    previous,
    current,
    currentIndex,
    replaceCurrent,
    tug,
    forward,
    forwardWhile,
    forwardUntil,
    backward,
    backwardWhile,
    backwardUntil,
  )
where

import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)

infixl 4 |:

-- | Construct a ListZipper from a list and a NonEmpty. Example: ["the","list"] |: "focus" :| ["nonempty","tail"]. Don't send in an infinite list.
(|:) :: [a] -> NonEmpty a -> ListZipper a
as1 |: (h :| as2) = ListZipper (reverse as1) h as2

singleton :: a -> ListZipper a
singleton a = ListZipper [] a []

data ListZipper a = ListZipper ![a] !a ![a]
  deriving
    ( Functor,
      Show,
      Eq,
      Generic
    )

instance Foldable ListZipper where
  foldMap :: Monoid m => (a -> m) -> ListZipper a -> m
  foldMap f (ListZipper ls x rs) = foldMap f (reverse ls) `mappend` f x `mappend` foldMap f rs

instance Traversable ListZipper where
  traverse :: Applicative f => (a -> f b) -> ListZipper a -> f (ListZipper b)
  traverse f (ListZipper ls x rs) = ListZipper <$> (reverse <$> traverse f (reverse ls)) <*> f x <*> traverse f rs

zmap :: (Bool -> a -> b) -> ListZipper a -> ListZipper b
zmap f (ListZipper as1 a as2) = ListZipper (f False <$> as1) (f True a) (f False <$> as2)

ix :: ListZipper a -> ListZipper (Int, a)
ix (ListZipper as1 a as2) =
  let (nextIx, ixAs1) = foldr ixAlg (0, []) as1
   in ListZipper ixAs1 (nextIx, a) $ zip [nextIx + 1 ..] as2
  where
    ixAlg :: a -> (Int, [(Int, a)]) -> (Int, [(Int, a)])
    ixAlg a (!nextIndex, !prev) = (nextIndex + 1, (nextIndex, a) : prev)

ixzmap :: (Bool -> Int -> a -> b) -> ListZipper a -> ListZipper b
ixzmap f = zmap (uncurry . f) . ix

toList :: ListZipper a -> [a]
toList (ListZipper as1 a as2) = reverse as1 <> (a : as2)

zreverse :: ListZipper a -> ListZipper a
zreverse (ListZipper as1 a as2) = ListZipper (reverse as2) a (reverse as1)

next :: ListZipper a -> Maybe a
next (ListZipper _ _ (a' : _)) = Just a'
next _ = Nothing

previous :: ListZipper a -> Maybe a
previous (ListZipper (a' : _) _ _) = Just a'
previous _ = Nothing

current :: ListZipper a -> a
current (ListZipper _ a _) = a

currentIndex :: ListZipper x -> Int
currentIndex = fst . current . ix

replaceCurrent :: a -> ListZipper a -> ListZipper a
replaceCurrent a (ListZipper as1 _ as2) = ListZipper as1 a as2

tug :: (a -> Maybe a) -> a -> a
tug f a = Maybe.fromMaybe a $ f a

forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper as1 a (a' : as2)) = Just $ ListZipper (a : as1) a' as2
forward _ = Nothing

--TODO: Stupid impl, rework after quickspec
forwardWhile :: (a -> Bool) -> ListZipper a -> ListZipper a
forwardWhile cond lz =
  if maybe False cond (next lz)
    then forwardWhile cond (tug forward lz)
    else lz

--TODO: Stupid impl, rework after quickspec
forwardUntil :: (a -> Bool) -> ListZipper a -> Maybe (ListZipper a)
forwardUntil cond lz = do
  next <- next lz
  if cond next
    then forward lz
    else forwardUntil cond (tug forward lz)

backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper (a' : as1) a as2) = Just $ ListZipper as1 a' (a : as2)
backward _ = Nothing

--TODO: Stupid impl, rework after quickspec
backwardWhile :: (a -> Bool) -> ListZipper a -> ListZipper a
backwardWhile cond lz =
  if maybe False cond (previous lz)
    then backwardWhile cond (tug forward lz)
    else lz

--TODO: Stupid impl, rework after quickspec
backwardUntil :: (a -> Bool) -> ListZipper a -> Maybe (ListZipper a)
backwardUntil cond lz = do
  previous <- previous lz
  if cond previous
    then backward lz
    else backwardUntil cond (tug backward lz)