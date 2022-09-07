module Hw07.JoinList where

import Data.Monoid
import Hw07.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i >= 0 && i < leftSize         = indexJ i jl1
  | i >= leftSize && i < totalSize = indexJ (i - leftSize) jl2
  | otherwise                      = Nothing
  where leftSize = getSize . size . tag $ jl1
        totalSize = getSize . size $ m

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n <= 0 = jl
  | otherwise = Empty
dropJ n jl@(Append m jl1 jl2)
  | n <= 0                         = jl
  | n > 0 && n < leftSize          = jl1' +++ jl2
  | n >= leftSize && n < totalSize = dropJ (n - leftSize) jl2
  | otherwise                      = Empty
  where jl1' = dropJ n jl1
        leftSize = getSize . size . tag $ jl1
        totalSize = getSize . size $ m

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ a)
  | n <= 0    = Empty
  | otherwise = jl
takeJ n jl@(Append m jl1 jl2)
  | n <= 0 = Empty
  | n > 0 && n <= leftSize = takeJ n jl1
  | n > leftSize && n < totalSize = jl1 +++ jl2'
  | otherwise = jl
  where jl2' = takeJ (n-leftSize) jl2
        leftSize = getSize . size . tag $ jl1
        totalSize = getSize . size $ m


-- To test the above functions
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

a = Append (Size 3)
      (Append (Size 1)
        Empty
        (Single (Size 1) "b")
      )
      (Append (Size 2)
        (Single (Size 1) "c")
        (Single (Size 1) "d")
      )
      