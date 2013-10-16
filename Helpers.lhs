> {-# LANGUAGE TypeOperators #-}

> module Helpers where

> type Filename = String
> type Directory = String
> type SourceText = String

Other helpers

> fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
> fanout f g x = (f x, g x)

> (<>) :: (a -> b) -> (a -> c) -> a -> (b, c)
> f <> g = fanout f g

> (><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> f >< g = \(x, y) -> (f x, g y)

Computes all pairwise combinations

> pairs :: [a] -> [(a, a)]
> pairs []     = []
> pairs (x:xs) = (zip (repeat x) xs) ++ (pairs xs)

Functor composed with list functor

> mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
> mfmap f = map (fmap f)

An infix `map` operation.

> each = flip (map)

> cmpEq :: Ordering -> Bool
> cmpEq EQ = True
> cmpEq _  = False

Used for type-level annotations giving documentation

> type (:?) a b = a