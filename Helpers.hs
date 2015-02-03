{-# LANGUAGE TypeOperators #-}

module Helpers where

type Filename = String
type Directory = String
type SourceText = String
type FileOrDir = String

-- Helpers

fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
fanout f g x = (f x, g x)

(<>) :: (a -> b) -> (a -> c) -> a -> (b, c)
f <> g = fanout f g

(><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
f >< g = \(x, y) -> (f x, g y)

-- Lookup functions over relation s

lookups :: Eq a => a -> [(a, b)] -> [b]
lookups _ [] = []
lookups x ((a, b):xs) = if (x == a) then b : lookups x xs
                                    else lookups x xs

lookups' :: Eq a => a -> [((a, b), c)] -> [(b, c)]
lookups' _ [] = []
lookups' x (((a, b), c):xs) = if (x == a) then (b, c) : lookups' x xs 
                                          else lookups' x xs

{-| Computes all pairwise combinations -}
pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = (zip (repeat x) xs) ++ (pairs xs)

{-| Functor composed with list functor -}
mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
mfmap f = map (fmap f)

{-| An infix `map` operation.-}
each = flip (map)

{-| Is the Ordering an EQ? -}
cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

cmpFst :: (a -> a -> Ordering) -> (a, b) -> (a, b) -> Ordering
cmpFst c (x1, y1) (x2, y2) = c x1 x2

cmpSnd :: (b -> b -> Ordering) -> (a, b) -> (a, b) -> Ordering
cmpSnd c (x1, y1) (x2, y2) = c y1 y2

{-| used for type-level annotations giving documentation -}
type (:?) a b = a
