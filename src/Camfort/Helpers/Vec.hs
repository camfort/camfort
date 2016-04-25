{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving, RankNTypes, TypeFamilies #-}

module Camfort.Helpers.Vec where


data Nat = Z | S Nat

-- Indexed natural number type
data Natural (n :: Nat) where
     Zero :: Natural Z
     Succ :: Natural n -> Natural (S n)

deriving instance Show (Natural n)

data NatBox where NatBox :: Natural n -> NatBox
deriving instance Show NatBox

toNatBox :: Int -> NatBox
toNatBox 0 = NatBox Zero
toNatBox n = case toNatBox (n-1) of
              (NatBox n) -> NatBox (Succ n)


     
-- Indexed vector type
data Vec (n :: Nat) a where
     Nil :: Vec Z a
     Cons :: a -> Vec n a -> Vec (S n) a

lengthV :: Vec n a -> Int
lengthV Nil = 0
lengthV (Cons x xs) = 1 + lengthV xs

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f Nil         = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

instance Functor (Vec n) where
    fmap = vmap
deriving instance Eq a => Eq (Vec n a)
instance Ord a => Ord (Vec n a) where
    (Cons x xs) <= (Cons y ys) | xs == ys = x <= y
                               | otherwise = xs <= ys
instance Show a => Show (Vec n a) where
    show = showV

showV :: Show a => Vec n a -> String
showV xs = "<" ++ showV' xs ++ ">"
  where
    showV' :: Show a => Vec n a -> String
    showV' Nil          = ""
    showV' (Cons x Nil) = show x
    showV' (Cons x xs)  = show x ++ "," ++ showV' xs




type family Max (n :: Nat) (m :: Nat) :: Nat where
            Max Z Z = Z
            Max Z m = m
            Max m Z = m
            Max (S n) (S m) = S (Max n m)

zipVec :: Vec m Int -> Vec n Int -> (Vec (Max n m) Int, Vec (Max n m) Int)
zipVec Nil Nil = (Nil, Nil)
zipVec Nil xs  = (fmap (const 0) xs, xs)
zipVec xs Nil  = (xs, fmap (const 0) xs)
zipVec (Cons x xs) (Cons y ys)
               = (Cons x xs', Cons y ys') where (xs', ys') = zipVec xs ys


