{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving, RankNTypes #-}

module Helpers.Vec where

data Nat = Z | S Nat

-- Indexed natural number type
data Natural (n :: Nat) where
     Zero :: Natural Z
     Succ :: Natural n -> Natural (S n)
     
-- Indexed vector type
data Vec (n :: Nat) a where
     Nil :: Vec Z a
     Cons :: a -> Vec n a -> Vec (S n) a

-- Lists existentially quanitify over a vector's size : Exists n . Vec n a 
data List a where
     List :: Vec n a -> List a
     
lnil :: List a
lnil = List Nil
lcons :: a -> List a -> List a
lcons x (List xs) = List (Cons x xs)

fromList :: [a] -> List a
fromList []       = lnil
fromList (x : xs) = lcons x (fromList xs)

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f Nil         = Nil
vmap f (Cons x xs) = Cons (f x) (vmap f xs)

instance Functor (Vec n) where
    fmap = vmap
deriving instance Eq a => Eq (Vec n a)
deriving instance Show a => Show (Vec n a)
instance Ord a => Ord (Vec n a) where
       (Cons x xs) <= (Cons y ys) | xs == ys = x <= y
                                  | otherwise = xs <= ys

