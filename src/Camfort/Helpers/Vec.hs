{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Camfort.Helpers.Vec where

import Prelude hiding (length, zipWith, take, drop)

import Data.Proxy

data Nat = Z | S Nat

-- Indexed natural number type
data Natural (n :: Nat) where
     Zero :: Natural Z
     Succ :: Natural n -> Natural (S n)

deriving instance Show (Natural n)

data NatBox where NatBox :: Natural n -> NatBox
deriving instance Show NatBox

-- Conversions to and from the type-representation
-- of natural numbers
toNatBox :: Int -> NatBox
toNatBox 0 = NatBox Zero
toNatBox n = case toNatBox (n-1) of
              (NatBox n) -> NatBox (Succ n)

class IsNatural (n :: Nat) where
   fromNat :: Proxy n -> Int

instance IsNatural Z where
   fromNat Proxy = 0
instance IsNatural n => IsNatural (S n) where
   fromNat Proxy = 1 + fromNat (Proxy :: Proxy n)

-- Indexed vector type
data Vec (n :: Nat) a where
     Nil :: Vec Z a
     Cons :: a -> Vec n a -> Vec (S n) a

length :: Vec n a -> Int
length Nil = 0
length (Cons x xs) = 1 + length xs

instance Functor (Vec n) where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

deriving instance Eq a => Eq (Vec n a)

instance Ord a => Ord (Vec n a) where
    Nil         <= _ = True
    (Cons x xs) <= (Cons y ys) | xs == ys = x <= y
                               | otherwise = xs <= ys
instance Show a => Show (Vec n a) where
  show xs = "<" ++ showV xs ++ ">"
    where
      showV :: forall n a . Show a => Vec n a -> String
      showV Nil          = ""
      showV (Cons x Nil) = show x
      showV (Cons x xs)  = show x ++ "," ++ showV xs

instance Foldable (Vec n) where
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = foldr f (f x acc) xs

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f Nil Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

-- Equality type
data EqT a b where
  ReflEq :: EqT a a

-- Lists existentially quanitify over a vector's size : Exists n . Vec n a
data VecBox a where
     VecBox :: Vec n a -> VecBox a

fromList :: [a] -> VecBox a
fromList = foldr (\x (VecBox xs) -> VecBox (Cons x xs)) (VecBox Nil)

toList :: Vec n a -> [ a ]
toList Nil = [ ]
toList (Cons x xs) = x : toList xs

-- | Apply length preserving list operation.
applyListOp :: ([ a ] -> [ a ]) -> Vec n a -> Vec n a
applyListOp f v =
  case fromList . f . toList $ v of
    VecBox v' ->
      case proveEqSize v v' of
        Just ReflEq -> v'
        Nothing -> error "List operation was not length preserving."

proveEqSize :: Vec n a -> Vec m b -> Maybe (EqT m n)
proveEqSize Nil Nil = return ReflEq
proveEqSize (Cons _ xs) (Cons _ ys) = do
  ReflEq <- proveEqSize xs ys
  return ReflEq
proveEqSize _ _ = Nothing

hasSize :: Vec m a -> Natural n -> Maybe (EqT m n)
hasSize Nil Zero = return ReflEq
hasSize (Cons _ xs) (Succ n) = do
  ReflEq <- xs `hasSize` n
  return ReflEq
hasSize _ _ = Nothing
