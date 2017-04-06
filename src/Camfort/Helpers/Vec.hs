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

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Camfort.Helpers.Vec where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool

-- Indexed vector type
data Vec (n :: Nat) a where
  Nil :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

length :: forall n a . KnownNat n => Vec n a -> Int
length _ = fromInteger . natVal $ (Proxy :: Proxy n)

instance Functor (Vec n) where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Eq a => Eq (Vec n a) where
  Nil         == Nil          = True
  (Cons x xs) == (Cons y ys)  = x == y && xs == ys
  _           == _            = False

instance Ord a => Ord (Vec n a) where
    Nil         <= _ = True
    (Cons x xs) <= (Cons y ys)
      | xs == ys = x <= y
      | otherwise = xs <= ys

instance Show a => Show (Vec n a) where
  show xs = "<" ++ showV' xs ++ ">"
    where
      showV' :: forall n a . Show a => Vec n a -> String
      showV' Nil          = ""
      showV' (Cons x Nil) = show x
      showV' (Cons x xs)  = show x ++ "," ++ showV' xs

zipVec :: Vec n Int -> Vec n Int -> (Vec n Int, Vec n Int)
zipVec Nil Nil = (Nil, Nil)
zipVec (Cons x xs) (Cons y ys)
  = (Cons x xs', Cons y ys') where (xs', ys') = zipVec xs ys
