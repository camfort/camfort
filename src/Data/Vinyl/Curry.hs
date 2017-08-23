
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Vinyl.Curry where

import           Data.Vinyl
import           Data.Vinyl.Functor


class RecordCurry ts where
  type CurriedF (f :: * -> *) ts a
  type Curried ts a

  rcurry :: (Rec f ts -> a) -> CurriedF f ts a
  rcurry' :: (Rec Identity ts -> a) -> Curried ts a

  runcurry :: CurriedF f ts a -> Rec f ts -> a
  runcurryA' :: (Applicative f) => f (Curried ts a) -> Rec f ts -> f a

  runcurryA :: (Applicative f) => Curried ts a -> Rec f ts -> f a
  runcurryA = runcurryA' . pure


instance RecordCurry '[] where
  type CurriedF f '[] a = a
  type Curried '[] a = a

  rcurry f = f RNil

  rcurry' f = f RNil

  runcurry x RNil = x

  runcurryA' x RNil = x


instance RecordCurry ts => RecordCurry (t : ts) where
  type CurriedF f (t : ts) a = f t -> CurriedF f ts a
  type Curried (t : ts) a = t -> Curried ts a

  rcurry f x = rcurry (\xs -> f (x :& xs))

  rcurry' f x = rcurry' (\xs -> f (Identity x :& xs))

  runcurry f (x :& xs) = runcurry (f x) xs

  runcurryA' f (x :& xs) =
    let f' = f <*> x
    in runcurryA' f' xs
