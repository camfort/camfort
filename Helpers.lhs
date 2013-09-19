> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE RankNTypes #-}

 {-# LANGUAGE MultiParamTypeClasses #-}

 {-# LANGUAGE OverlappingInstances #-}

 {-# LANGUAGE ImplicitParams #-}
 {-# LANGUAGE KindSignatures #-}

> module Helpers where

> import Language.Fortran
> import Analysis.Annotations

> import Generics.Deriving.Base
> import Generics.Deriving.Copoint
> import GHC.Generics


> import Data.Generics.Zipper
> import Data.Generics.Aliases
> import Data.Generics.Str
> import Data.Generics.Uniplate.Operations

> import Language.Fortran.Lexer

> import Control.Comonad

> import Data.Data
> import Data.Maybe
> import Data.Monoid     

> import Debug.Trace

> instance Monoid x => Monad ((,) x) where
>     return a = (mempty, a)
>     (x, a) >>= k = let (x', b) = k a
>                    in (mappend x x', b)

Other helpers

> fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
> fanout f g x = (f x, g x)

> prod :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> prod f g (x, y) = (f x, g y)

> (><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> f >< g = prod f g

> mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
> mfmap f = map (fmap f)

> each = flip (map)