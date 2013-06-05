> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}

> module Annotations where

> import Data.Data
> import Data.Generics.Uniplate.Operations

> import Data.Map.Lazy hiding (map)

> import Language.Fortran
> import Traverse

> import Language.Haskell.ParseMonad 
> import Language.Haskell.Syntax (SrcLoc,srcLine,srcColumn)

Loop classigications 

> data ReduceType = Reduce | NoReduce
> data AccessPatternType = Regular | RegularAndConstants | Irregular | Undecidable 
> data LoopType = Functor ReduceType | Gather ReduceType ReduceType AccessPatternType | Scatter ReduceType AccessPatternType

 classify :: Fortran Annotation -> Fortran Annotation
 classify x = 

> data Annotation = A {lives ::[Variable],
>                      indices :: [Variable],
>                      arrsRead :: Map Variable [[Expr ()]], 
>                      arrsWrite :: Map Variable [[Expr ()]],
>                      number :: Int}
>                    deriving (Eq, Show, Typeable, Data)

 -- Map Variable [[(Variable,Int)]],

> setNumber :: Int -> Annotation -> Annotation
> setNumber x a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) x

> setLives :: [Variable] -> Annotation -> Annotation
> setLives x a = A x (indices a) (arrsRead a) (arrsWrite a) (number a)

> setIndices :: [Variable] -> Annotation -> Annotation
> setIndices x a = A (lives a) x (arrsRead a) (arrsWrite a) (number a)

> setArrsRead :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsRead x a = A (indices a) (lives a) x (arrsWrite a) (number a)

> setArrsWrite :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsWrite x a = A (indices a) (lives a) (arrsRead a) x (number a)

> unitAnnotation = A [] [] empty empty 0

