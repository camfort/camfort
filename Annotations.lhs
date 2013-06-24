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
>                      number :: Int,
>                      pRefactored :: Bool}
>                    deriving (Eq, Show, Typeable, Data)

 -- Map Variable [[(Variable,Int)]],

> setRefactored :: Annotation -> Annotation
> setRefactored a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) (number a) True

> setUnfactored :: Annotation -> Annotation
> setUnfactored a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) (number a) False

> setNumber :: Int -> Annotation -> Annotation
> setNumber x a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) x (pRefactored a)

> setLives :: [Variable] -> Annotation -> Annotation
> setLives x a = A x (indices a) (arrsRead a) (arrsWrite a) (number a) (pRefactored a)

> setIndices :: [Variable] -> Annotation -> Annotation
> setIndices x a = A (lives a) x (arrsRead a) (arrsWrite a) (number a) (pRefactored a)

> setArrsRead :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsRead x a = A (indices a) (lives a) x (arrsWrite a) (number a) (pRefactored a)

> setArrsWrite :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsWrite x a = A (indices a) (lives a) (arrsRead a) x (number a) (pRefactored a)

> unitAnnotation = A [] [] empty empty 0 False

