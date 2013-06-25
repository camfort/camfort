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

> import Debug.Trace

> import Language.Fortran.Pretty

> import Language.Haskell.ParseMonad 
> import Language.Haskell.Syntax (SrcLoc(..))

Loop classifications 

> data ReduceType = Reduce | NoReduce
> data AccessPatternType = Regular | RegularAndConstants | Irregular | Undecidable 
> data LoopType = Functor ReduceType | Gather ReduceType ReduceType AccessPatternType | Scatter ReduceType AccessPatternType

 classify :: Fortran Annotation -> Fortran Annotation
 classify x = 

> data Annotation = A {indices :: [Variable],
>                      lives ::([Variable],[Variable]),
>                      arrsRead :: Map Variable [[Expr ()]], 
>                      arrsWrite :: Map Variable [[Expr ()]],
>                      number :: Int,
>                      refactored :: Maybe SrcLoc}
>                    deriving (Eq, Show, Typeable, Data)

 -- Map Variable [[(Variable,Int)]],

> pRefactored :: Annotation -> Bool
> pRefactored x = case (refactored x) of
>                   Nothing -> False
>                   Just _  -> True

> setRefactored :: SrcLoc -> Annotation -> Annotation
> setRefactored x a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) (number a) (Just x)

> setUnfactored :: Annotation -> Annotation
> setUnfactored a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) (number a) Nothing

> setNumber :: Int -> Annotation -> Annotation
> setNumber x a = A (indices a) (lives a) (arrsRead a) (arrsWrite a) x (refactored a)

> setLives :: ([Variable], [Variable]) -> Annotation -> Annotation
> setLives x a = A (indices a) x (arrsRead a) (arrsWrite a) (number a) (refactored a)

> setIndices :: [Variable] -> Annotation -> Annotation
> setIndices x a = A x (lives a) (arrsRead a) (arrsWrite a) (number a) (refactored a)

> setArrsRead :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsRead x a = A (indices a) (lives a) x (arrsWrite a) (number a) (refactored a)

> setArrsWrite :: Map Variable [[Expr ()]] -> Annotation -> Annotation
> setArrsWrite x a = A (indices a) (lives a) (arrsRead a) x (number a) (refactored a)

> unitAnnotation = A [] ([], []) empty empty 0 Nothing

-- Indenting for refactored code

> instance Indentor Annotation where
>     indR t i = case (refactored . copoint $ t) of
>                  Just (SrcLoc f _ c) -> take c (repeat ' ')
>                  Nothing             -> ind i