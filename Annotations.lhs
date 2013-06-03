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

Initial annotations from parser

Type of annotations

> type A0 = (SrcLoc, SrcLoc) 

Given a source location (usually token start),
get the current src loc from the parser monad (usually token end), 
return as pair giving bounds on the syntax span

> srcSpan :: SrcLoc -> P A0
> srcSpan l = do l' <- getSrcLoc
>                return $ (l, l')

0-length span at current position

> srcSpanNull :: P A0
> srcSpanNull = do l <- getSrcLoc
>                  return $ (l, l)

Combinators to generate spans anchored at existing elements

> class SrcSpanFromAnnotation t where
>    srcSpanFrom :: Copointed d => d t -> P A0

>    srcSpanFromL :: Copointed d => d t -> (A0 -> b) -> P b
>    srcSpanFromL x f = do a <- srcSpanFrom x
>                          return $ f a

> instance SrcSpanFromAnnotation A0 where
>    srcSpanFrom x = do let l = fst $ copoint x
>                       l' <- getSrcLoc
>                       return $ (l, l')

> instance SrcSpanFromAnnotation SrcLoc where
>    srcSpanFrom x = do let l = copoint x
>                       l' <- getSrcLoc
>                       return $ (l, l')
