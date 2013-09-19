> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}

> {-# LANGUAGE DeriveGeneric #-}

> module Transformation.Syntax where

Standard imports 

> import Data.Char
> import Data.List
> import Control.Monad.State.Lazy

Data-type generics imports

> import Data.Data
> import Data.Generics.Uniplate.Data
> import Data.Generics.Uniplate.Operations
> import Data.Generics.Zipper
> import Data.Typeable

CamFort specific functionality

> import Analysis.Annotations
> import Analysis.IntermediateReps
> import Traverse
> import Language.Fortran
> import Language.Haskell.Syntax (SrcLoc(..))

TODO: Needs fixing with the spans - need to pull apart and put back together

> reassociate :: Fortran Annotation -> Fortran Annotation
> reassociate (FSeq a1 sp1 (FSeq a2 sp2 a b) c) = FSeq a1 sp1 (reassociate a) (FSeq a2 sp2  (reassociate b) (reassociate c))
> reassociate t = t

 reassociate :: Fortran Annotation -> Fortran Annotation
 reassociate (FSeq a1 sp1 (FSeq a2 sp2 a b) c) = FSeq a1 sp1 (reassociate a) (FSeq a2 sp2  (reassociate b) (reassociate c))
 reassociate t = t


Helpers to do with source locations and parsing

> refactorSpan :: SrcSpan -> SrcSpan
> refactorSpan (SrcLoc f ll cl, SrcLoc _ lu cu) = (SrcLoc f (lu+1) 0, SrcLoc f lu cu)

> refactorSpanN :: Int -> SrcSpan -> SrcSpan
> refactorSpanN n (SrcLoc f ll cl, SrcLoc _ lu cu) = (SrcLoc f (lu+1+n) 0, SrcLoc f (lu+n) cu)

> toCol0 (SrcLoc f l c) = SrcLoc f l 0

dropLine extends a span to the start of the next line
This is particularly useful if a whole line is being redacted from a source file

> dropLine :: SrcSpan -> SrcSpan
> dropLine (s1, SrcLoc f l c) = (s1, SrcLoc f (l+1) 0)

> srcLineCol :: SrcLoc -> (Int, Int)
> srcLineCol (SrcLoc _ l c) = (l, c)
