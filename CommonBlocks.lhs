> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module CommonBlocks where

> import Data.Data

> import Language.Fortran
> import Language.Fortran.Pretty

> import Annotations
> import Syntax
> import Traverse

> type Commons p = [(Maybe String, [Expr p])]

> commonElim :: [(String, [Program Annotation])] -> [[Program Annotation]]
> commonElim = let cms = collectCommons 
>              in (show cms) `trace` []

> collectCommons :: (Block Annotation) -> State Commons (Block Annotation)
> collectCommons = transformBiM commons' 
>                   where commons' :: Fortran Annotation -> State Commons (Fortran Annotation)
>                         commons' f@(Common a sp 