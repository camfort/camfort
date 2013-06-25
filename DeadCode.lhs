> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveGeneric #-}

> module DeadCode where

> import Annotations
> import Language.Fortran

> import Generics.Deriving.Copoint
> import GHC.Generics

> import Data.Generics.Uniplate.Operations

> deadCode :: [Program Annotation] -> [Program Annotation]
> deadCode x = undefined