> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE ImplicitParams #-}

> module Traverse where

> import Language.Fortran.Parser
> import Language.Fortran

 import Generics.Deriving.Base

 class GComonad d where
    gextend :: (d a -> b) -> d a -> d b

> import Control.Comonad

> instance Comonad SubName where
>     extract (SubName x _) = x
>     extract (NullSubName x) = x

>     extend f y@(Subname x s) = SubName (f y) s
>     extend f NullSubName = NullSubName

> instance Comonad VarName where
>     extract (VarName x _) = x
>     extend f y@(VarName x s) = VarName (f y) s

> instance Comonad ArgName where
>     extract (ArgName x _) = x
>                             

