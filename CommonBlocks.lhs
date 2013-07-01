> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module CommonBlocks where

> import Data.Data

> import Language.Fortran
> import Language.Fortran.Pretty

> type Commons p = [(Maybe String, [Expr p])]

> commonElim = undefined