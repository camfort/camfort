> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module CommonBlocks where

> import Data.Data

> import Language.Fortran
> import Language.Fortran.Pretty

> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy
> import Debug.Trace

> import Annotations
> import Syntax
> import Traverse

Calculates inter-procedural information

> type DefSites = [(String, String)]

