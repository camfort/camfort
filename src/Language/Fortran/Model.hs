{-# OPTIONS_GHC -Wall #-}

{-|

An embedding of Fortran expressions into strongly-typed Haskell values, and
facilities for reasoning about them.

-}

module Language.Fortran.Model
  (
  {-|
  Each Fortran type gets a corresponding Haskell type.
  -}
    module Language.Fortran.Model.Types

  {-|
  The Fortran expressions that can be formed are specified at the type level.
  -}
  , module Language.Fortran.Model.Op


  {-|
  Fortran values and expressions are represented symbolically, in a form that
  can be easily used in external theorem-provers via "Data.SBV".
  -}
  , module Language.Fortran.Model.Repr
  ) where

import Language.Fortran.Model.Types
import Language.Fortran.Model.Op
import Language.Fortran.Model.Repr
