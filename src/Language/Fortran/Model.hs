{-# OPTIONS_GHC -Wall #-}

{-|

The goal of this module is to embed the Fortran type system in Haskell.

* Each Fortran type gets a corresponding Haskell type, and
  "Language.Fortran.Model.Types" describes how that works.

* The Fortran expressions that can be formed are described at the
  type level in "Language.Fortran.Model.Op".

* Fortran values and expressions are represented symbolically, in a form that
  can be easily used in external theorem-provers via "Data.SBV". Symbolic
  representations for each Fortran type are described in
  "Language.Fortran.Model.Repr".

-}

module Language.Fortran.Model
  ( module Types
  , module Op
  ) where

import Language.Fortran.Model.Types as Types
import Language.Fortran.Model.Op as Op
