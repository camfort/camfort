{-# OPTIONS_GHC -Wall #-}

{-|

The root module for the language of Fortran operators.

"Language.Expression" provides a way of forming expressions from what we call
/operators/, via 'HFree'.

For any type constructor @op@ of kind @(* -> *) -> * -> *@, @'HFree' op v a@ can
be seen as an /expression/ over @op@. @op@ defines how we may combine different
expressions. This is best illustrated with a simple example.

@
-- An operator type has two arguments. @t@ is a type constructor used to refer
-- to expressions. @a@ is the semantic type of the expression formed by the
-- operator.
data SimpleOp t a where
  -- Given two int expressions, we may add them. Notice how we refer to
  -- expressions recursively with the type constructor parameter @t@.
  Add :: t Int -> t Int -> SimpleOp t Int

  -- An operator does not have to actually combine expressions. It may produce
  -- an expression from a basic value, i.e. a literal int.
  Literal :: Int -> SimpleOp t Int
@


These modules define
operators designed to be used as the higher-ranked functors for the
higher-ranked free monads defined in "Language.Expression". For example,
@'HFree' 'CoreOp' v a@ is an expression over core Fortran operators with
variables in @v@ representing a computation of type @a@.

-}
module Language.Fortran.Model.Op
  (
    -- * Core Fortran operators

    {-|
    \'Core\' Fortran operators, that is operators that directly represent parts of 
    -}

    module Language.Fortran.Model.Op.Core
    -- * Meta-level Fortran operators
  , module Language.Fortran.Model.Op.Meta
    -- * High-level operators
  , module Language.Fortran.Model.Op.High
  ) where


import           Language.Fortran.Model.Op.Core
import           Language.Fortran.Model.Op.High
import           Language.Fortran.Model.Op.Meta
