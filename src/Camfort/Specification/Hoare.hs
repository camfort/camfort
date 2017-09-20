{-# LANGUAGE OverloadedStrings #-}

{-|

Hoare-logic-based invariant annotation and automated checking for Fortran.

= Annotations

Programs must be annotation in order to check them. Annotations are of the form

== Static assertions

@
!= static_assert <keyword> (<logic_expression>)
@

The possible keywords are

  [@pre@] program unit preconditions.

  [@post@] program unit postconditions.

  [@seq@] invariants between program statements.

  [@invariant@] loop invariant annotations.

== Logical Expressions

A logical expression is an expression formed from quoted Fortran expressions
combined with an arbitrary combination of the logical operators

  [ @&@ ] conjunction

  [ @|@ ] disjunction

  [ @->@ ] implication

  [ @\<-\>@ ] bi-implication, equivalence

  [ @!@ ] negation

For example,

@
"x >= 3" & ("y = 7" | "y = z")
@

is a valid logical expression.

== Variables

In logical expressions, you may use any Fortran variables that are declared in
the associated program unit, and you may use an annotated function's return
variable as long as it has a declared type (i.e. the function's name unless
otherwise specified). In addition, you may declare auxiliary variables using the
syntax

@
!= decl_aux("\<type spec\>" :: \<name\>)
@

  [@\<type spec\>@] an arbitrary Fortran type specification, such as
    @integer@ or @real(kind=8,dimensions=(3,4))@

  [@\<name\>@] any string (case-insensitive) that is a valid Fortran
    identifier, such as @my_var123@.

Using any undeclared variables will result in an error.

== Necessary Annotations

Annotations are required at the following program points:

* Between any two statements @S1@ and @S2@, where @S2@ is not an assignment.
* On the line after @do ...@.

== Unsupported Program Constructs

Many parts of Fortran are currently unsupported. You will receive a helpful
error message if you try to use something that's unsupported. Notable
unsupported constructs are:

* Standard @do@ loops (@do while@ loops /are/ supported)
* Multi-dimensional arrays
* User-defined data types
* Program sub-units
* Intrinsic functions
* @write@, @read@, etc
* Subroutine and function calls

== Example

Here's an example of a properly annotated Fortran program:

@
!= decl_aux("integer" :: x_)
!= decl_aux("integer" :: y_)
!= static_assert pre("x == x_" & "y == y_")
!= static_assert post("multiply == x_ * y_")
integer function multiply(x, y)
  implicit none

  integer :: x, y
  integer :: r, n

  if (x < 0) then
     x = -x
     y = -y
  end if

  r = 0
  n = 0
  != static_assert seq("x * y == x_ * y_" & "n == 0" & "r == 0" & "n <= x")
  do while (n < x)
     != static_assert invariant("x * y == x_ * y_" & "r == n * y" & "n <= x")
     r = r + y
     n = n + 1
  end do

  multiply = r
end function multiply
@

-}
module Camfort.Specification.Hoare (check, HoareCheckResults(..), PrimReprOption(..)) where

import           Control.Monad.Except
import           Data.List                                 (intersperse)

import qualified Language.Fortran.Analysis                 as FA
import qualified Language.Fortran.Analysis.BBlocks         as FAB
import qualified Language.Fortran.Analysis.Renaming        as FAR
import qualified Language.Fortran.AST                      as F
import qualified Language.Fortran.Util.Position            as F

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.ModFile
import           Camfort.Helpers
import           Camfort.Input
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.CheckFrontend
import           Camfort.Specification.Hoare.Parser
import           Language.Fortran.Model.Repr.Prim

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

newtype HoareCheckResults = HoareCheckResults [HoareCheckResult]

instance Describe HoareCheckResults where
  describeBuilder (HoareCheckResults rs) =
    mconcat . intersperse "\n" . map describeBuilder $ rs

-- TODO: Give more control here
data PrimReprOption = PROIdealized | PROPrecise

--------------------------------------------------------------------------------
--  Checking
--------------------------------------------------------------------------------

{-|
The main entry point for the invariant checking analysis. Runs invariant
checking on every annotated program unit in the given program file.

The 'PrimReprOption' argument controls how Fortran data types are treated
symbolically. See the documentation in "Language.Fortran.Model.Repr.Prim" for a
detailed explanation.
-}
check :: PrimReprOption
      -> F.ProgramFile Annotation
      -> HoareAnalysis HoareCheckResults
check pro = fmap HoareCheckResults . invariantChecking (fromPrimReprOption pro) . getBlocks

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

getBlocks :: F.ProgramFile A -> F.ProgramFile (FA.Analysis (HoareAnnotation A))
getBlocks =
  FAB.analyseBBlocks . FAR.analyseRenames .
  FA.initAnalysis . fmap hoareAnn0

defaultSymSpec :: PrimReprSpec
defaultSymSpec = prsIdealized

fromPrimReprOption :: PrimReprOption -> PrimReprSpec
fromPrimReprOption PROIdealized = prsIdealized
fromPrimReprOption PROPrecise = prsPrecise

--------------------------------------------------------------------------------
--  Testsing
--------------------------------------------------------------------------------

testOn :: FilePath -> IO ()
testOn fp = do
  (mfs, pfsSources) <- loadModAndProgramFiles simpleCompiler () fp fp []
  describePerFileAnalysis
    "invariant checking"
    (check PROIdealized)
    (logOutputStd True)
    LogDebug
    mfs
    pfsSources

testHoare = do
  testOn "camfort/samples/invariants/arrays.f90"
  testOn "camfort/samples/invariants/invariants.f90"
