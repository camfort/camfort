module Camfort.Specification.Stencils.DenotationalSemanticsSpec (spec) where

import qualified Camfort.Helpers.Vec as V

import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Consistency
import Camfort.Specification.Stencils.Syntax
import Camfort.Specification.Stencils.DenotationalSemantics

import qualified Data.Set as S
import Algebra.Lattice

import Test.Hspec

spec :: Spec
spec =
  describe "Denotational semantics spec" $ do
    let fivePointSpatial = Spatial $
          Sum [ Product [ Centered 1 1 True, Centered 0 2 True ]
              , Product [ Centered 1 2 True, Centered 0 1 True ] ]
    it "transforms five point spatial correctly to union normal form" $ do
      let regFivePoint = Right $
            return (V.Cons (IntervHoled (-1) 1 True)
                           (V.Cons (IntervHoled 0 0 True) V.Nil))
            \/
            return (V.Cons (IntervHoled 0 0 True)
                           (V.Cons (IntervHoled (-1) 1 True) V.Nil))
      shouldBe (regionsToIntervals (V.Succ (V.Succ V.Zero)) fivePointSpatial)
               regFivePoint
