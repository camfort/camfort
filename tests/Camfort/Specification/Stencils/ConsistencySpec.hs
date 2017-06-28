module Camfort.Specification.Stencils.ConsistencySpec (spec) where

import qualified Camfort.Helpers.Vec as V

import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Consistency
import Camfort.Specification.Stencils.Syntax

import qualified Data.Set as S
import Algebra.Lattice

import Test.Hspec

spec :: Spec
spec =
  describe "Consistency spec" $ do
    let fivePointSpec = Specification (Once . Exact . Spatial $
          Sum [ Product [ Centered 1 1 True, Centered 0 2 True ]
              , Product [ Centered 1 2 True, Centered 0 1 True ] ]) True
    let offFivePoint =
          return (V.Cons (Offsets . S.fromList $ [-1])
                         (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
          \/
          return (V.Cons (Offsets . S.fromList $ [0])
                         (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
          \/
          return (V.Cons (Offsets . S.fromList $ [1])
                         (V.Cons (Offsets . S.fromList $ [0]) V.Nil))
          \/
          return (V.Cons (Offsets . S.fromList $ [0])
                         (V.Cons (Offsets . S.fromList $ [-1]) V.Nil))
          \/
          return (V.Cons (Offsets . S.fromList $ [0])
                         (V.Cons (Offsets . S.fromList $ [1]) V.Nil))
    let fivePointIndices = Once offFivePoint
    it "finds read once five point stencil consistent with its indices" $
      fivePointSpec `consistent` fivePointIndices `shouldBe` Consistent
