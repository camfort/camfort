module Camfort.Specification.Stencils.InferenceBackendSpec (spec) where

import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Syntax
import Camfort.Specification.Stencils.Model
import qualified Camfort.Helpers.Vec as V

import Test.Hspec

spec :: Spec
spec =
  describe "Inference backend" $ do
    describe "spans to approximate regions" $ do

      it "handles spans of a(i-2) + a(i) + a(i+2)" $ do
        let spans = [ (V.Cons (-2) V.Nil, V.Cons (-2) V.Nil)
                    , (V.Cons 0 V.Nil, V.Cons 0 V.Nil)
                    , (V.Cons 2 V.Nil, V.Cons 2 V.Nil) ]
        let region = Right $ Bound
              (Just . Spatial $ Sum [ Product [ Centered 0 1 True ]])
              (Just . Spatial $ Sum [ Product [ Centered 2 1 True ]])
        spansToApproxSpatial spans `shouldBe` region

      it "handles spans of a(i,0) + a(0,j)" $ do
        let spans = [ ( V.Cons 0 (V.Cons absoluteRep V.Nil)
                      , V.Cons 0 (V.Cons absoluteRep V.Nil) )
                    , ( V.Cons absoluteRep (V.Cons 0 V.Nil)
                      , V.Cons absoluteRep (V.Cons 0 V.Nil) ) ]
        let region = Right . Exact .  Spatial $
              Sum [ Product [ Centered 0 1 True ]
                  , Product [ Centered 0 2 True ] ]
        spansToApproxSpatial spans `shouldBe` region
