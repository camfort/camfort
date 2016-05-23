{-# LANGUAGE TypeFamilies #-}

module Camfort.Analysis.StencilSpecification.ModelSpec (spec) where

import Camfort.Helpers.Vec
import Camfort.Analysis.StencilSpecification
import Camfort.Analysis.StencilSpecification.Synthesis
import Camfort.Analysis.StencilSpecification.Model
import Camfort.Analysis.StencilSpecification.Syntax hiding (Spec)

import Camfort.Analysis.Annotations
import qualified Language.Fortran.AST as F
import Language.Fortran.Util.Position

import Data.List
import Data.Map hiding (map)

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Stencils - Model" $ do
    describe "Soundness of model, on some examples" $ modelHasLeftInverse

variations =
  [ ([ (0,0) ],
    Spatial NonLinear [] [ 1, 2 ] (Sum [Product []]))

  , ([ (1,0), (0,0) ],
    Spatial NonLinear [] [2] (Sum [Product [Forward 1 1]]))

  , ([ (0,1), (0,0) ],
    Spatial NonLinear [] [1] (Sum [Product [Forward 1 2]]))

  , ([ (1,1), (0,1), (1,0), (0,0) ],
    Spatial NonLinear [] [] (Sum [Product [Forward 1 1, Forward 1 2]]))

  , ([ (-1,0), (0,0) ],
    Spatial NonLinear [] [2] (Sum [Product [Backward 1 1]]))

  , ([ (0,-1), (0,0) ],
    Spatial NonLinear [] [1] (Sum [Product [Backward 1 2]]))

  , ([ (-1,-1), (0,-1), (-1,0), (0,0) ],
    Spatial NonLinear [] [] (Sum [Product [Backward 1 1, Backward 1 2]]))

  , ( [ (0,-1), (1,-1), (0,0), (1,0), (1,1), (0,1), (2,-1), (2,0), (2,1) ],
    Spatial NonLinear [] []
              (Sum [Product [ Forward 2 1, Centered 1 2 ] ] ))

  , ( [ (-1,0), (-1,1), (0,0), (0,1), (1,1), (1,0), (-1,2), (0,2), (1,2) ],
    Spatial NonLinear [] []
              (Sum [Product [ Forward 2 2, Centered 1 1 ] ] ))

  -- Stencil which is non-contiguous from the origin in both directions
  , ([ (0, constantRep), (1, constantRep) ],
    Spatial NonLinear [] [] (Sum [Product [Constant 2, Forward 1 1]]))
 ]

modelHasLeftInverse = mapM_ check (zip variations [0..])
  where check ((ixs, spec), n) = it ("("++show n++")") $ (sort mdl) `shouldBe` (sort ixs)
          where mdl = map (toPair . fst) $ toList $ (model (Specification (Left spec)))
        toPair [x, y] = (x, y)
        toPair [x]    = (x, 0)
        toPair xs     = error $ "Got " ++ show xs