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
    NonLinear $ SpatialSpec [] [ 1, 2 ] (Summation [Product []]))

  , ([ (1,0), (0,0) ],
    NonLinear $ SpatialSpec [] [2] (Summation [Product [Forward 1 [ 1 ]]]))

  , ([ (0,1), (0,0) ],
    NonLinear $ SpatialSpec [] [1] (Summation [Product [Forward 1 [ 2 ]]]))

  , ([ (1,1), (0,1), (1,0), (0,0) ],
    NonLinear $ SpatialSpec [] [] (Summation [Product [Forward 1 [ 1, 2 ]]]))

  , ([ (-1,0), (0,0) ],
    NonLinear $ SpatialSpec [] [2] (Summation [Product [Backward 1 [ 1 ]]]))

  , ([ (0,-1), (0,0) ],
    NonLinear $ SpatialSpec [] [1] (Summation [Product [Backward 1 [ 2 ]]]))

  , ([ (-1,-1), (0,-1), (-1,0), (0,0) ],
    NonLinear $ SpatialSpec [] [] (Summation [Product [Backward 1 [ 1, 2 ]]]))

  , ( [ (0,-1), (1,-1), (0,0), (1,0), (1,1), (0,1), (2,-1), (2,0), (2,1) ],
    NonLinear $ SpatialSpec [] []
              (Summation [Product [ Forward 2 [ 1 ], Symmetric 1 [ 2 ] ] ] ))

  , ( [ (-1,0), (-1,1), (0,0), (0,1), (1,1), (1,0), (-1,2), (0,2), (1,2) ],
    NonLinear $ SpatialSpec [] []
              (Summation [Product [ Forward 2 [ 2 ], Symmetric 1 [ 1 ] ] ] ))

  -- Stencil which is non-contiguous from the origin in both directions
  , ([ (0, 1), (1, 1) ],
    NonLinear $ SpatialSpec [] [] (Summation [Product [Forward 1 [ 1 ]]]))
 ]

modelHasLeftInverse = mapM_ check (zip variations [0..])
  where check ((ixs, spec), n) = it ("("++show n++")") $ (sort ixs) `shouldBe` (sort mdl)
          where mdl = nub $ map (toPair . fst) $ toList $ model spec
        toPair [x, y] = (x, y)
        toPair [x]    = (x, 0)
        toPair xs     = error $ "Got " ++ show xs