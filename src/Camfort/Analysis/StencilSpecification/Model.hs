module Camfort.Analysis.StencilSpecification.Model where

import Camfort.Analysis.StencilSpecification.Syntax
import Data.Set hiding (map,foldl')
import qualified Data.Set as Set
import Data.List

class Model t where
   model :: t -> Set [Int]

mkVec :: Int -> Int -> [Int]
mkVec i 1 = [i]
mkVec i d = 0 : (mkVec i $ d - 1)

instance Model Spec where
   model (Forward d dims) =
     fromList [mkVec i d | i <- [0..d], d <- dims]
   model (Backward d dims) =
     fromList [mkVec i d | i <- [0..d], d <- dims]
   model (Symmetric d dims) =
     fromList [mkVec i d | i <- [-d..d], d <- dims]    
   model (Constant dims) = error "No model yet"

instance Model SpecProd where
   model (Product ss) =
      fromList $ foldl' cprodV [] (map (toList . model) ss)
       where -- Cross-product on vectors
             cprodV :: [[Int]] -> [[Int]] -> [[Int]]
             cprodV xss yss = xss >>= (\xs -> yss >>= (\ys -> [xs ++ ys])) 

instance Model SpecUnion where
   model (Union ss) = unions (map model ss)

instance Model SpatialSpec where
   model (SpatialSpec irrefls refls s) =
      Set.difference
        (Set.union (fromList [mkVec 0 d | d <- refls]) (model s))
        (fromList [mkVec 0 d | d <- irrefls])

instance Model Specification where
   model (Linear s) = model s

   -- TODO, extend to multi-sets
   model (NonLinear s) = model s
         
   model Empty = Set.empty
   model _     = error "No model for temporal specs yet"

-- Local test

variations =
        [ ([ (0,0) ], NonLinear $ SpatialSpec [] [ 1, 2 ] (Union [Product []]))
        , ([ (1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Union [Product [Forward 1 [ 1 ]]]))
        , ([ (0,1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Union [Product [Forward 1 [ 2 ]]]))
        , ([ (1,1), (0,1), (1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Union [Product [Forward 1 [ 1, 2 ]]]))
        , ([ (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [2] (Union [Product [Backward 1 [ 1 ]]]))
        , ([ (0,-1), (0,0) ], NonLinear $ SpatialSpec [] [1] (Union [Product [Backward 1 [ 2 ]]]))
        , ([ (-1,-1), (0,-1), (-1,0), (0,0) ], NonLinear $ SpatialSpec [] [] (Union [Product [Backward 1 [ 1, 2 ]]]))
        , ( [ (0,-1), (1,-1), (0,0), (1,0), (1,1), (0,1) ]
          , NonLinear $ SpatialSpec [] [] (Union [Product [ Forward 1 [ 1 ], Symmetric 1 [ 2 ] ] ] ))
         -- Stencil which is non-contiguous from the origin in both directions
        , ([ (0, 1), (1, 1) ], NonLinear $ SpatialSpec [] [] (Union [Product [Forward 1 [ 1 ]]]))
        ]

check = mapM check' variations
  where check' (ixs, spec) = putStrLn $ show (ixs == mdl, ixs, mdl)
          where mdl = nub $ map toPair $ toList $ model spec
                toPair [x, y] = (x, y)
                toPair [x]    = (x, 0)