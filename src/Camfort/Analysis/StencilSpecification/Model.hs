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
   model (Forward dep dims) = 
     fromList . cprodVV $ [[mkVec i d | i <- [0..dep]] | d <- dims]
   model (Backward dep dims) =
     fromList . cprodVV $ [[mkVec i d | i <- [(-dep)..0]] | d <- dims]
   model (Symmetric dep dims) =
     fromList . cprodVV $ [[mkVec i d | i <- [(-dep)..dep]] | d <- dims]
   model (Constant dims) = error "No model yet"

instance Model SpecProd where
   model (Product []) = Set.empty
   model (Product ss) =
      fromList $ cprodVV $ map (toList . model) ss

cprodVV :: [[[Int]]] -> [[Int]]
cprodVV = foldr1 cprodV

-- Cross-product on vectors
cprodV :: [[Int]] -> [[Int]] -> [[Int]]
cprodV xss yss = prod (normalise xss) (normalise yss)
  where
    findMax = maximum . map length
    dims = max (findMax xss) (findMax yss)
    expand n xs = xs ++ take n [0..]
    normalise = map (expand dims)
    prod xss yss = sort . nub $ xss >>= (\xs -> yss >>= (\ys -> xs >>= (\x -> ys >>= \y -> return [x, y])))

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

check = mapM_ check' variations
  where check' (ixs, spec) = putStrLn $ show (sort ixs == sort mdl, sort ixs, sort mdl)
          where mdl = nub $ map toPair $ toList $ model spec
                toPair [x, y] = (x, y)
                toPair [x]    = (x, 0)
                toPair xs     = error $ "Got " ++ show xs