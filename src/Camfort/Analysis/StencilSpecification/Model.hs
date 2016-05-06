module Camfort.Analysis.StencilSpecification.Model where

import Camfort.Analysis.StencilSpecification.Syntax
import Data.Set hiding (map,foldl')
import qualified Data.Set as Set
import Data.List

class Model t where
   model :: t -> Set [Int]

mkVec :: Int -> Int -> [Int]
mkVec i 0 = [i]
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
   model (SpatialSpec irefls refls s) = model s
      
