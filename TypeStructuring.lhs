> {-# LANGUAGE FlexibleContexts #-}

> module TypeStructuring where

> import Data.Data
> import Data.List 
> import Data.Maybe

> import Debug.Trace

 import Data.Graph

> import Language.Fortran

> import Annotations
> import IntermediateReps
> import Loops
> import Syntax
> import Traverse
> import Types

> typeStruct :: [Program Annotation] -> [Program Annotation]
> typeStruct x = let x' = loopAnalyse x
>                in (show $ map tS x) `trace` x

> listToSymmRelation :: [a] -> [(a, a)] 
> listToSymmRelation []     = []
> listToSymmRelation (x:xs) = ((repeat x) `zip` xs) ++ (listToSymmRelation xs)

Counts number of duplicate edges and makes this the "weight"


Compute variable coincidences for those variables that are used for indexing.

> type Graph a = [(a, a)]
> type WeightedGraph a = [((a, a), Int)]

> vertices :: WeightedGraph a -> [a]
> vertices = concatMap (\((x, y), _) -> [x, y])

Non-interprocedural version first 

> calculateWeights :: Eq (AnnotationFree a) => Graph a -> WeightedGraph a
> calculateWeights xs = calcWs xs 1
>                       where calcWs [] _  = []
>                             calcWs [e] n = [(e, n)]
>                             calcWs (e:(e':es)) n | ((af e == af e') || (af e == (af (swap e'))))
>                                                                    = calcWs (e':es) (n + 1)
>                                                  | otherwise       = (e, n) : (calcWs (e':es) 1)

> swap (a, b) = (b, a)

> locsFromIndirectReads :: Data t => t -> [(String, Access)]
> locsFromIndirectReads x = 
>        concat . concat $ 
>              each (Vars `from` x)
>                     (\(Var _ _ ves) -> 
>                         each ves (\(VarName _ v, ixs) -> 
>                            if (not $ all isConstant ixs) 
>                                   then map (\x -> (v, x)) (Locs `from` ixs)
>                                   else []))
>                              
> matchingTargets r ((a, x), (b, y)) | a == b = (x, y) : r
>                                    | otherwise = r


> tS p = each (Blocks `from` p) $
>          \b ->  let tenv = typeEnv b
>                     es = Exprs `topFrom` b
>                     ls = map locsFromIndirectReads es 
>                     lss = (foldl matchingTargets []) . (concatMap listToSymmRelation) $ ls
>                     lss' = calculateWeights $ sort lss
>                 in (lss', inventName lss')

> inventName :: WeightedGraph Access -> String
> inventName graph = let vs = vertices graph
>                     in map mode (transpose (map accessToVarName vs))
                        
> mode :: (Ord a) => [a] -> a
> mode x = fst . last . (sortBy (\x -> \y -> (snd x) `compare` (snd y))) . (map (\x -> (head x, length x))) . group . sort $ x 

 sccToDType :: WeightedGraph Access -> TypeEnv Annotation ->  Decl Annotation
 sccToDType graph tenv = let vs = vertices graph
                             
>               
