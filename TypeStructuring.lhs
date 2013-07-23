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

> mkSCgraph :: [a] -> [(a, a)] 
> mkSCgraph []     = []
> mkSCgraph (x:xs) = ((repeat x) `zip` xs) ++ (mkSCgraph xs)

Counts number of duplicate edges and makes this the "weight"


Compute variable coincidences for those variables that are used for indexing.



Non-interprocedural version first 

> calculateWeights :: Eq (AnnotationFree a) => [(a, a)] -> [((a, a), Int)]
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

> tS p = each (Blocks `from` p) $
>          \b ->  let tenv = typeEnv b
>                     es = Exprs `topFrom` b
>                     lss = concatMap (mkSCgraph . locsFromIndirectReads) es
>                     lss' = filter (\((a, _), (b, _)) -> a == b) lss 
>                     lss'' = calculateWeights $ sort (map (\((_, x), (_, y)) -> (x, y)) lss')
>                 in lss''


