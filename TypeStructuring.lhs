> {-# LANGUAGE FlexibleContexts #-}

> module TypeStructuring where

> import Data.List 

> import Debug.Trace

 import Data.Graph

> import Language.Fortran
> import Annotations
> import Loops
> import Syntax
> import Traverse

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



> tS p = each (Blocks `from` p) $
>          \b ->  let es = Exprs `topFrom` b
>                     lss = concatMap (mkSCgraph . (Locs `from`)) es
>                     lss' = calculateWeights $ sort lss
>                 in lss' 


