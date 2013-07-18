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

> calculateWeights :: Eq (AnnotationFree (a, a)) => [(a, a)] -> [((a, a), Int)]
> calculateWeights xs = calcWs xs 0
>                       where calcWs [] _  = []
>                             calcWs [e] n = [(e, n)]
>                             calcWs (e:(e':es)) n | (af e == af e') = calcWs (e':es) (n + 1)
>                                                  | otherwise       = (e, n) : (calcWs (e':es) 0)

Non-interprocedural version first 

> tS p = "1" `trace` each (Blocks `from` p) $
>          \b ->  let es = Exprs `from` b
>                     lss = concatMap (mkSCgraph . (Locs `from`)) es
>                     lss' = calculateWeights $ sort lss
>                 in lss' --  concatMap (Locs `from`) es
