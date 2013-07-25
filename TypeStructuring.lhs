> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module TypeStructuring where

> import Data.Data
> import Data.List hiding (union, insert)
> import Data.Maybe
> import Data.Set hiding (foldl, map)

> import Data.Generics.Uniplate.Operations

> import Debug.Trace

> import Language.Fortran

> import Annotations
> import IntermediateReps
> import Loops
> import Syntax
> import Traverse
> import Types

> typeStruct :: [(String, [Program Annotation])] -> (String, [[Program Annotation]])
> typeStruct fps = mapM (\(_, ps) -> mapM tS ps) fps

> listToSymmRelation :: [a] -> [(a, a)] 
> listToSymmRelation []     = []
> listToSymmRelation (x:xs) = ((repeat x) `zip` xs) ++ (listToSymmRelation xs)

Counts number of duplicate edges and makes this the "weight"


Compute variable coincidences for those variables that are used for indexing.

> type Graph a = [(a, a)]

> type WeightedEdge a = ((a, a), Int)
> type WeightedGraph a = [WeightedEdge a]

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


> tS :: Program Annotation -> (Report, Program Annotation)
> tS p = descendBiM
>          (\b@(Block a uses implicits span decs f) ->
>                 let tenv = typeEnv b
>                     es = Exprs `topFrom` b
>                     ls = map locsFromIndirectReads es 
>                     lss = (foldl matchingTargets []) . (concatMap listToSymmRelation) $ ls
>                     lss' = calculateWeights $ sort lss
>                     wgf = weightedGraphToForests lss'
>                     tDefs = map ((mkTypeDef (fst span, fst span)). inventName) wgf
>                     rAnnotation = if (length tDefs > 0) then unitAnnotation { refactored = Just (fst span) } else unitAnnotation
>                     decs' = foldl (DSeq unitAnnotation) decs tDefs
>                     a' = if (length tDefs > 0) then a { refactored = Just (fst span) } else a
>                 in  -- Create outgoing block
>                     ((show $ length tDefs) ++ "\n", Block a' uses implicits span decs' f)) p

> mkTypeDef sp name = let ra = unitAnnotation { refactored = Just (fst sp) } 
>                     in DerivedTypeDef ra sp (SubName ra name) [] [] []

> inventName :: WeightedGraph Access -> String
> inventName graph = let vs = vertices graph
>                     in map mode (transpose (map accessToVarName vs))
                        
> mode :: String -> Char
> mode x = let freqs = (map (\x -> (head x, length x))) . group . sort $ x
>              sortedFreqs = sortBy (\x -> \y -> (snd x) `compare` (snd y)) freqs
>              max = last sortedFreqs
>          in -- mode or 'X' if mode is less than the majority
>             if (snd max) > ((length x) `div` 2) then fst max else 'X'

> weightedGraphToForests :: forall a . (Show a, Ord a) => WeightedGraph a -> [WeightedGraph a]
> weightedGraphToForests g = map snd (foldl binEdge [] g)

"bins" edges into a list of graphs with a set of their vertices

> binEdge :: (Show a, Ord a) => [(Set a, WeightedGraph a)] -> WeightedEdge a -> [(Set a, WeightedGraph a)]
> binEdge bins e@((x, y), _) = 
>     let findBin v [] = ((insert x empty, []), [])
>         findBin v ((vs, es):bs) | member v vs = ((insert v vs, es), bs)
>                                       | otherwise = let (n, bs') = findBin v bs
>                                                     in (n, (vs, es) : bs')
>         ((vs, es), bins') = findBin x bins
>         ((vs', es'), bins'') = findBin y bins'
>     in (vs `union` vs', e : (es ++ es')) : bins''
>                                       
>  


 binEdge bins e@((x, y), _) = let r = binVertex y e (binVertex x e bins) in (show r) `trace` r

 binVertex :: Ord a => a -> WeightedEdge a -> [(Set a, WeightedGraph a)] -> [(Set a, WeightedGraph a)]
 binVertex x e ss = bin' x e ss [] Nothing
                     where bin' x e []     bs' Nothing  = (insert x empty, [e]) : bs'
                           bin' x e []     bs' (Just s) = s : bs'
                                                          
                           bin' x e ((vs, es):bs) bs' ms | member x vs = 
                             case ms of 
                              Nothing -> bin' x e bs bs' (Just (insert x vs, e:es))
                              Just (vs', es') -> bin' x e bs bs' (Just (union vs' (insert x  vs'), (e:es) ++ es'))
                                                         | otherwise = bin' x e bs ((vs, es):bs) ms