> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Transformation.DerivedTypeIntro where

> import Data.Data
> import Data.List hiding (union, insert)
> import Data.Maybe
> import Data.Set hiding (foldl, map)

> import Data.Generics.Uniplate.Operations

> import Control.Monad.State.Lazy

> import Debug.Trace

> import Language.Fortran

> import Analysis.Annotations
> import Analysis.IntermediateReps
> import Analysis.Loops
> import Analysis.Syntax
> import Transformation.Syntax
> import Analysis.Types

> import Traverse

> typeStruct :: [(String, [Program Annotation])] -> (String, [[Program Annotation]])
> typeStruct fps = mapM (\(_, ps) -> mapM tS ps) fps

> listToSymmRelation :: [a] -> [(a, a)] 
> listToSymmRelation []     = []
> listToSymmRelation (x:xs) = ((repeat x) `zip` xs) ++ (listToSymmRelation xs)

Counts number of duplicate edges and makes this the "weight"


Compute variable coincidences for those variables that are used for indexing.

> type Graph v a = [((v, v), a)]

> type WeightedEdge v a = ((v, v), (a, Int))
> type WeightedGraph v a = [WeightedEdge v a]

> vertices :: WeightedGraph v a -> [v]
> vertices = concatMap (\((x, y), _) -> [x, y])

Non-interprocedural version first 

> calculateWeights :: (Eq (AnnotationFree a), Eq (AnnotationFree v), Ord a, Ord v) => Graph v a -> WeightedGraph v a
> calculateWeights xs = calcWs (sort xs) 1
>                       where calcWs [] _  = []
>                             calcWs [((v1, v2), a)] n = [((v1, v2), (a, n))]
>                             calcWs (e@((v1, v2), a):(e':es)) n | ((af e == af e') || (af e == (af (swap e'))))
>                                                                    = calcWs (e':es) (n + 1)
>                                                  | otherwise       = ((v1, v2), (a, n)) : (calcWs (e':es) 1)

> swap ((a, b), v) = ((b, a), v)

> locsFromIndirectReads :: Data t => t -> [(Variable, Access)]
> locsFromIndirectReads x = 
>        concat . concat $ 
>              each (Vars `from` x)
>                     (\(Var _ _ ves) -> 
>                         each ves (\(VarName _ v, ixs) -> 
>                            if (not $ all isConstant ixs) 
>                                   then map (\x -> (v, x)) (Locs `from` ixs)
>                                   else []))
>                              
> matchingTargets r ((a, x), (b, y)) | a == b = ((x, y), a) : r
>                                    | otherwise = r


> tS :: Program Annotation -> (Report, Program Annotation)
> tS p = descendBiM
>          (\b@(Block a uses implicits span decs f) ->
>                 let 
>                     tenv = typeEnv b
>                            
>                     -- Compute graph of semantically related variables used in indirect reads
>                     es = Exprs `topFrom` b
>                     ls = map locsFromIndirectReads es 
>                     lss = (foldl matchingTargets []) . (concatMap listToSymmRelation) $ ls
>                     lss' = calculateWeights lss
>                     wgf = weightedGraphToForests lss'

>                     -- Generate definitions
>                     tDefsAndNames = evalState (mapM (mkTypeDef tenv (fst span, fst span)) wgf) 0

>                     nwgf = zip wgf (map snd tDefsAndNames)
>                     

>                     rAnnotation = if (length tDefsAndNames > 0)
>                                   then unitAnnotation { refactored = Just (fst span) }
>                                   else unitAnnotation

>                     decs' = foldl (DSeq unitAnnotation) decs (map fst tDefsAndNames)
>                     a' = if (length tDefsAndNames > 0) then a { refactored = Just (fst span) } else a
>                 in  -- Create outgoing block
>                     (show lss' ++ "\n\n" ++ show wgf, Block a' uses implicits span decs' f)) p

> findMatch v ix ((wg, n):wgns) = vertices 
>                       

 replaceAccess :: [(WeightedGraph Variable Access, Variable)] -> Block Annotation -> Block Annotation
 replaceAccess wgns x = transformBi (\t@(VarName _ v, ixs) -> t) x
                                  
                                  

> mkTyDecl :: SrcSpan -> Variable -> Type Annotation -> Decl Annotation
> mkTyDecl sp v t = let ua = unitAnnotation
>                   in Decl ua sp [(Var ua sp [(VarName ua v, [])], NullExpr ua sp)] t

> mkTypeDef :: TypeEnv Annotation -> SrcSpan -> WeightedGraph Access Variable -> State Int (Decl Annotation, String)
> mkTypeDef tenv sp wg = (inventName wg) >>= (\name -> 
>                           let edgeToDecls ((vx, vy), (va, w)) = 
>                                  case (lookup va tenv) of
>                                     Just t -> [mkTyDecl sp (accessToVarName vx) (arrayElementType t),
>                                                mkTyDecl sp (accessToVarName vy) (arrayElementType t)]
>                                     Nothing -> error $ "Can't find the type of " ++ show va ++ "\n"

>                               ra = unitAnnotation { refactored = Just (fst sp) } 

>                               (_, (arrayVar, _)) = head wg

>                               tdecls = concatMap edgeToDecls wg
>                               typeDecl = DerivedTypeDef ra sp (SubName ra name) [] [] tdecls

>                               typeCons = BaseType ra (DerivedType ra (SubName ra name)) [] (NullExpr ra sp) (NullExpr ra sp)
>                               valDecl = Decl ra sp [(Var ra sp [(VarName ra (arrayVar ++ name), [])] , NullExpr ra sp)] typeCons
>                           in return $ (DSeq unitAnnotation typeDecl valDecl, name))

> inventName :: WeightedGraph Access Variable -> State Int String
> inventName graph = do n <- get
>                       put (n + 1)
>                       let vs = vertices graph
>                       return $ map mode (transpose (map accessToVarName vs)) ++ (show n)
                        
> mode :: String -> Char
> mode x = let freqs = (map (\x -> (head x, length x))) . group . sort $ x
>              sortedFreqs = sortBy (\x -> \y -> (snd x) `compare` (snd y)) freqs
>              max = last sortedFreqs
>          in -- mode or 'X' if mode is less than the majority
>             if (snd max) > ((length x) `div` 2) then fst max else 'X'

> weightedGraphToForests :: forall v a . (Show v, Ord v, Ord a) => WeightedGraph v a -> [WeightedGraph v a]
> weightedGraphToForests g = map snd (concatMap (foldl binEdge []) (groupBy groupOnArrayVar (sortBy sortOnArrayVar g)))
>                             where groupOnArrayVar (_, (av, _)) (_, (av', _)) = av == av'
>                                   sortOnArrayVar (_, (av, _)) (_, (av', _)) = compare av av'

map snd (foldl binEdge [] g)

"bins" edges into a list of graphs with a set of their vertices

> binEdge :: (Show v, Ord v, Ord a) => [(Set v, WeightedGraph v a)] -> WeightedEdge v a -> [(Set v, WeightedGraph v a)]
> binEdge bins e@((x, y), _) = 
>     let findBin v [] = ((insert x empty, []), [])
>         findBin v ((vs, es):bs) | member v vs = ((insert v vs, es), bs)
>                                 | otherwise = let (n, bs') = findBin v bs
>                                               in (n, (vs, es) : bs')
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