> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Analysis.Loops where

> import Data.Data
> import Data.List
> import Data.Ord

> import Language.Fortran
> import Language.Fortran.Pretty


> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy
> import Debug.Trace

> import Analysis.LVA
> import Analysis.Annotations
> import Analysis.Syntax
> import Analysis.Types

> import Helpers
> import Traverse

> import Transformation.Syntax -- <- for doing reassociation
> 
> import qualified Data.Map.Lazy as Map hiding (map, (\\))

- when travesing whole program collect all declarations with bounds 
- collect all constants (#1) 
- identify all loop 'variables' (#2) 
   - identify all variables indexed by the loop variables

 loopBody :: Fortran t -> State (TypeEnvStack t) (Fortran ([String], [String], [String]))
 loopBody (For _ v@(VarName _ s) e1 e2 e3 body) = 
     let
         anno = (
     in For anno v e1 e2 e3 body
  
 newFrame gammas = []:gammas
 pushVar v t (g:gs) = ((v, t):g):gs
 popVar (((v,t):g):gs) = (g:gs)
 popFrame (g:gs) = (g, gs)

map (fmap ((,[""]),[""]))

> loopAnalyse :: Program a -> Program Annotation
> loopAnalyse p = map ((descendBi arrayIndices) . ix . lva' . (transformBi reassociate) . (fmap (const unitAnnotation))) p

> analyse' :: Program Annotation -> Program Annotation
> analyse' p = map ((descendBi arrayIndices) . ix . lva' . (transformBi reassociate))  p


> collect :: (Eq a, Ord k) => [(k, a)] -> Map.Map k [a]
> collect = collect' Map.empty 
>           where collect' as []                         = as
>                 collect' as ((v, n):es) | Map.member v as = collect' (Map.insert v (nub $ n : ((Map.!) as v)) as) es
>                                         | otherwise   = collect' (Map.insert v [n] as) es

> arrayIndices :: Block Annotation -> Block Annotation
> arrayIndices x = 
>     let tenv = typeEnv x
>         
>         arrIxsF :: Fortran Annotation -> Annotation
>         arrIxsF y = let readIxs = [(v, mfmap (const ()) e) | 
>                                      (Var _ _ [(VarName _ v, e)]) <- rhsExpr y,
>                                      length e > 0,
>                                      isArrayTypeP' tenv v]

>                         writeIxs = [(v, mfmap (const ()) e) |
>                                      (Var _ _ [(VarName _ v, e)]) <- lhsExpr y,
>                                      length e > 0,
>                                      isArrayTypeP' tenv v]

>                     in (tag y) { arrsRead = (collect readIxs), arrsWrite = (collect writeIxs) } 
>     in extendBi arrIxsF x               

> ix :: ProgUnit Annotation -> ProgUnit Annotation
> ix = let ixF :: Fortran Annotation -> Annotation
>          ixF f = (tag f) { indices = (nub [v | (For _ _ (VarName _ v) _ _ _ _) <- ((universeBi f)::[Fortran Annotation])])}
>      in extendBi ixF

