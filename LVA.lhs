> {-# LANGUAGE ScopedTypeVariables #-}

> module LVA where
>     
> import Data.Data
> import Data.List

> import Data.Generics.Zipper
> import Data.Generics.Uniplate.Operations

> import Language.Fortran

> import Annotations
> import Syntax
> import Traverse

Kill/gen functions

> kill :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [Variable]
> kill t =    [v | (AssgExpr _ _ v _) <- (universeBi t)::[Expr a]] 
>          ++ [v | (For _ _ (VarName _ v) _ _ _ _) <- (universeBi t)::[Fortran a]] 
>          ++ [v | (Assg _ _ (Var _ _ [(VarName _ v, _)]) _) <- (universeBi t)::[Fortran a]] 


> gen :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [Variable]
> gen t = (variables t) \\ (kill t)

Single iteration of live-variable analysis

> lva0 :: Fortran Annotation -> Annotation
> -- lva0 x = (universeBi (foldl union [] (successors x)))::[String]
> lva0 x = let liveOut = concat $ map (fst . lives . copoint) (successors x)
>              killV   = kill x
>              genV    = gen x
>              liveIn  = union genV (liveOut \\ killV)
>          in (copoint x) { lives = (liveIn, liveOut), successorStmts = (map (number . copoint) (successors x)) }
               

 lva0P :: Zipper (Program Annotation) -> Annotation
 -- lva0 x = (universeBi (foldl union [] (successors x)))::[String]
 lva0P x = case ((getHole x)::(Maybe (Fortran Annotation)) of

            

let liveOut = concat $ map (fst . lives . copoint) (successors x)
>              killV   = kill x
>              genV    = gen x
>              liveIn  = union genV (liveOut \\ killV)
>          in (copoint x) { lives = (liveIn, liveOut), successorStmts = (map (number . copoint) (successors x)) }
               


Iterate the comonadic application of lva0 over a block, till a fixed-point is reached

> lva :: Program Annotation -> Program Annotation
> lva x = let y = extendBi lva0 x
>         in if (y == x) then x
>            else lva y

Apply live-variable analysis over all blocks

 lva :: [Program Annotation] -> [Program Annotation]
 lva = map lvaN . (map (fmap (const [""])))

> successorsAnnotations :: Zipper (Fortran Annotation) -> [Annotation]
> successorsAnnotations x = goRight x ++ (case (up x) of
>                                          Just ux -> case (getHole ux)::(Maybe (Fortran Annotation)) of
>                                                       Just f -> map copoint (successors f)
>                                                       Nothing -> []
>                                          Nothing -> [])
>                             

> goRight :: Zipper (Fortran Annotation) -> [Annotation]
> goRight z = case (right z) of
>               Just rz -> case (getHole z)::(Maybe (Fortran Annotation)) of 
>                             Just f -> copoint f : (goRight rz)
>                             Nothing -> goRight rz
>               Nothing -> []
>               
>                 