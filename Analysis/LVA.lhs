> {-# LANGUAGE ScopedTypeVariables #-}

> module Analysis.LVA where
>     
> import Data.Data
> import Data.List

> import Data.Generics.Zipper
> import Data.Generics.Uniplate.Operations

> import Language.Fortran

> import Analysis.Annotations
> import Analysis.Syntax
> import Transformation.Syntax
> import Analysis.IntermediateReps
> import Traverse

Kill/gen functions

> genForLhsVar, killForLhsVar :: Expr Annotation -> [Access]
> killForLhsVar (Var a p xes) = map (\((VarName _ v), _) -> VarA v) xes
> killForLhsVar _            = []
>                                  
> genForLhsVar t@ (Var _ _  xes) = concatMap (\(_, es) -> accesses es) xes
> genForLhsVar _            = []

> kill :: Fortran Annotation -> [Access]
> kill (Assg _ _ e1 _) = killForLhsVar e1 
> kill t = concatMap accesses (lhsExpr t)

> gen :: Fortran Annotation -> [Access]
> gen t@(Assg _ _ e1 e2) = (concatMap accesses (rhsExpr t)) ++ (genForLhsVar e1)
> gen t = concatMap accesses (rhsExpr t)            

Single iteration of live-variable analysis

> lva0 :: Zipper (Program Annotation) -> Zipper (Program Annotation)
> lva0 z = case (getHole z)::(Maybe (Fortran Annotation)) of
>             Just f ->  setHole (refill f (lvaBody z f)) z
>             Nothing -> z

> lvaBody :: Zipper (Program Annotation) -> Fortran Annotation -> Annotation
> lvaBody z e = 
>               let anns =  map copoint ((successors z)::[Fortran Annotation])
>                   liveOut = nub $ concat $ map (fst . lives) anns
>                   killV = kill e
>                   genV  = gen e
>                   liveIn = nub $ union genV (liveOut \\ killV)

>               in (copoint e) { lives = (liveIn, liveOut), successorStmts = map number anns }

Iterate the contextual computation of lva0 over a block, till a fixed-point is reached

> lva :: Program Annotation -> Program Annotation
> lva x = lva' (numberStmts . (transformBi reassociate) $ x)

> lva':: Program Annotation -> Program Annotation
> lva' x = let y = fromZipper . (everywhere lva0) . toZipper $ x
>          in if (y == x) then y else lva' y


 successorAnnotations :: Zipper (Program Annotation) -> [Annotation]
 successorAnnotations x = goRight x ++ (case (up x) of
                                          Just ux -> case (getHole ux)::(Maybe (Fortran Annotation)) of
                                                       Just f -> map copoint (successors f) ++ (goRight ux)
                                                       Nothing -> (goRight ux)
                                          Nothing -> []) 
                           where goRight :: Zipper (Program Annotation) -> [Annotation]
                                 goRight z = (case (getHole z)::(Maybe (Fortran Annotation)) of 
                                                Just f -> [copoint f]
                                                Nothing -> []) ++
                                             (case (right z) of
                                                Just rz -> goRight rz
                                                Nothing -> [])
               
                 