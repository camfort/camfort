> {-# LANGUAGE ScopedTypeVariables #-}

> module LVA where
>     
> import Data.Data
> import Data.List

> import Data.Generics.Uniplate.Operations

> import Language.Fortran
> import Syntax
> import Traverse

Kill/gen functions

> kill :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [Variable]
> kill t =    [v | (AssgExpr _ v _) <- (universeBi t)::[Expr a]] 
>          ++ [v | (For _ (VarName _ v) _ _ _ _) <- (universeBi t)::[Fortran a]] 
>          ++ [v | (Assg _ (Var _ [(VarName _ v, _)]) _) <- (universeBi t)::[Fortran a]] 

> gen :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [Variable]
> gen t = (variables t) \\ (kill t)

Single iteration of live-variable analysis

> lva0 :: Fortran [Variable] -> [Variable]
> -- lva0 x = (universeBi (foldl union [] (successors x)))::[String]
> lva0 x = let liveOut = concat $ map rextract (successors x)
>              killV   = kill x
>              genV    = gen x
>              liveIn  = union genV (liveOut \\ killV)
>          in liveIn 
               
Iterate the comonadic application of lva0 over a block, till a fixed-point is reached

> lvaN :: Program [Variable] -> Program [Variable]
> lvaN x = let y = extendBi lva0 x
>          in if (y == x) then x
>             else lvaN y

Apply live-variable analysis over all blocks

> lva :: [Program a] -> [Program [Variable]]
> lva = map lvaN . (map (fmap (const [""])))