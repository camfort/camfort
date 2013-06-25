> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE ImplicitParams #-}

> module Equivalences where

> import Data.Data
> import Data.List

> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy

> import Language.Fortran

> import Output
> import Language.Fortran.Pretty

> import Annotations
> import Syntax
> import Traverse

> import Debug.Trace

> type Report = String

> type RfEqState = ([[Expr Annotation]], Int, Report)

> refactorEquivalences :: [Program Annotation] -> (Report, [Program Annotation])
> refactorEquivalences = mapM (transformBiM equivalences)
>                         where equivalences :: Block Annotation -> (Report, Block Annotation)
>                               equivalences b = let (b', (_, _, r)) = runState ((rmEquivalences b) >>= (transformBiM rfAssgn)) ([], 0, "")
>                                                in (r, b')

> rfAssgn :: Fortran Annotation -> State RfEqState (Fortran Annotation)
> rfAssgn x@(Assg a sp@(s1, s2) e1 e2) | not (pRefactored a) =
>    do eqs <- equivalents e1
>       if (length eqs > 1) then 

>          let a' = setRefactored s1 a
>              sp' = refactorSpan sp
>              eqs' = deleteBy (\x -> \y -> (af x) == (af y)) e1 eqs -- remove self from list
>              
>              -- Reporting
>              (l, c) = srcLineCol s1
>              reportF (e', i) = "o" ++ show (l + i + 1, c) ++ ": added assignment: " ++ (outputF e') ++ " = " ++ outputF e1
>                                                ++ " due to refactored equivalence\n"
>              report n = let ?variant = Alt1 in concatMap reportF (zip eqs' [n..(n + length eqs')])

>          in do -- Update refactoring state
>                (equivs, n, r) <- get
>                put (equivs, n + (length eqs'), r ++ (report n))

>                -- Sequence original assignment with new assignments
>                return $ FSeq a sp x (foldl1 (FSeq a' sp') (map (\e' -> Assg a' sp' e' e1) eqs'))
>       else
>          return x
> rfAssgn x = return x 


> rmEquivalences :: (Block Annotation) -> State RfEqState (Block Annotation)
> rmEquivalences = transformBiM rmEquiv'
>                    where rmEquiv' ::  Fortran Annotation -> State RfEqState (Fortran Annotation)
>                          rmEquiv' f@(Equivalence a sp equivs) = 
>                                      do (ess, n, r) <- get
>                                         put (equivs:ess, n - 1, r ++ "i" ++ (show . srcLineCol . fst $ sp) ++ ": removed equivalence \n")
>                                         return (NullStmt (setRefactored (fst sp) a) (dropLine sp))
>                          rmEquiv' f = return f
                                     
> equivalents :: Expr Annotation -> State RfEqState [Expr Annotation]
> equivalents x = let inGroup x [] = []
>                     inGroup x (xs:xss) = if (AnnotationFree x `elem` (map AnnotationFree xs)) then xs
>                                          else inGroup x xss
>                 in do (equivs, _, _) <- get 
>                       return (inGroup x equivs)
