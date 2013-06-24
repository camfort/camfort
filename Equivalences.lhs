> {-# LANGUAGE ScopedTypeVariables #-}

> module Equivalences where

> import Data.Data
> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy

> import Language.Fortran

> import Annotations
> import Syntax
> import Traverse

> collectEquivalences :: (Block Annotation) -> State [[Expr Annotation]] (Block Annotation)
> collectEquivalences = descendBiM cequiv

> cequiv ::  Fortran Annotation -> State [[Expr Annotation]] (Fortran Annotation)
> cequiv f@(Equivalence _ _ equivs) = (modify (equivs:)) >> (return f)
> cequiv f = return f

> inGroup :: Eq a => a -> [[a]] -> [a]
> inGroup x [] = []
> inGroup x (xs:xss) = if (x `elem` xs) then xs
>                      else inGroup x xss
                                      
> equivalents :: Expr Annotation -> State [[Expr Annotation]] [Expr Annotation]
> equivalents x = do equivs <- get 
>                    return (inGroup x equivs)

> refactorEquivalences :: [Program Annotation] -> [Program Annotation]
> refactorEquivalences = map (transformBi equivalences)
>                         where equivalences :: Block Annotation -> Block Annotation
>                               equivalences b = evalState (do collectEquivalences b
>                                                              transformBiM rewrites b) []
>                               rewrites :: Fortran Annotation -> State [[Expr Annotation]] (Fortran Annotation)
>                               rewrites x@(Assg p s e1 e2) = let p' = setRefactored p
>                                                                 x' = Assg p' s e1 e2
>                                                             in do eqs <- equivalents e1
>                                                                   return $ foldl (\f -> \e' -> FSeq p' s f (Assg p' s e' e2)) x' eqs
>                               rewrites x = return x 
