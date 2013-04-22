> {-# LANGUAGE ScopedTypeVariables #-}

> module Syntax where

> import Data.Data
> import Data.List

> import Language.Fortran
> import Data.Generics.Uniplate.Operations

All variables from a Fortran syntax tree

> variables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> variables f = nub $
>                  [v | (AssgExpr _ v _) <- (universeBi f)::[Expr a]]
>               ++ [v | (VarName _ v) <- (universeBi f)::[VarName a]] 
               
Free-variables in a piece of Fortran syntax

> freeVariables :: (Data (t a), Data a) => t a -> [String]
> freeVariables f = (variables f) \\ (binders f)

All variables from binders

> binders :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> binders f = nub $
>                [v | (ArgName _ v) <- (universeBi f)::[ArgName a]] 
>             ++ [v | (VarName _ v) <- (universeBi ((universeBi f)::[Decl a]))::[VarName a]]
>             ++ [v | (For _ (VarName _ v) _ _ _ _) <- (universeBi f)::[Fortran a]]
