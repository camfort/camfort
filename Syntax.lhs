> {-# LANGUAGE ScopedTypeVariables #-}

> module Syntax where

> import Data.Data
> import Data.List

> import Annotations

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


 fi :: Fortran Annotation -> [Expr Annotation]
 fi t@(Assg _ e1 e2) = (universeBi e2)::[Expr Annotation]
 fi t = (concatMap fi ((children t)::[Fortran Annotation])) ++ ((childrenBi t) :: [Expr Annotation])

> lhsExpr :: Fortran Annotation -> [Expr Annotation]
> lhsExpr (Assg x e1 e2)        = (universeBi e2)::[Expr Annotation]
> lhsExpr (For x v e1 e2 e3 fs) = ((universeBi e1)::[Expr Annotation]) ++
>                              ((universeBi e2)::[Expr Annotation]) ++
>                              ((universeBi e3)::[Expr Annotation]) ++ 
>                             lhsExpr fs
> lhsExpr (FSeq x f1 f2)        = lhsExpr f1 ++ lhsExpr f2
> lhsExpr (If x e f1 fes f3)    = ((universeBi e)::[Expr Annotation]) ++
>                              (lhsExpr f1) ++ 
>                              (concatMap (\(e, f) -> ((universeBi e)::[Expr Annotation]) ++ lhsExpr f) fes) ++
>                               (case f3 of 
>                                  Nothing -> []
>                                  Just x -> lhsExpr x)
>                             
> lhsExpr (Allocate x e1 e2)    = ((universeBi e1)::[Expr Annotation]) ++
>                              ((universeBi e2)::[Expr Annotation])
> lhsExpr (Call x e as)         = (universeBi e)::[Expr Annotation] 
> lhsExpr (Deallocate x es e)   = (concatMap (\e -> (universeBi e)::[Expr Annotation]) es) ++
>                              ((universeBi e)::[Expr Annotation])
> lhsExpr (Forall x (es, e) f)  = concatMap (\(_, e1, e2, e3) -> -- TODO: maybe different here
>                                            ((universeBi e1)::[Expr Annotation]) ++
>                                             ((universeBi e2)::[Expr Annotation]) ++
>                                             ((universeBi e3)::[Expr Annotation])) es ++
>                             ((universeBi e)::[Expr Annotation]) ++
>                             lhsExpr f 
> lhsExpr (Nullify x es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (Inquire x s es)      = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (Stop x e)            = (universeBi e)::[Expr Annotation]
> lhsExpr (Where x e f)         = ((universeBi e)::[Expr Annotation]) ++ lhsExpr f
> lhsExpr (Write x s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (PointerAssg x e1 e2) = ((universeBi e1)::[Expr Annotation]) ++
>                             ((universeBi e2)::[Expr Annotation])
> lhsExpr (Return x e)          = (universeBi e)::[Expr Annotation]
> lhsExpr (Label x s f)         = lhsExpr f
> lhsExpr (Print x e es)        = ((universeBi e)::[Expr Annotation]) ++ 
>                              (concatMap (\e -> (universeBi e)::[Expr Annotation]) es)
> lhsExpr (ReadS x s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr _                     = []

> rhsExpr :: Fortran Annotation -> [Expr Annotation]
> rhsExpr (Assg x e1 e2)        = (universeBi e1)::[Expr Annotation]
> rhsExpr t                     = concatMap rhsExpr ((children t)::[Fortran Annotation])

 rhsExpr (For x v e1 e2 e3 fs) = rhsExpr fs
 rhsExpr (FSeq x f1 f2)        = rhsExpr f1 ++ rhsExpr f2
 rhsExpr (If x e f1 fes f3)    = (rhsExpr f1) ++ 
                              (concatMap (\(e, f) -> rhsExpr f) fes) ++
                               (case f3 of 
                                  Nothing -> []
                                  Just x -> rhsExpr x)
                             
 rhsExpr (Forall x (es, e) f)  = rhsExpr f -- TODO: maybe different here
 rhsExpr (Where x e f)         = rhsExpr f
 rhsExpr (Label x s f)         = rhsExpr f
 rhsExpr t                     = [] -- concatMap rhsExpr ((universeBi t)::[Fortran Annotation])
