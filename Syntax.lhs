> {-# LANGUAGE ScopedTypeVariables #-}

> module Syntax where

> import Data.Data
> import Data.List
> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy

> import Annotations
> import Language.Fortran


Number

> numberStmts :: Program Annotation -> Program Annotation
> numberStmts x = let 
>                   numberF :: Fortran Annotation -> State Int (Fortran Annotation)
>                   numberF = descendBiM number'

>                   number' :: Annotation -> State Int Annotation
>                   number' x = do n <- get 
>                                  put (n + 1)
>                                  return $ setNumber n x 
>          
>                 in fst $ runState (descendBiM numberF x) 0

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

> affineMatch (Bin _ (Plus _) (Var _ [(VarName _ v, _)]) (Con _ n))   = Just (v, read n)
> affineMatch (Bin _ (Plus _) (Con _ n) (Var _ [(VarName _ v, _)]))   = Just (v, read n)
> affineMatch (Bin _ (Minus _) (Var _ [(VarName _ v, _)]) (Con _ n))  = Just (v, - read n)
> affineMatch (Bin _ (Minus _) (Con _ n) (Var _  [(VarName _ v, _)])) = Just (v, - read n)
> affineMatch (Var _  [(VarName _ v, _)])                             = Just (v, 0)
> affineMatch _                                                       = Nothing


 indexVariables :: Program Annotation -> Program Annotation
 indexVariables = descendBi indexVariables'

 indexVariables' :: Block Annotation -> Block Annotation
 indexVariables' x = 
     let typeEnv = snd $ runState (buildTypeEnv x) []

         indexVars :: Fortran Annotation -> Annotation
         indexVars y = let is = [e | (Var _ [(VarName _ v, e)]) <- (universeBi y)::[Expr Annotation], length e > 0, isArrayTypeP' typeEnv v]
                           indices = [v | (VarName _ v) <- (universeBi is)::[VarName Annotation]]
                       in setIndices (nub indices) (extract y) 
     in extendBi indexVars x