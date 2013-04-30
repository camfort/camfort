> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE TupleSections #-}

> {-# LANGUAGE ImplicitParams #-}

> module Main where

> import Language.Fortran.Parser
> import Language.Fortran
> import System.Environment
> import System.IO

> import Data.Generics.Uniplate.Operations

> import Control.Comonad
> import Control.Monad
> import Control.Monad.State.Lazy

> import Annotations
> import LVA
> import Output
> import Syntax
> import Traverse
> import Types

> import Debug.Trace

> import Data.Data
> import qualified Data.Maybe as MaybeList
> import Data.Typeable

> import Data.List (nub)

> import Data.Map.Lazy hiding (map)

> main :: IO ()
> main = do return ()

> quickAnnotateDo :: Fortran String -> Fortran String
> quickAnnotateDo (For _ v@(VarName _ s) e1 e2 e3 body) = For s v e1 e2 e3 body
> quickAnnotateDo t = t

> annotateFVDo :: Fortran ([String], [String]) -> Fortran ([String], [String])
> annotateFVDo (For _ v@(VarName _ s) e1 e2 e3 body) = For anno v e1 e2 e3 body
>   where anno = ([""], freeVariables body) -- \\ [s] -- indexVariables body
> annotateFVDo t = t

- when travesing whole program collect all declarations with bounds 
- collect all constants (#1) 
- identify all loop 'variables' (#2) 
   - identify all variables indexed by the loop variables

 loopBody :: Fortran t -> State (TypeEnvStack t) (Fortran ([String], [String], [String]))
 loopBody (For _ v@(VarName _ s) e1 e2 e3 body) = 
     let
         anno = (
     in For anno v e1 e2 e3 body
  
> newFrame gammas = []:gammas
> pushVar v t (g:gs) = ((v, t):g):gs
> popVar (((v,t):g):gs) = (g:gs)
> popFrame (g:gs) = (g, gs)

 type Annotation = (((), [Variable]), [Variable])
 unitAnnotation = (((), []), [])

map (fmap ((,[""]),[""]))

> analyse :: [Program ()] -> [Program Annotation]
> analyse p = map ((descendBi writeArr) . (descendBi readArr) . indexVariables . lva . (fmap (const unitAnnotation))) p


> affineMatch (Bin _ (Plus _) (Var _ [(VarName _ v, _)]) (Con _ n))   = Just (v, read n)
> affineMatch (Bin _ (Plus _) (Con _ n) (Var _ [(VarName _ v, _)]))   = Just (v, read n)
> affineMatch (Bin _ (Minus _) (Var _ [(VarName _ v, _)]) (Con _ n))  = Just (v, - read n)
> affineMatch (Bin _ (Minus _) (Con _ n) (Var _  [(VarName _ v, _)])) = Just (v, - read n)
> affineMatch (Var _  [(VarName _ v, _)])                             = Just (v, 0)
> affineMatch _                                                       = Nothing

> collect :: (Eq a, Ord k) => [(k, a)] -> Map k [a]
> collect = collect' empty 
>           where collect' as []                         = as
>                 collect' as ((v, n):es) | member v as = collect' (insert v (n : as!v) as) es
>                                         | otherwise   = collect' (insert v [n] as) es

> readArr :: Block Annotation -> Block Annotation
> readArr x = 
>     let typeEnv = snd $ runState (buildTypeEnv x) []
>         
>         readVars :: Fortran Annotation -> Annotation
>         readVars y = let vs = [(v, nub (MaybeList.mapMaybe affineMatch e)) | (Var _ [(VarName _ v, e)]) <- lhsExpr y, length e > 0, isArrayTypeP' typeEnv v]
>                       in setArrsRead (collect $ nub vs) (extract y) 
>     in extendBi readVars x     

> writeArr :: Block Annotation -> Block Annotation
> writeArr x = 
>     let typeEnv = snd $ runState (buildTypeEnv x) []
>         
>         writeVars :: Fortran Annotation -> Annotation
>         writeVars y = let vs = [(v, nub (MaybeList.mapMaybe affineMatch e)) | (Var _ [(VarName _ v, e)]) <- rhsExpr y, length e > 0, isArrayTypeP' typeEnv v]
>                       in setArrsWrite (collect $ nub vs) (extract y) 
>     in extendBi writeVars x                                    

(universeBi y)::[Expr Annotation]

 fi :: Fortran Annotation -> [Expr Annotation]
 fi t@(Assg _ e1 e2) = (universeBi e2)::[Expr Annotation]
 fi t = (concatMap fi ((children t)::[Fortran Annotation])) ++ ((childrenBi t) :: [Expr Annotation])

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

> rhsExpr (Assg x e1 e2)        = (universeBi e1)::[Expr Annotation]
> rhsExpr (For x v e1 e2 e3 fs) = rhsExpr fs
> rhsExpr (FSeq x f1 f2)        = rhsExpr f1 ++ rhsExpr f2
> rhsExpr (If x e f1 fes f3)    = (rhsExpr f1) ++ 
>                              (concatMap (\(e, f) -> rhsExpr f) fes) ++
>                               (case f3 of 
>                                  Nothing -> []
>                                  Just x -> rhsExpr x)
>                             
> rhsExpr (Forall x (es, e) f)  = rhsExpr f -- TODO: maybe different here
> rhsExpr (Where x e f)         = rhsExpr f
> rhsExpr (Label x s f)         = rhsExpr f
> rhsExpr t                     = [] -- concatMap rhsExpr ((universeBi t)::[Fortran Annotation])


 readArrays :: Block Annotation -> Block Annotation
 readArrays x = 
     let typeEnv = snd $ runState (buildTypeEnv x) []
         
         readVars :: Fortran Annotation -> Annotation
         readVars y = let vs = [(v, [iv | (VarName _ iv) <- (universeBi e)::[VarName Annotation]]) | (Var _ [(VarName _ v, e)])<- (universeBi y)::[Expr Annotation], length e > 0, isArrayTypeP' typeEnv v]
                       in setArrsRead (nub vs) (extract y) 
     in extendBi readVars x> readArrays :: Block Annotation -> Block Annotation

> indexVariables :: Program Annotation -> Program Annotation
> indexVariables = descendBi indexVariables'

> indexVariables' :: Block Annotation -> Block Annotation
> indexVariables' x = 
>     let typeEnv = snd $ runState (buildTypeEnv x) []

>         indexVars :: Fortran Annotation -> Annotation
>         indexVars y = let is = [e | (Var _ [(VarName _ v, e)]) <- (universeBi y)::[Expr Annotation], length e > 0, isArrayTypeP' typeEnv v]
>                           indices = [v | (VarName _ v) <- (universeBi is)::[VarName Annotation]]
>                       in setIndices (nub indices) (extract y) 
>     in extendBi indexVars x

> go :: String -> IO ()
> go s = do f <- readFile s
>           let f' = parse f
>           --let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           --let f'' = map ((transformBi annotateFVDo) . (fmap (const ([""],[""])))) f'
>           let f'' = analyse f'
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           -- putStrLn (show $ variables f'')
>           -- putStrLn (show $ binders f'')
>           (show ((map (fmap (const ())) f')::([Program ()]))) `trace` return ()


 