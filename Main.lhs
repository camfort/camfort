> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE ImplicitParams #-}

> module Main where

> import Language.Fortran.Parser
> import Language.Fortran
> import System.Environment
> import System.IO

> import Data.Generics.Uniplate.Operations

> import Control.Comonad

> import Output
> import Syntax
> import Traverse
> import Types

> import Debug.Trace

> import Data.Data
> import Data.Typeable
> import Data.List


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

> kill :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [String]
> kill t =    [v | (AssgExpr _ v _) <- (universeBi t)::[Expr a]] 
>          ++ [v | (For _ (VarName _ v) _ _ _ _) <- (universeBi t)::[Fortran a]] 
>          ++ [v | (Assg _ (Var _ [(VarName _ v, _)]) _) <- (universeBi t)::[Fortran a]] 

> gen :: forall a t . (Data (t a), Typeable (t a), Typeable a, Data a)
>                   => t a -> [String]
> gen t = (variables t) \\ (kill t)

> successors :: Fortran t -> [Fortran t]
> successors (FSeq _ f1 f2)           = f2 : successors f1
> successors (For _ _ _ _ _ f)        = [f]
> successors (If _ _ f efs Nothing)   = f : map snd efs
> successors (If _ _ f efs (Just f')) = [f, f'] ++ map snd efs
> successors (Forall _ _ f)           = [f]
> successors (Where _ _ f)            = [f]
> successors (Label _ _ f)            = [f]
> successors _                        = []       

 type Annotation = ([String], ([String], [String]))
 unitAnnotation = ([], ([], []))

> type Annotation = ([String], [String])
> unitAnnotation = ([], [])

Single iteration of live-variable analysis

> lva0 :: Fortran Annotation -> Annotation
> -- lva0 x = (universeBi (foldl union [] (successors x)))::[String]
> lva0 x = let liveOut = concat $ map (fst . rextract) (successors x)
>              killV   = kill x
>              genV    = gen x
>              liveIn  = union genV (liveOut \\ killV)
>          in (liveIn,  indexVariables x) -- ) (killV, genV))

> indexVariables2 :: forall a t . (Data (t Annotation), Typeable (t Annotation)) => t Annotation -> [String]
> indexVariables2 f = nub $ [ v |  (VarName _ v) <- (universeBi f')::[VarName Annotation] ]
>                        where f' = [ exprs | (ArrayCon _ exprs) <- ((universeBi f)::[Expr Annotation]) ]

                
Iterate the comonadic application of lva0 over a block, till a fixed-point is reached

> lvaN :: Program Annotation -> Program Annotation
> lvaN x = let y = extendBi lva0 x
>          in if (y == x) then x
>             else lvaN y

Apply live-variable analysis over all blocks

> lva :: [Program Annotation] -> [Program Annotation]
> lva = map lvaN
     

             ++ (concat [map fst es | Var es <- universeBi f])

> indexVariables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> indexVariables x = let is = [e | (Var _ [(VarName _ _, e)]) <- (universeBi x)::[Expr a], length e > 0]
>                    in nub [v | (VarName _ v) <- (universeBi is)::[VarName a]]

 indexVariables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
 indexVariables f = nub $ [ v |  (VarName _ v) <- (universeBi f')::[VarName a] ]
                        where f' = [ exprs | (ArrayCon _ exprs) <- ((universeBi f)::[Expr a]) ]

                              transformBiM

                                 (\(Decl  -> do xs <- get 

> predBounds [] = False
> predBounds [(Bound _ _ _)] = True
> predBounds ((Bound _ _ _):bs) = True || predBounds bs
> predBounds _ = False

> declsWithBounds :: forall a .  (Data a, Typeable a) => [Program a] -> [String]
> declsWithBounds x = [v | (VarName _ v, b) <- (universeBi ((universeBi x)::[Decl a]))::[(VarName a, [Expr a])], predBounds b]


> go :: String -> IO ()
> go s = do f <- readFile s
>           let f' = parse f
>           --let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           --let f'' = map ((transformBi annotateFVDo) . (fmap (const ([""],[""])))) f'
>           let f'' = lva (map (fmap (const unitAnnotation)) f')
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           -- putStrLn (show $ variables f'')
>           -- putStrLn (show $ binders f'')
>           --(show ((map (fmap (const ())) f')::([Program ()]))) `trace`
>           (show (map gtypes f'')) `trace`
>             (show (declsWithBounds f'')) `trace` return ()
>           -- (show [ d | d@(Decl _ p t) <- (universeBi (map (fmap (const ())) f''))::[Decl ()]]) `trace` return ()

 