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

> import LVA
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

    
> type Annotation = ([Variable], [Variable])
> unitAnnotation = ([], [])

> indices :: [Program [Variable]] -> [Program Annotation] -- State (TypeEnv a) 
> indices = let indexVariablesF = indexVariables :: Fortran Annotation -> [Variable] 
>           in map (extendBi ((fst . rextract) `fanout` indexVariablesF)) . (map (fmap (,[""])))

> analyse :: [Program ()] -> [Program Annotation]
> analyse p = indices . lva $ p


> indexVariables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [Variable]
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
>           let f'' = analyse f'
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           -- putStrLn (show $ variables f'')
>           -- putStrLn (show $ binders f'')
>           (show ((map (fmap (const ())) f')::([Program ()]))) `trace`
>           --(show (map gtypes f'')) `trace`
>             (show (declsWithBounds f'')) `trace` return ()
>           -- (show [ d | d@(Decl _ p t) <- (universeBi (map (fmap (const ())) f''))::[Decl ()]]) `trace` return ()

 