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

> import Output

> import Debug.Trace

> import Data.Data
> import Data.Typeable
> import Data.List

> import Control.Monad.State.Lazy

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
  

> type TypeEnv t = [(VarName t, Type t)]
> type TypeEnvStack t = [TypeEnv t] -- stack of environments
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

 liveVariableAnalysis :: Fortran [String] -> [String]
 liveVariableAnalysis x = (universeBi (foldl union [] (successors x)))::[String]

> liveVariableAnalysis x = 
>               let liveOut = foldl union [] ((universeBi (successors x))::[String])
>                   killV   = kill x
>                   genV    = gen x
>                   livein  = union genV (liveOut \\ killV)

     

> variables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> variables f = nub $
>                  [v | (AssgExpr _ v _) <- (universeBi f)::[Expr a]]
>               ++ [v | (VarName _ v) <- (universeBi f)::[VarName a]] 
               
             ++ (concat [map fst es | Var es <- universeBi f])

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

> binders :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> binders f = nub $
>                [v | (ArgName _ v) <- (universeBi f)::[ArgName a]] 
>             ++ [v | (VarName _ v) <- (universeBi ((universeBi f)::[Decl a]))::[VarName a]]
>             ++ [v | (For _ (VarName _ v) _ _ _ _) <- (universeBi f)::[Fortran a]]

> freeVariables :: (Data (t a), Data a) => t a -> [String]
> freeVariables f = (variables f) \\ (binders f)

> go :: String -> IO ()
> go s = do f <- readFile s
>           let f' = parse f
>           --let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           let f'' = map ((transformBi annotateFVDo) . (fmap (const ([""],[""])))) f'
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           -- putStrLn (show $ variables f'')
>           -- putStrLn (show $ binders f'')
>           (show ((map (fmap (const ())) f')::([Program ()]))) `trace`
>             (show (declsWithBounds f'')) `trace` return ()
>           -- (show [ d | d@(Decl _ p t) <- (universeBi (map (fmap (const ())) f''))::[Decl ()]]) `trace` return ()

 