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

> -- import Language.Haskell.Parser
> import Language.Haskell.ParseMonad

> import Data.Generics.Str
> import Data.Generics.Uniplate.Operations

> import Control.Comonad
> import Control.Monad
> import Control.Monad.State.Lazy

> import Annotations
> import DeadCode 
> import Equivalences
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
> quickAnnotateDo (For _ sp v@(VarName _ s) e1 e2 e3 body) = For s sp v e1 e2 e3 body
> quickAnnotateDo t = t

> annotateFVDo :: Fortran ([String], [String]) -> Fortran ([String], [String])
> annotateFVDo (For _ sp v@(VarName _ s) e1 e2 e3 body) = For anno sp v e1 e2 e3 body
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

> analyse :: [Program A0] -> [Program Annotation]
> analyse p = map ((descendBi arrayIndices) . ix . lva . numberStmts . (fmap (const unitAnnotation))) p


> collect :: (Eq a, Ord k) => [(k, a)] -> Map k [a]
> collect = collect' empty 
>           where collect' as []                         = as
>                 collect' as ((v, n):es) | member v as = collect' (insert v (nub $ n : as!v) as) es
>                                         | otherwise   = collect' (insert v [n] as) es

> arrayIndices :: Block Annotation -> Block Annotation
> arrayIndices x = 
>     let typeEnv = snd $ runState (buildTypeEnv x) []
>         
>         arrIxsF :: Fortran Annotation -> Annotation
>         arrIxsF y = let readIxs = [(v, mfmap (const ()) e) | 
>                                      (Var _ _ [(VarName _ v, e)]) <- lhsExpr y,
>                                      length e > 0,
>                                      isArrayTypeP' typeEnv v]

>                         writeIxs = [(v, mfmap (const ()) e) |
>                                      (Var _ _ [(VarName _ v, e)]) <- rhsExpr y,
>                                      length e > 0,
>                                      isArrayTypeP' typeEnv v]

>                     in (copoint y) { arrsRead = (collect readIxs), arrsWrite = (collect writeIxs) } 
>     in extendBi arrIxsF x               

> ix :: Program Annotation -> Program Annotation
> ix = let ixF :: Fortran Annotation -> Annotation
>          ixF f = (copoint f) { indices = (nub [v | (For _ _ (VarName _ v) _ _ _ _) <- ((universeBi f)::[Fortran Annotation])])}
>      in extendBi ixF


> pr  :: String -> IO [Program A0]
> pr f = let mode = ParseMode { parseFilename = f }
>        in do inp <- readFile f
>              case (runParserWithMode mode parser inp) of
>                (ParseOk p)       -> return $ p
>                (ParseFailed l e) -> error e

> go :: String -> IO ()
> go s = do -- f <- readFile s
>           -- let f' = parse f
>           f' <- pr s
>           --let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           --let f'' = map ((transformBi annotateFVDo) . (fmap (const ([""],[""])))) f'
>           let f'' = analyse f'
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           -- putStrLn (show $ variables f'')
>           -- putStrLn (show $ binders f'')
>           (show ((map (fmap (const ())) (descendBi reassociate f'))::([Program ()]))) `trace` return ()

 go2 :: String -> IO String

A sample transformation

> fooTrans p = transformBi f p
>                 where f :: Fortran A1 -> Fortran A1
>                       f p@(Call x sp e as) = Label True sp "10" p
>                       f p@(Assg x sp e1 e2) = Label True sp "5" p
>                       f p = p


> go2 f = do inp <- readFile f
>            p <- pr f
>            let p' = fooTrans $ (fmap (const unitAnnotation) (head p))
>            let out = reprint inp f p'
>            writeFile (f ++ ".out") out
>            return $ (out, p')

> go3 f = do inp <- readFile f
>            p <- pr f
>            let (r, p') = refactorEquivalences (map (fmap (const unitAnnotation)) p)
>            let out = reprint inp f (head p')
>            writeFile (f ++ ".out") out
>            putStrLn r
>            
>            

