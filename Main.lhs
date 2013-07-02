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

> import System.Directory
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
> import CommonBlocks
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

> import Data.List (nub, (\\), elemIndices)
> import qualified Data.Map.Lazy as Map hiding (map, (\\))
> import Data.Text hiding (length, head, concatMap, map)


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

> analyse :: [Program a] -> [Program Annotation]
> analyse p = map ((descendBi arrayIndices) . ix . lva . numberStmts . (transformBi reassociate) . (fmap (const unitAnnotation))) p

> analyse' :: [Program Annotation] -> [Program Annotation]
> analyse' p = map ((descendBi arrayIndices) . ix . lva . numberStmts . (transformBi reassociate))  p


> collect :: (Eq a, Ord k) => [(k, a)] -> Map.Map k [a]
> collect = collect' Map.empty 
>           where collect' as []                         = as
>                 collect' as ((v, n):es) | Map.member v as = collect' (Map.insert v (nub $ n : ((Map.!) as v)) as) es
>                                         | otherwise   = collect' (Map.insert v [n] as) es

> arrayIndices :: Block Annotation -> Block Annotation
> arrayIndices x = 
>     let typeEnv = snd $ runState (buildTypeEnv x) []
>         
>         arrIxsF :: Fortran Annotation -> Annotation
>         arrIxsF y = let readIxs = [(v, mfmap (const ()) e) | 
>                                      (Var _ _ [(VarName _ v, e)]) <- rhsExpr y,
>                                      length e > 0,
>                                      isArrayTypeP' typeEnv v]

>                         writeIxs = [(v, mfmap (const ()) e) |
>                                      (Var _ _ [(VarName _ v, e)]) <- lhsExpr y,
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

> goR :: String -> IO ()
> goR s = do f' <- pr s
>            putStrLn $ show f' -- show (transformBi reassociate f')

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
>           -- putStrLn $ show f''
>           -- (show ((map (fmap (const ())) (descendBi reassociate f'))::([Program ()]))) `trace` return ()

 go2 :: String -> IO String

A sample transformation

> fooTrans p = transformBi f p
>                 where f :: Fortran A1 -> Fortran A1
>                       f p@(Call x sp e as) = Label True sp "10" p
>                       f p@(Assg x sp e1 e2) = Label True sp "5" p
>                       f p = p


> go2 f = do inp <- readFile f
>            p <- pr f
>            let p' = fooTrans $ (map (fmap (const unitAnnotation)) p)
>            let out = reprint inp f p'
>            writeFile (f ++ ".out") out
>            return $ (out, p')

> go3 f = do inp <- readFile f
>            p <- pr f
>            let (r, p') = (refactorEquivalences (map (fmap (const unitAnnotation)) p)) 
>            let out = reprint inp f p'
>            let pa' = analyse' p'
>            writeFile (f ++ ".out.html") (concatMap outputHTML pa')
>            writeFile (f ++ ".out") out
>            let (r2, p'') = (deadCode True pa')
>            let out' = reprint inp f p''
>            writeFile (f ++ ".2.out") out'
>            putStrLn $ r ++ r2

> data ArgType = Named String | NamedList String 

 ensureArgs [] _        = return []
 ensureArgs ((Named n :ns) args = 

> introMessage = "CamFort 0.1 - Cambridge Fortran Infrastructure."
> usage = "Usage: camfort <function> <directory> \n\n"
> menu = "Refactor functions: \n \t common \t [refactor common blocks] \n " ++
>                " \t equivalence \t [refactor equivalences] \n" ++
>                " \t dead \t\t [dead-code eliminate] \n" ++ "\n" ++
>        "Analysis functions: \n \t lva \t\t [live-variable analysis] \n " ++
>                           "\t loops \t\t [analyse loops] \n\n"

> main = do putStrLn $ introMessage 
>           d <- getArgs 
>           if (length d == 2) then
>              let (func:(dir:_)) = d
>              in case func of
>                    "common" -> common dir
>                    "equivalence" -> return ()
>                    "dead" -> return ()
>                    "lva" -> return ()
>                    "loops" -> return ()
>                    _ -> putStrLn $ usage ++ menu
>           else
>              putStrLn $ usage ++ menu

> readParseSrc f = do putStrLn f 
>                     inp <- readFile f
>                     ast <- pr f
>                     return $ (f, inp, map (fmap (const unitAnnotation)) ast)

         
> common d = do putStrLn $ "Refactoring common blocks for source in directory " ++ show d ++ "\n"
>               putStrLn $ "Exclude any files from " ++ d ++ "/? (comma-separate list)\n"
>               excludes <- getLine
>               dirF <- rGetDirectoryContents d
>               files <- return $ dirF \\ ((map unpack (split (==',') (pack excludes))))
>               files' <- return $ map (\y -> d ++ "/" ++ y) files
>               ps <- mapM readParseSrc files'
>               let (report, asts') = commonElim (map (\(f, inp, ast) -> (f, ast)) ps)
>               putStrLn report
>               mapM (\(ast', (f, inp, _)) -> writeFile (f ++ ".out") (reprint inp f ast')) (Prelude.zip asts' ps)
>               return ()

> rGetDirectoryContents d = do ds <- getDirectoryContents d
>                              ds' <- return $ ds \\ [".", ".."] -- remove '.' and '..' entries
>                              rec ds'
>                              where 
>                                rec []     = return $ []
>                                rec (x:xs) = do xs' <- rec xs
>                                                g <- doesDirectoryExist (d ++ "/" ++ x)
>                                                if g then 
>                                                   do x' <- rGetDirectoryContents (d ++ "/" ++ x)
>                                                      return $ (map (\y -> x ++ "/" ++ y) x') ++ xs'
>                                                else if (isFortran x) then
>                                                         return $ x : xs'
>                                                     else return $ xs'
>                                                   
> isFortran x = let ix = elemIndices '.' x
>               in if (length ix == 0) then False
>                  else case (Prelude.drop (Prelude.last ix) x) of 
>                         ".f" -> True
>                         ".f90" -> True
>                         ".f77" -> True
>                         _     -> False
>               
>            

