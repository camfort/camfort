> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE DoAndIfThenElse #-}

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

> import Analysis.Annotations

> import Transformation.DeadCode
> import Transformation.CommonBlockElim
> import Transformation.EquivalenceElim
> import Transformation.DerivedTypeIntro

> import Analysis.Types
> import Analysis.Loops
> import Analysis.LVA
> import Analysis.Syntax

> import Output
> import Traverse

> import Debug.Trace

> import Data.Data
> import Data.Typeable
> import qualified Data.Maybe as MaybeList

> import Data.List (nub, (\\), elemIndices)
> import Data.Text hiding (length, head, concatMap, map, filter, take, last)


> quickAnnotateDo :: Fortran String -> Fortran String
> quickAnnotateDo (For _ sp v@(VarName _ s) e1 e2 e3 body) = For s sp v e1 e2 e3 body
> quickAnnotateDo t = t

> annotateFVDo :: Fortran ([String], [String]) -> Fortran ([String], [String])
> annotateFVDo (For _ sp v@(VarName _ s) e1 e2 e3 body) = For anno sp v e1 e2 e3 body
>   where anno = ([""], freeVariables body) -- \\ [s] -- indexVariables body
> annotateFVDo t = t


> data ArgType = Named String | NamedList String 


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
>                    "equivalence" -> equivalences dir
>                    "dead" -> dead dir
>                    "lva" -> lvaA dir
>                    "loops" -> loops dir
>                    _ -> putStrLn $ usage ++ menu
>           else
>              putStrLn $ usage ++ menu

> typeStructuring d = doRefactor typeStruct d

> loops d =  do putStrLn $ "Analysing loops for source in directory " ++ show d ++ "\n"
>               doAnalysis loopAnalyse d

> lvaA d =  do putStrLn $ "Analysing loops for source in directory " ++ show d ++ "\n"
>              doAnalysis (map lva) d


> dead d = do putStrLn $ "Eliminating dead code for source in directory " ++ show d ++ "\n"
>             doRefactor ((mapM (deadCode False)) . (map snd)) d

> common d = do putStrLn $ "Refactoring common blocks for source in directory " ++ show d ++ "\n"
>               doRefactor commonElim d

> equivalences d =
>            do putStrLn $ "Refactoring equivalences blocks for source in directory " ++ show d ++ "\n"
>               doRefactor (mapM refactorEquivalences) d

General analysis/refactor builders

> doAnalysis aFun d = do putStrLn $ "Exclude any files from " ++ d ++ "/? (comma-separate list)\n"
>                        excludes <- getLine
>                           
>                        (pss, fileNames) <- readParseSrc d excludes
>                        let asts' = map (\(f, _, ps) -> aFun ps) pss
>                        -- (show (map (map (fmap (const ()))) (map (\(_, _, f) -> f) pss))) `trace`
>                        outputAnalysisFiles d asts' fileNames

> doRefactor rFun d = do putStrLn $ "Exclude any files from " ++ d ++ "/? (comma-separate list)\n"
>                        excludes <- getLine
>               
>                        (ps, fileNames) <- readParseSrc d excludes
>                        let (report, asts') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
>                        putStrLn report
>               
>                        outputFiles d asts' (map snd3 ps) fileNames

General source file handling stuff


> readParseSrc d excludes = do dirF <- rGetDirectoryContents d
>                              let files = dirF \\ ((map unpack (split (==',') (pack excludes))))
>                              let files' = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) files
>                              let files'' = map (\y -> d ++ "/" ++ y) files'
>                              ps <- mapM readParseSrc' files''
>                              return (ps, files')


> readParseSrc' f = do putStrLn f 
>                      inp <- readFile f
>                      ast <- pr f
>                      return $ (f, inp, map (fmap (const unitAnnotation)) ast)                                        

> setupOut d = if ((Prelude.drop (length d - 3) d) == "-out") then  -- don't do this (hence the '-' pref to stop this)
>                  return d
>              else if d == "." then 
>                   do createDirectoryIfMissing True ("out")
>                      return $ "out"    
>              else do createDirectoryIfMissing True (d ++ "out")
>                      return $ d ++ "out"

> checkDir f = let ix = elemIndices '/' f
>                  d = take (last ix) f
>              in createDirectoryIfMissing True d

> outputFiles d asts inps files =
>            do d' <- setupOut d
>               putStrLn $ "Writing refactored files to directory: " ++ d' ++ "/"
>               mapM (\(ast', (inp, f)) -> do checkDir (d' ++ "/" ++ f) 
>                                             writeFile (d' ++ "/" ++ f) (reprint inp f ast')) (Prelude.zip asts (Prelude.zip inps files))
>               return ()

> outputAnalysisFiles d asts files =
>            do putStrLn $ "Writing analysis files to directory: " ++ d ++ "/"
>               mapM (\(ast', f) -> writeFile (d ++ "/" ++ f ++ ".html") ((concatMap outputHTML) ast')) (Prelude.zip asts files)
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

> pr  :: String -> IO [Program A0]
> pr f = let mode = ParseMode { parseFilename = f }
>        in do inp <- readFile f
>              case (runParserWithMode mode parser inp) of
>                (ParseOk p)       -> return $ p
>                (ParseFailed l e) -> error e




OLD FUNS FOR PURPOSE OF TESTING

> go3 f = 
>     do inp <- readFile f
>        p <- pr f
>        let (r, p') = (refactorEquivalences (f, map (fmap (const unitAnnotation)) p))
>        let out = reprint inp f p'
>        let pa' = analyse' p'
>        writeFile (f ++ ".out.html") (concatMap outputHTML pa')
>        writeFile (f ++ ".out") out
>        let (r2, p'') = (deadCode True pa')
>        let out' = reprint inp f p''
>        writeFile (f ++ ".2.out") out'
>        putStrLn $ r ++ r2

> goR :: String -> IO ()
> goR s = do f' <- pr s
>            putStrLn $ show f'

> go :: String -> IO ()
> go s = do -- f <- readFile s
>           -- let f' = parse f
>           f' <- pr s
>           --let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           --let f'' = map ((transformBi annotateFVDo) . (fmap (const ([""],[""])))) f'
>           let f'' = loopAnalyse f'
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