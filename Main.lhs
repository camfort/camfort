> module Main where

> import Language.Fortran.Parser
> import Language.Fortran.PreProcess
> import Language.Fortran

> import System.Directory
> import System.Environment
> import System.IO

> import Language.Haskell.ParseMonad

> import Data.Monoid
> import Data.Generics.Uniplate.Operations

> import Analysis.Annotations

> import Transformation.DeadCode
> import Transformation.CommonBlockElim
> import Transformation.EquivalenceElim
> import Transformation.DerivedTypeIntro
> import Extensions.Units

> import Analysis.Types
> import Analysis.Loops
> import Analysis.LVA
> import Analysis.Syntax

> import Helpers
> import Output
> import Traverse

> import Debug.Trace

> import Data.List (nub, (\\), elemIndices)
> import Data.Text hiding (length, head, concatMap, map, filter, take, last)

> version = 0.615

Register all availble refactorings and analyses
-----------------------------------------------

> refactorings = 
>     [("common", (common, "common block elimination")),
>      ("commonArg", (commonToArgs, "common block elimination (to parameter passing)")),
>      ("equivalence", (equivalences, "equivalence elimination")),
>      ("dataType", (typeStructuring, "derived data type introduction")),
>      ("dead", (dead, "dead-code elimination")),
>      ("units", (units, "unit-of-measure inference")),
>      ("removeUnits", (unitRemoval, "unit-of-measure removal"))]
>            
> analyses = 
>     [("lva", (lvaA, "live-variable analysis")),
>      ("loops", (loops, "loop information")), 
>      ("count", (countVarDecls, "count variable declarations"))]

> main = do putStrLn introMessage 
>           d <- getArgs 
>           if (length d == 4) then
>              let (func:(dir:(excludes:(outdir:_)))) = d
>              in case (lookup func (analyses ++ refactorings)) of 
>                   Just (func, _) -> func dir excludes outdir
>                   Nothing ->
>                       case func of 
>                          "ast"   -> ast dir (d!!2) -- d!!2 is output file here
>                          _ -> putStrLn $ usage ++ menu
>            else
>               putStrLn $ usage ++ menu

User information

> introMessage = "CamFort " ++ (show version) ++ " - Cambridge Fortran Infrastructure."
> usage = "Usage: camfort <function> <input-dir> <\"excluded-file, ...\"> <output-dir>\n\n"
> menu = "Refactor functions:\n"
>         ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ "\t [" ++ info ++ "] \n") refactorings
>         ++ "Analysis functions:\n" 
>         ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ "\t [" ++ info ++ "] \n") analyses

Wrappers on all of the features
--------------------------------

> typeStructuring inDir excludes outDir = 
>      do putStrLn $ "Introducing derived data types for source in directory " ++ show inDir ++ "\n"
>         doRefactor typeStruct inDir excludes outDir

> ast d f = do (_, _, p) <- readParseSrcFile (d ++ "/" ++ f)
>              putStrLn $ show p

> countVarDecls inDir excludes _ =  
>     do putStrLn $ "Counting variable declarations in directory " ++ show inDir ++ "\n"
>        doAnalysisSummary countVariableDeclarations inDir excludes 

> loops inDir excludes _ =  
>            do putStrLn $ "Analysing loops for source in directory " ++ show inDir ++ "\n"
>               doAnalysis loopAnalyse inDir excludes 

> lvaA inDir excludes _ =  
>           do putStrLn $ "Analysing loops for source in directory " ++ show inDir ++ "\n"
>              doAnalysis lva inDir excludes 

> dead inDir excludes outDir =
>          do putStrLn $ "Eliminating dead code for source in directory " ++ show inDir ++ "\n"
>             doRefactor ((mapM (deadCode False))) inDir excludes outDir

> commonToArgs inDir excludes outDir = 
>                  do putStrLn $ "Refactoring common blocks for source in directory " ++ show inDir ++ "\n"
>                     doRefactor (commonElimToCalls inDir) inDir excludes outDir

> common inDir excludes outDir =  
>            do putStrLn $ "Refactoring common blocks for source in directory " ++ show inDir ++ "\n"
>               doRefactor (commonElimToModules inDir) inDir excludes outDir

> equivalences inDir excludes outDir =
>            do putStrLn $ "Refactoring equivalences blocks for source in directory " ++ show inDir ++ "\n"
>               doRefactor (mapM refactorEquivalences) inDir excludes outDir

> units inDir excludes outDir = 
>           do putStrLn $ "Inferring units for source in directory " ++ show inDir ++ "\n"
>              doRefactor (mapM inferUnits) inDir excludes outDir

> unitRemoval inDir excludes outDir = 
>           do putStrLn $ "Removing units for source in directory " ++ show inDir ++ "\n"
>              doRefactor (mapM removeUnits) inDir excludes outDir


General analysis/refactor builders
----------------------------------

> doAnalysis aFun d excludes = 
>                     do if excludes /= "" 
>                            then putStrLn $ "Excluding " ++ excludes ++ " from " ++ d ++ "/"
>                            else return ()
>                       
>                        ps <- readParseSrcDir d excludes

>                        let inFiles = map fst3 ps
>                        let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) inFiles
>                        let asts' = map (\(f, _, ps) -> aFun ps) ps
>                        -- (show (map (map (fmap (const ()))) (map (\(_, _, f) -> f) pss))) `trace`
>                        outputAnalysisFiles d asts' outFiles

> doAnalysisSummary aFun d excludes = 
>                     do if excludes /= "" 
>                            then putStrLn $ "Excluding " ++ excludes ++ " from " ++ d ++ "/"
>                            else return ()
>                       
>                        ps <- readParseSrcDir d excludes

>                        let inFiles = map fst3 ps
>                        let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) inFiles
>                        putStrLn "Output of the analysis:" 
>                        putStrLn $ show $ Prelude.foldl (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps
>                        


> doRefactor rFun inDir excludes outDir
>                   = do if excludes /= "" 
>                            then putStrLn $ "Excluding " ++ excludes ++ " from " ++ inDir ++ "/"
>                            else return ()
>
>                        ps <- readParseSrcDir inDir excludes
>                        let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
>                        --let outFiles = filter (\f -> not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
>                        let outFiles = map fst ps'
>                        putStrLn report
>                        outputFiles inDir outDir (zip3 outFiles (map snd3 ps ++ (repeat "")) (map snd ps'))


General source file handling stuff
----------------------------------

> readParseSrcDir :: Directory -> String -> IO [(Filename, SourceText, Program A)]
> readParseSrcDir d excludes = do dirF <- rGetDirectoryContents d
>                                 let files = dirF \\ ((map unpack (split (==',') (pack excludes))))
>                                 let files' = map (\y -> d ++ "/" ++ y) files
>                                 mapM readParseSrcFile files'
>                                

> readParseSrcFile :: Filename -> IO (Filename, SourceText, Program A)
> readParseSrcFile f = do putStrLn f 
>                         inp <- readFile f
>                         ast <- pr f
>                         return $ (f, inp, map (fmap (const unitAnnotation)) ast)                                   

> {--

OLD 

> setupOut d = if ((Prelude.drop (length d - 3) d) == "-out") then  -- don't do this (hence the '-' pref to stop this)
>                  return d
>              else if d == "." then 
>                   do createDirectoryIfMissing True ("out")
>                      return $ "out"    
>              else do createDirectoryIfMissing True (d ++ "out")
>                      return $ d ++ "out"
> --}

> -- checkDir creates a directory (from a filename string) if it doesn't exist
> checkDir f = case (elemIndices '/' f) of 
>                [] -> return ()
>                ix -> let d = take (last ix) f
>                      in createDirectoryIfMissing True d

Given a directory and list of triples of filenames, with their source text (if it exists) and
their AST, write these to the director

> outputFiles :: Directory -> Directory -> [(Filename, SourceText, Program Annotation)] -> IO ()
> outputFiles inDir outDir pdata = 
>            do createDirectoryIfMissing True outDir
>               putStrLn $ "Writing refactored files to directory: " ++ outDir ++ "/"
>               mapM_ (\(f, inp, ast') -> let f' = changeDir outDir inDir f
>                                         in do checkDir f'
>                                               putStrLn $ "Writing " ++ f'
>                                               writeFile f' (reprint inp f' ast')) pdata


> -- changeDir is used to change the directory of a filename string.
> --  If the filename string has no directory then this is an identity 
> changeDir newDir oldDir oldFilename = newDir ++ (listDiffL oldDir oldFilename)

> listDiffL []     ys = ys
> listDiffL xs     [] = []
> listDiffL (x:xs) (y:ys) | x==y      = listDiffL xs ys
>                         | otherwise = ys

> outputAnalysisFiles d asts files =
>            do putStrLn $ "Writing analysis files to directory: " ++ d ++ "/"
>               mapM (\(ast', f) -> writeFile (f ++ ".html") ((concatMap outputHTML) ast')) (Prelude.zip asts files)
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
>                                                 else if (isFortran x) then
>                                                          return $ x : xs'
>                                                      else return $ xs'
>                                                   
> isFortran x = elem (fileExt x) [".f", ".f90", ".f77", ".cmn", ".inc"]
>               
> fileExt x = let ix = elemIndices '.' x
>             in if (length ix == 0) then "" 
>                else Prelude.drop (Prelude.last ix) x

> pr  :: String -> IO (Program ())
> pr f = let mode = ParseMode { parseFilename = f }
>            selectedParser = case (fileExt f) of 
>                             ".cmn" -> include_parser
>                             ".inc" -> include_parser
>                             _      -> parser
>        in do inp <- readFile f
>              case (runParserWithMode mode selectedParser (pre_process inp)) of
>                (ParseOk p)       -> return $ p
>                (ParseFailed l e) -> error e




OLD FUNS FOR PURPOSE OF TESTING

 go3 f = 
     do inp <- readFile f
        p <- pr f
        let (r, p') = (refactorEquivalences (f, map (fmap (const unitAnnotation)) p))
        let out = reprint inp f p'
        let pa' = analyse' p'
        writeFile (f ++ ".out.html") (concatMap outputHTML pa')
        writeFile (f ++ ".out") out
        let (r2, p'') = (deadCode True pa')
        let out' = reprint inp f p''
        writeFile (f ++ ".2.out") out'
        putStrLn $ r ++ r2

> goR :: String -> IO ()
> goR s = do f' <- pr s
>            putStrLn $ show f'

> go :: String -> IO ()
> go s = do -- f <- readFile s
>           -- let f' = parse f
>           f' <- pr s
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