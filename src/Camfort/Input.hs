{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-2

Handles input of code base (files and directories)
 and passing them into the core functionality

-}

{-# LANGUAGE DoAndIfThenElse #-}

module Camfort.Input where

import Camfort.Analysis.Annotations
import Camfort.Helpers
import Camfort.Output

import qualified Language.Fortran.Parser.Any as FP
import qualified Language.Fortran.AST as F
import Language.Fortran.Util.ModFile

import qualified Data.ByteString.Char8 as B
import Data.Data
import Data.Char (toUpper)
import Data.Maybe
import Data.List (foldl', nub, (\\), elemIndices, intercalate)
import Data.Monoid
import Data.Text.Encoding.Error (replace)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)

import System.Directory

-- Class for default values of some type 't'
class Default t where
    defaultValue :: t

-- * Builders for analysers and refactorings

{-| Performs an analysis provided by its first parameter which generates
    information 's', which is then combined together (via a monoid) -}
doAnalysisSummary :: (Monoid s, Show' s) => (Filename -> F.ProgramFile A -> (s, F.ProgramFile A))
                        -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummary aFun inSrc excludes = do
  if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
                                 ++ " from " ++ inSrc ++ "/"
    else return ()
  ps <- readParseSrcDir inSrc excludes
  let (out, ps') = callAndSummarise aFun ps
  putStrLn . show' $ out

callAndSummarise aFun =
  foldl' (\(n, pss) (f, _, ps) -> let (n', ps') = aFun f ps
                                  in (n `mappend` n', ps' : pss)) (mempty, [])


{-| Performs an analysis which reports to the user,
    but does not output any files -}
doAnalysisReport :: ([(Filename, F.ProgramFile A)] -> r)
                       -> (r -> IO out)
                       -> FileOrDir -> [Filename] -> IO out
doAnalysisReport rFun sFun inSrc excludes = do
  if excludes /= [] && excludes /= [""]
      then putStrLn $ "Excluding " ++ intercalate "," excludes
                    ++ " from " ++ inSrc ++ "/"
      else return ()
  ps <- readParseSrcDir inSrc excludes
----
  let report = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
  sFun report
----

doAnalysisReportWithModFiles :: ([(Filename, SourceText, F.ProgramFile A)] -> r)
                             -> (r -> IO out)
                             -> FileOrDir
                             -> [Filename]
                             -> ModFiles
                             -> IO out
doAnalysisReportWithModFiles rFun sFun inSrc excludes mods = do
  if excludes /= [] && excludes /= [""]
      then putStrLn $ "Excluding " ++ intercalate "," excludes
                    ++ " from " ++ inSrc ++ "/"
      else return ()
  ps <- readParseSrcDirWithModFiles inSrc excludes mods
----
  let report = rFun ps
  sFun report
----

{-| Performs a refactoring provided by its first parameter, on the directory
    of the second, excluding files listed by third,
    output to the directory specified by the fourth parameter -}

-- Refactoring where just a single list of filename/program file
-- pairs is returned (the case when no files are being added)
doRefactor ::
     ([(Filename, F.ProgramFile A)] -> (String, [(Filename, F.ProgramFile A)]))
  -> FileOrDir -> [Filename] -> FileOrDir -> IO String
doRefactor rFun inSrc excludes outSrc = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
           ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readParseSrcDir inSrc excludes
    let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
    let outputs = reassociateSourceText ps ps'
    outputFiles inSrc outSrc outputs
    return report

doRefactorWithModFiles :: ([(Filename, SourceText, F.ProgramFile A)] -> (String, [(Filename, F.ProgramFile A)]))
                       -> FileOrDir
                       -> [Filename]
                       -> FileOrDir
                       -> ModFiles
                       -> IO String
doRefactorWithModFiles rFun inSrc excludes outSrc mods = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
           ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readParseSrcDirWithModFiles inSrc excludes mods
    let (report, ps') = rFun ps
    let outputs = reassociateSourceText ps ps'
    outputFiles inSrc outSrc outputs
    return report

-- For refactorings which create some files too
-- i.e., for refactoring functions that return a
-- pair of lists of filename/program file pairs is
doRefactorAndCreate ::
     ([(Filename, F.ProgramFile A)]
     -> (String, [(Filename, F.ProgramFile A)], [(Filename, F.ProgramFile A)]))
  -> FileOrDir -> [Filename] -> FileOrDir -> IO String
doRefactorAndCreate rFun inSrc excludes outSrc = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
           ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readParseSrcDir inSrc excludes
    let (report, ps', ps'') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
    let outputs = reassociateSourceText ps ps'
    let outputs' = map (\(f, pf) -> (f, B.empty, pf)) ps''
    outputFiles inSrc outSrc outputs
    outputFiles inSrc outSrc outputs'
    return report

-- For refactorings which create some files too
-- i.e., for refactoring functions that return a
-- pair of lists of filename/program file pairs is
type FileProgram = (Filename, F.ProgramFile A)
doRefactorAndCreateBinary :: ([FileProgram] -> (String, [FileProgram], [(Filename, B.ByteString)]))
                             -> FileOrDir -> [Filename] -> FileOrDir -> IO String
doRefactorAndCreateBinary rFun inSrc excludes outSrc = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
                    ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readParseSrcDir inSrc excludes
    let (report, ps', bins) = rFun (map (\ (f, inp, ast) -> (f, ast)) ps)
    let outputs = reassociateSourceText ps ps'
    outputFiles inSrc outSrc outputs
    outputFiles inSrc outSrc bins
    return report

doCreateBinary :: ([FileProgram] -> (String, [(Filename, B.ByteString)]))
               -> FileOrDir
               -> [Filename]
               -> FileOrDir
               -> ModFiles
               -> IO String
doCreateBinary rFun inSrc excludes outSrc mods = do
    if excludes /= [] && excludes /= [""]
    then putStrLn $ "Excluding " ++ intercalate "," excludes
                    ++ " from " ++ inSrc ++ "/"
    else return ()
    ps <- readParseSrcDirWithModFiles inSrc excludes mods
    let (report, bins) = rFun (map (\ (f, inp, ast) -> (f, ast)) ps)
    outputFiles inSrc outSrc bins
    return report

reassociateSourceText :: [(Filename, SourceText, a)]
                   -> [(Filename, F.ProgramFile Annotation)]
                   -> [(Filename, SourceText, F.ProgramFile Annotation)]
reassociateSourceText ps ps' = zip3 (map fst ps') (map snd3 ps) (map snd ps')
  where snd3 (a, b, c) = b

-- * Source directory and file handling

{-| Read files from a direcotry, excluding those listed
    by the second parameter -}
-- * Source directory and file handling
readParseSrcDir :: FileOrDir -> [Filename]
                   -> IO [(Filename, SourceText, F.ProgramFile A)]
readParseSrcDir inp excludes = do
    isdir <- isDirectory inp
    files <- if isdir
             then do
               files <- rGetDirContents inp
               -- Compute alternate list of excludes with the
               -- the directory appended
               let excludes' = excludes ++ map (\x -> inp ++ "/" ++ x) excludes
               return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes'
             else return [inp]
    mapMaybeM readParseSrcFile files

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . (mapM f)

readParseSrcDirWithModFiles :: FileOrDir
                            -> [Filename]
                            -> ModFiles
                            -> IO [(Filename, SourceText, F.ProgramFile A)]
readParseSrcDirWithModFiles inp excludes mods = do
    isdir <- isDirectory inp
    files <- if isdir
             then do
               files <- rGetDirContents inp
               -- Compute alternate list of excludes with the
               -- the directory appended
               let excludes' = excludes ++ map (\x -> inp ++ "/" ++ x) excludes
               return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes'
             else return [inp]
    mapMaybeM (readParseSrcFileWithModFiles mods) files

{-| Read a specific file, and parse it -}
readParseSrcFile :: Filename
                 -> IO (Maybe (Filename, SourceText, F.ProgramFile A))
readParseSrcFile f = do
    inp <- flexReadFile f
    let result = FP.fortranParserWithModFiles [] inp f
    case result of
      Right ast  -> return $ Just (f, inp, fmap (const unitAnnotation) ast)
      Left error -> (putStrLn $ show error) >> return Nothing

readParseSrcFileWithModFiles :: ModFiles
                             -> Filename
                             -> IO (Maybe (Filename, SourceText, F.ProgramFile A))
readParseSrcFileWithModFiles mods f = do
    inp <- flexReadFile f
    let result = FP.fortranParserWithModFiles mods inp f
    case result of
      Right ast  -> return $ Just (f, inp, fmap (const unitAnnotation) ast)
      Left error -> (putStrLn $ show error) >> return Nothing
----

rGetDirContents :: FileOrDir -> IO [String]
rGetDirContents d = do
    ds <- getDirectoryContents d
    let ds' = ds \\ [".", ".."] -- remove '.' and '..' entries
    rec ds'
      where
        rec []     = return []
        rec (x:xs) = do xs' <- rec xs
                        g <- doesDirectoryExist (d ++ "/" ++ x)
                        if g then
                           do x' <- rGetDirContents (d ++ "/" ++ x)
                              return $ (map (\y -> x ++ "/" ++ y) x') ++ xs'
                        else if isFortran x
                             then return (x : xs')
                             else return xs'

-- A version that lists all files, not just Fortran ones
rGetDirContents' :: FileOrDir -> IO [String]
rGetDirContents' d = do
    ds <- getDirectoryContents d
    fmap concat . mapM f $ ds \\ [".", ".."] -- remove '.' and '..' entries
      where
        f x = do
          g <- doesDirectoryExist (d ++ "/" ++ x)
          if g then do
            x' <- rGetDirContents (d ++ "/" ++ x)
            return $ map (\ y -> x ++ "/" ++ y) x'
          else return [x]

{-| predicate on which fileextensions are Fortran files -}
isFortran x = fileExt x `elem` (exts ++ extsUpper)
  where exts = [".f", ".f90", ".f77", ".cmn", ".inc"]
        extsUpper = map (map toUpper) exts

{-| extract a filename's extension -}
fileExt x = let ix = elemIndices '.' x
            in if null ix then ""
               else Prelude.drop (Prelude.last ix) x

-- | Read file using ByteString library and deal with any weird characters.
flexReadFile :: String -> IO B.ByteString
flexReadFile = fmap (encodeUtf8 . decodeUtf8With (replace ' ')) . B.readFile
