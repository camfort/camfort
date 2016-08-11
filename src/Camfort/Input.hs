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

{-

Handles input of code base (files and directories)
 and passing them into the core functionality

-}

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Input where

-- FIXME: Did enough to get this module to compile, it's not optimised to use ByteString.
import qualified Data.ByteString.Char8 as B
import qualified Language.Fortran.Parser as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

import Data.Monoid
import Data.Generics.Uniplate.Operations
import Camfort.Analysis.Annotations

import Language.Haskell.ParseMonad
import qualified Language.Haskell.Syntax as LHS

import System.Directory

import Camfort.Helpers
import Camfort.Output
import Camfort.Traverse

import Data.Data
import Data.List (nub, (\\), elemIndices, intersperse)

-- Class for default values of some type 't'
class Default t where
    defaultValue :: t

-- From a '[t]' extract the first occurence of an 'opt' value.
-- If one does not exist, return the default 'opt'
getOption :: forall t opt . (Data opt, Data t, Default opt) => [t] -> opt
getOption [] = defaultValue
getOption (x : xs) =
    case universeBi x :: [opt] of
      []        -> getOption xs
      (opt : _) -> opt

-- * Builders for analysers and refactorings

{-| Performs an analysis provided by its first parameter which generates
    information 's', which is then combined together (via a monoid) -}
doAnalysisSummary :: (Monoid s, Show s)
                  => (Program A -> s) -> FileOrDir -> [Filename] -> IO ()
doAnalysisSummary aFun d excludes = do
  if excludes /= [] && excludes /= [""]
  then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                               ++ " from " ++ d ++ "/"
  else return ()

  ps <- readParseSrcDir d excludes

  let inFiles = map Fortran.fst3 ps
  putStrLn "Output of the analysis:"
  putStrLn $ show $ Prelude.foldl (\n (f, _, ps) -> n `mappend` (aFun ps)) mempty ps

{-| Performs an analysis which reports to the user,
    but does not output any files -}
doAnalysisReport :: ([(Filename, Program A)] -> (String, t1))
                 -> FileOrDir -> [Filename] -> t -> IO ()
doAnalysisReport rFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
  then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                               ++ " from " ++ inSrc ++ "/"
  else return ()
  ps <- readParseSrcDir inSrc excludes
  putStr "\n"
  let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
  putStrLn report

-- Temporary doAnalysisReport version to make it work with Units-Of-Measure
-- glue code.
doAnalysisReport' :: ([(Filename, Program A)] -> (String, t1))
                  -> FileOrDir -> [Filename] -> t -> IO ()
doAnalysisReport' rFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
  then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                               ++ " from " ++ inSrc ++ "/"
  else return ()
  ps <- readParseSrcDir inSrc excludes
  putStr "\n"
  let (report, ps') = rFun (map (\(a, b, c) -> (a, c)) ps)
  putStrLn report

{-| Performs a refactoring provided by its first parameter, on the directory
    of the second, excluding files listed by third,
    output to the directory specified by the fourth parameter -}
doRefactor :: ([(Filename, Program A)]
           -> (String, [(Filename, Program Annotation)]))
           -> FileOrDir -> [Filename] -> FileOrDir -> IO String
doRefactor rFun inSrc excludes outSrc = do
  if excludes /= [] && excludes /= [""]
  then putStrLn $ "Excluding " ++ (concat $ intersperse "," excludes)
                               ++ " from " ++ inSrc ++ "/"
  else return ()

  ps <- readParseSrcDir inSrc excludes
  let (report, ps') = rFun (map (\(f, inp, ast) -> (f, ast)) ps)
  --let outFiles = filter (\f -not ((take (length $ d ++ "out") f) == (d ++ "out"))) (map fst ps')
  let outFiles = map fst ps'
  let outData = zip3 outFiles (map (B.pack . Fortran.snd3) ps) (map snd ps')
  outputFiles inSrc outSrc outData
  return report

-- * Source directory and file handling

{-| Read files from a direcotry, excluding those listed
    by the second parameter -}
readParseSrcDir :: FileOrDir -> [Filename] -> IO [(Filename, String, Program A)]
readParseSrcDir inp excludes = do
    isdir <- isDirectory inp
    files <- if isdir then do
               files <- rGetDirContents inp
               return $ (map (\y -> inp ++ "/" ++ y) files) \\ excludes
             else return [inp]
    mapM readParseSrcFile files

rGetDirContents :: FileOrDir -> IO [String]
rGetDirContents d = do
    ds <- getDirectoryContents d
    ds' <- return $ ds \\ [".", ".."] -- remove '.' and '..' entries
    rec ds'
      where
        rec []     = return $ []
        rec (x:xs) = do xs' <- rec xs
                        g <- doesDirectoryExist (d ++ "/" ++ x)
                        if g then
                           do x' <- rGetDirContents (d ++ "/" ++ x)
                              return $ (map (\y -> x ++ "/" ++ y) x') ++ xs'
                        else if isFortran x
                             then return $ x : xs'
                             else return $ xs'

{-| predicate on which fileextensions are Fortran files -}
isFortran x = elem (fileExt x) [".f", ".f90", ".f77", ".cmn", ".inc"]

{-| Read a specific file, and parse it -}
readParseSrcFile :: Filename -> IO (Filename, String, Program A)
readParseSrcFile f = do
    putStrLn f
    inp <- readFile f
    ast <- parse f
    return $ (f, inp, map (fmap (const unitAnnotation)) ast)



{-| parse file into an un-annotated Fortran AST -}
parse  :: Filename -> IO (Program ())
parse f =
    let mode = ParseMode { parseFilename = f }
        selectedParser = case (fileExt f) of
                           ".cmn" -> Fortran.include_parser
                           ".inc" -> Fortran.include_parser
                           _      -> Fortran.parser

    in do inp <- readFile f
          -- There is a temporary fix here of adding a space at the start,
          -- this is to deal with an alignment issue in the parser,
          -- but will be removed when we move to the new parser.
          case runParserWithMode mode selectedParser (' ' : pre_process inp) of
             (ParseOk p)       -> return $ p
             (ParseFailed l e) -> error e

{-| extract a filename's extension -}
fileExt x = let ix = elemIndices '.' x
            in if (length ix == 0) then ""
               else Prelude.drop (Prelude.last ix) x
