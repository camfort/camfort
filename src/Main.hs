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

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import Data.Generics.Uniplate.Operations
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Helpers
import Camfort.Output
import Camfort.Input
import Camfort.Functionality

import Data.Text (pack, unpack, split)
import qualified Data.Map as M
import Data.Maybe

{-| The entry point to CamFort. Displays user information, and
    handlers which functionality is being requested -}
main = do
  putStrLn introMsg
  args <- getArgs

  if length args >= 2 then

    let (func : (inp : _)) = args
    in case lookup func functionality of
         Just (fun, _) -> do
           (numReqArgs, outp) <-
             if func `elem` outputNotRequired
             then if length args >= 3 && (head (args !! 2) == '-')
                  then return (2, "")
                  else -- case where an unnecessary output is specified
                       return (3, "")
             else if length args >= 3
                  then return (3, args !! 2)
                  else error $ usage ++ "This mode requires an output "
                                     ++ "file/directory to be specified."
           (opts, _) <- compilerOpts (drop numReqArgs args)
           let excluded_files = map unpack . split (==',') . pack . getExcludes
           fun inp (excluded_files opts) outp opts
         Nothing -> putStrLn fullUsageInfo

  else if length args == 1
       then putStrLn $ usage ++ "Please specify an input file/directory"
       else putStrLn fullUsageInfo

-- * Options for CamFort  and information on the different modes

fullUsageInfo = usageInfo (usage ++ menu ++ "\nOptions:") options

options :: [OptDescr Flag]
options =
     [ Option ['v','?'] ["version"] (NoArg Version)
         "show version number"
     , Option ['e']     ["exclude"] (ReqArg Excludes "FILES")
         "files to exclude (comma separated list, no spaces)"
     , Option ['s']     ["units-solver"]  (ReqArg (Solver . read) "ID")
         "units-of-measure solver. ID = Custom or LAPACK"
     , Option ['l']     ["units-literals"] (ReqArg (Literals . read) "ID")
         "units-of-measure literals mode. ID = Unitless, Poly, or Mixed"
     , Option ['m']     ["stencil-inference-mode"]
                (ReqArg (StencilInferMode . read . (++ "Mode")) "ID")
                "stencil specification inference mode. ID = Do, Assign, or Both"
     ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = introMsg ++ usage ++ menu ++ "\nOptions:"

-- * Which modes do not require an output
outputNotRequired = ["criticalUnits", "count"]

functionality = analyses ++ refactorings

{-| List of refactorings provided in CamFort -}
refactorings :: [(String
               , (FileOrDir -> [Filename] -> FileOrDir -> Options -> IO ()
               , String))]
refactorings =
    [("common", (common, "common block elimination")),
     ("commonArg", (commonToArgs,
       "common block elimination (to parameter passing)")),
     ("equivalence", (equivalences, "equivalence elimination")),
     ("dataType", (typeStructuring, "derived data type introduction")),
     ("dead", (dead, "dead-code elimination")),
     ("units", (units, "unit-of-measure inference")) ]

{-| List of analses provided by CamFort -}
analyses :: [(String
           , (FileOrDir -> [Filename] -> FileOrDir -> Options -> IO ()
           , String))]
analyses =
    [("asts", (asts,
        "blank analysis, outputs analysis files with AST information")),
     ("lva", (lvaA, "live-variable analysis")),
     ("loops", (loops, "loop information")),
     ("count", (countVarDecls, "count variable declarations")),
     ("criticalUnits", (unitCriticals,
         "calculate the critical variables for units-of-measure inference")),
     ("ast", (ast, "print the raw AST -- for development purposes")),
     ("stencils-infer", (stencilsInf, "stencil spec inference")),
     ("stencils-check", (stencilsCheck, "stencil spec checking"))]

-- * Usage and about information
version = 0.776
introMsg = "CamFort " ++ show version ++ " - Cambridge Fortran Infrastructure."
usage = "Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]\n"
menu =
  "Refactor functions:\n"
  ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ replicate (15 - length k) ' '
  ++ "\t [" ++ info ++ "] \n") refactorings
  ++ "\nAnalysis functions:\n"
  ++ concatMap (\(k, (_, info)) -> "\t" ++ k ++ replicate (15 - length k) ' '
  ++ "\t [" ++ info ++ "] \n") analyses