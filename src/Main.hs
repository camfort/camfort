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

import Data.Foldable (find)

import System.Console.GetOpt
import System.Environment

import Camfort.Helpers
import Camfort.Functionality

import Data.Text (pack, unpack, split)




{-| The entry point to CamFort. Displays user information, and
    handlers which functionality is being requested -}
main = do
  args <- getArgs
  putStrLn ""
  (opts,posArgs) <- compilerOpts args
  case opts of
    (Version:_) -> displayVersion
    (Help:_)    -> displayHelp
    _           -> do
      if length args >= 2 then
        let (f : (inp : _)) = args
        in case lookupFunctionality f functionalities of
          Nothing -> putStrLn fullUsageInfo
          Just func -> do
            (numReqArgs, outp) <-
              if RefactorInPlace `elem` opts
              -- Does not check to see if an output directory
              -- is also specified since flags come last and therefore
              -- override any specification of an output directory
              -- (which would come earlier).
                then return (2, inp)
                else case outputFileReq func of
                  OutputFileNotReq ->
                    if length args >= 3 && (head (args !! 2) == '-')
                      then
                        return (2, "")
                      else -- case where an unnecessary output is specified
                        return (3, "")
                  OutputFileReq ->
                    if length args >= 3
                      then return (3, args !! 2)
                      else fail $ usage ++ "\nThis mode requires an output\
                                          \ file/directory to be specified\n\
                                          \ or use the --inplace flag to set\
                                          \ the ouput location to be the input\
                                          \ location."
            let excluded_files = map unpack . split (==',') . pack . getExcludes
            fun func inp (excluded_files opts) outp opts
      else do
        putStrLn versionMessage
        if length args == 1
        then putStrLn $ usage ++ "Please specify an input file/directory"
        else putStrLn fullUsageInfo




-- * Options for CamFort and information on the different modes

fullUsageInfo = usageInfo (usage ++ menu ++ "\nOptions:") options

options :: [OptDescr Flag]
options =
     [ Option ['v','?'] ["version"] (NoArg Version)
         "show version number"
     , Option [] ["help"] (NoArg Help)
         "show this help message and exit"
     , Option [] ["inplace"] (NoArg RefactorInPlace)
         "refactor in place (replaces input files)"
     , Option ['e'] ["exclude"] (ReqArg Excludes "FILES")
         "files to exclude (comma separated list, no spaces)"
     , Option ['l'] ["units-literals"] (ReqArg (Literals . read) "ID")
         "units-of-measure literals mode. ID = Unitless, Poly, or Mixed"
     , Option ['m'] ["stencil-inference-mode"]
                (ReqArg (StencilInferMode . read . (++ "Mode")) "ID")
                "stencil specification inference mode. ID = Do, Assign, or Both"
     , Option ['I'] ["include-dir"] (ReqArg IncludeDir "DIR")
                "directory to search for precompiled files"
     , Option [] ["debug"] (NoArg Debug)
         "enable debug mode"
     , Option [] ["doxygen"] (NoArg Doxygen)
         "synthesise annotations compatible with Doxygen"
     , Option [] ["ford"] (NoArg Ford)
         "synthesise annotations compatible with Ford"
     ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ helpPrompt))
  where helpPrompt = "Try camfort --help for more information."




-- | Datatype for a functionality provided by the system, i.e. this is the
-- wiring between the outward facing command line interface and our internal
-- functions
data Functionality = Functionality
  { commandName :: String          -- ^ the command invoked by user at the cli
  , altNames :: [String]           -- ^ alternative command names (let's be nice)
  , fun :: FileOrDir -> [Filename] -> FileOrDir -> Options -> IO () -- ^ the function to dispatch to
  , info :: String                 -- ^ information about the functionality
  , outputFileReq :: OutputFileReq -- ^ whether an output file is required
  }

-- | A tag do destinguish whether a functionality requires an output file
data OutputFileReq = OutputFileReq | OutputFileNotReq

-- | Given a string, maybe return an associated Functionality
lookupFunctionality :: String -> [Functionality] -> Maybe Functionality
lookupFunctionality str = find (getFunctionality str)
  where getFunctionality s f | commandName f == s  = True
                             | s `elem` altNames f = True
                             | otherwise           = False



-- | The list of functionalties that Camfort provides
functionalities :: [Functionality]
functionalities = analyses ++ refactorings

analyses =
  [ Functionality
      "count"
      []
      countVarDecls
      "count variable declarations"
      OutputFileNotReq

  , Functionality
      "ast"
      []
      ast
      "print the raw AST -- for development purposes"
      OutputFileNotReq

  , Functionality
      "stencils-check"
      ["stencil-check", "check-stencils", "check-stencil"]
      stencilsCheck
      "stencil spec checking"
      OutputFileNotReq

  , Functionality
      "stencils-infer"
      ["stencil-infer", "infer-stencils", "infer-stencil"]
      stencilsInfer
      "stencil spec inference"
      OutputFileNotReq

  , Functionality
      "stencils-synth"
      ["stencil-synth", "synth-stencils", "synth-stencil"]
      stencilsSynth
      "stencil spec synthesis"
      OutputFileReq

  , Functionality
      "units-suggest"
      ["unit-suggest", "suggest-units", "suggest-unit", "units-criticals"]
      unitsCriticals
      "suggest variables to annotate with units-of-measure for maximum coverage"
      OutputFileNotReq

  , Functionality
      "units-check"
      ["unit-check", "check-units", "check-unit"]
      unitsCheck
      "unit-of-measure checking"
      OutputFileNotReq

  , Functionality
      "units-infer"
      ["unit-infer", "infer-units", "infer-unit"]
      unitsInfer
      "unit-of-measure inference"
      OutputFileNotReq

  , Functionality
      "units-synth"
      ["unit-synth", "synth-units", "synth-unit"]
      unitsSynth
      "unit-of-measure synthesise specs"
      OutputFileReq

  , Functionality
      "units-compile"
      ["unit-compile", "compile-units", "compile-unit"]
      unitsCompile
      "units-of-measure compile module information"
      OutputFileReq
  ]

refactorings =
  [ Functionality
      "common"
      ["commons"]
      common
      "common block elimination"
      OutputFileReq

  , Functionality
      "equivalence"
      ["equivalences"]
      equivalences
      "equivalence elimination"
      OutputFileReq

  , Functionality
      "dead"
      []
      dead
      "dead-code elimination"
      OutputFileReq

  , Functionality
      "datatype"
      ["datatypes"]
      datatypes
      "derived data type introduction"
      OutputFileReq
  ]



-- * Usage and about information
version = "0.903"
versionMessage = "CamFort " ++ version ++ " - Cambridge Fortran Infrastructure."

displayVersion :: IO ()
displayVersion = putStrLn versionMessage

displayHelp :: IO ()
displayHelp = putStrLn fullUsageInfo

usage = "Usage: camfort <MODE> <INPUT> [OUTPUT] [OPTIONS...]\n"
menu =
  "Refactor functions:\n"
  ++ concatMap display refactorings
  ++ "\nAnalysis functions:\n"
  ++ concatMap display analyses
  where
    space = replicate 5 ' '
    display f = space ++ commandName f ++
                replicate (15 - length (commandName f)) ' ' ++
                "   [" ++ info f ++ "] \n"
