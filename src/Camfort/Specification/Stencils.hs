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

module Camfort.Specification.Stencils
 (infer, check, synth) where

import Control.Arrow ((***), first, second)

import Camfort.Specification.Stencils.CheckFrontend hiding (LogLine)
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Synthesis
import Camfort.Analysis
  (Analysis, Refactoring, mkAnalysis, runAnalysis, runRefactoring)
import Camfort.Analysis.Annotations
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB

import Data.List


-- | Helper for retrieving analysed blocks.
getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis

--------------------------------------------------
--         Stencil specification inference      --
--------------------------------------------------

-- Top-level of specification inference
infer :: Bool
      -> Char
      -> Analysis String (F.ProgramFile Annotation)
infer useEval marker = mkAnalysis $ \pf ->
  let filename = F.pfGetFilename pf
      output = intercalate "\n"
             . filter (not . white)
             . map formatSpecNoComment $ report
      white  = all (\x -> (x == ' ') || (x == '\t'))
      report = runAnalysis (stencilInference useEval marker) . getBlocks $ pf
  in
    -- Append filename to any outputs
    if null output
       then ""
       else "\n" ++ filename ++ "\n" ++ output

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: Char
      -> Refactoring String [F.ProgramFile A] [F.ProgramFile Annotation]
synth marker = first normaliseMsg . foldr buildOutput (("",""), [])
  where
    buildOutput pf =
      let f = F.pfGetFilename pf
      in case synthWithCheck pf of
           Left err         -> first . first  $ (++ mkMsg f err)
           Right (warn,pf') -> second (if null warn
                                       then id
                                       else (++ mkMsg f warn)) *** (pf':)
    synthWithCheck pf =
      let blocks = getBlocks pf
          checkRes = stencilChecking blocks in
        case checkFailure checkRes of
          Nothing  ->
            let inference = fmap FA.prevAnnotation .
                            snd $ runRefactoring (stencilSynthesis marker) blocks
                  in Right (maybe "" show (checkWarnings checkRes), inference)
          Just err -> Left $ show err

    mkMsg f e = "\nEncountered the following errors when checking\
                \ stencil specs for '" ++ f ++ "'\n\n" ++ e

    normaliseMsg ("",  warn) = warn
    normaliseMsg (err, warn) = err ++ warn ++ "\nPlease resolve these errors, and then\
                            \ run synthesis again."


--------------------------------------------------
--         Stencil specification checking       --
--------------------------------------------------

check :: Analysis String (F.ProgramFile Annotation)
check = mkAnalysis $ \pf ->
  -- Append filename to any outputs
  let output   = show . stencilChecking . getBlocks $ pf
      filename = F.pfGetFilename pf
  in if null output then "" else "\n" ++ filename ++ "\n" ++ output

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
