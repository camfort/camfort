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
 (InferMode, infer, check, synth) where

import Camfort.Specification.Stencils.CheckFrontend hiding (LogLine)
import Camfort.Specification.Stencils.InferenceFrontend
import Camfort.Specification.Stencils.Synthesis
import Camfort.Analysis.Annotations
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB

import Data.List

--------------------------------------------------
--         Stencil specification inference      --
--------------------------------------------------

-- Top-level of specification inference
infer :: InferMode -> Char -> Filename
      -> F.ProgramFile Annotation
      -> (String, F.ProgramFile Annotation)
infer mode marker filename pf =
    -- Append filename to any outputs
    if null output
       then ("", fmap FA.prevAnnotation pf'')
       else ("\n" ++ filename ++ "\n" ++ output, fmap FA.prevAnnotation pf'')
    where
      output = intercalate "\n"
             . filter (not . white)
             . map (formatSpec Nothing) $ results
      white = all (\x -> (x == ' ') || (x == '\t'))
      (pf'', results) = stencilInference mode marker
                      . FAB.analyseBBlocks $ pf'
      pf'     = FAR.analyseRenames . FA.initAnalysis $ pf

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: InferMode
      -> Char
      -> [(Filename, F.ProgramFile A)]
      -> (String, [(Filename, F.ProgramFile Annotation)])
synth mode marker = foldr buildOutput ("", [])
  where
    buildOutput (f, pf) (r, pfs) = (r ++ r', (f, pf') : pfs)
      where (r', pf') = synthPF mode marker f pf

synthPF :: InferMode -> Char -> Filename
      -> F.ProgramFile Annotation
      -> (String, F.ProgramFile Annotation)
synthPF _ marker _ pf =
    -- Append filename to any outputs
    ("", fmap FA.prevAnnotation pf'')
    where
      (pf'', _) = stencilInference Synth marker
                . FAB.analyseBBlocks $ pf'
      pf'     = FAR.analyseRenames . FA.initAnalysis $ pf

--------------------------------------------------
--         Stencil specification checking       --
--------------------------------------------------

check :: Filename -> F.ProgramFile Annotation -> String
check filename pf =
    -- Append filename to any outputs
    if null output then "" else "\n" ++ filename ++ "\n" ++ output
    where
     output  = intercalate "\n" results
     -- Applying checking mechanism
     results  = stencilChecking . FAB.analyseBBlocks $ pf'
     pf'      = FAR.analyseRenames . FA.initAnalysis $ pf

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
