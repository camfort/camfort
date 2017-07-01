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

import Control.Arrow (first, second)

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


-- | Helper for retrieving analysed blocks.
getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis

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
       then ("", infer1)
       else ("\n" ++ filename ++ "\n" ++ output, infer1)
    where
      output = intercalate "\n"
             . filter (not . white)
             . map formatSpecNoComment $ infer2
      white = all (\x -> (x == ' ') || (x == '\t'))
      infer' = stencilInference mode marker . getBlocks $ pf
      infer1 = fmap FA.prevAnnotation . fst $ infer'
      infer2 = snd infer'

--------------------------------------------------
--         Stencil specification synthesis      --
--------------------------------------------------

-- Top-level of specification synthesis
synth :: InferMode
      -> Char
      -> [(Filename, F.ProgramFile A)]
      -> (String, [(Filename, F.ProgramFile Annotation)])
synth mode marker = first normaliseMsg . foldr buildOutput ("", [])
  where
    buildOutput (f, pf) =
      case synthWithCheck pf of
        Left e    -> first (++ mkMsg f e)
        Right pf' -> second ((f, pf'):)
    synthWithCheck pf =
      let blocks = getBlocks pf in
        case checkFailure $ stencilChecking blocks of
          Nothing  -> Right . fmap FA.prevAnnotation . fst
                            $ stencilInference Synth marker blocks
          Just err -> Left  $ show err

    mkMsg f e = "\nEncountered the following errors when checking\
                \ stencil specs for '" ++ f ++ "'\n\n" ++ e

    normaliseMsg "" = ""
    normaliseMsg xs = xs ++ "\nPlease resolve these errors, and then\
                            \ run synthesis again."


--------------------------------------------------
--         Stencil specification checking       --
--------------------------------------------------

check :: Filename -> F.ProgramFile Annotation -> String
check filename pf =
    -- Append filename to any outputs
    if null output then "" else "\n" ++ filename ++ "\n" ++ output
    where
     output = show . stencilChecking . getBlocks $ pf

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
