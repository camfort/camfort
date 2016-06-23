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

module Camfort.Analysis.StencilSpecification
 (InferMode, infer, check, findVarFlowCycles) where

import Control.Monad.State.Lazy
import Control.Monad.Writer hiding (Product)

import qualified Camfort.Analysis.StencilSpecification.Grammar as Gram
import Camfort.Analysis.StencilSpecification.CheckFrontend hiding (LogLine)
import Camfort.Analysis.StencilSpecification.InferenceFrontend
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.CommentAnnotator
import Camfort.Analysis.Annotations
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers hiding (lineCol, spanLineCol)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Generics.Uniplate.Operations
import Data.Data
import qualified Data.Map as M
import Data.Maybe
import Data.List

--------------------------------------------------
--         Stencil specification inference      --
--------------------------------------------------

-- Top-level of specification inference
infer :: InferMode -> Filename -> F.ProgramFile Annotation -> String
infer mode filename pf =
    -- Append filename to any outputs
    if null output then "" else "\n" ++ filename ++ "\n" ++ output
    where
      output = concatMap (formatSpec nameMap) $ results
      results = stencilInference mode . FAB.analyseBBlocks $ pf'
      nameMap = FAR.extractNameMap pf'
      pf'     = FAR.analyseRenames. FA.initAnalysis $ pf


-- Format inferred specifications
formatSpec :: FAR.NameMap -> LogLine -> String
formatSpec nm (span, Right evalInfo) =
  show (spanLineCol span) ++ " \t" ++ evalInfo ++ "\n"
formatSpec nm (span, Left []) = ""
formatSpec nm (span, Left specs) =
  (intercalate "\n" $ map (\s -> loc ++ " \t" ++ doSpec s) specs) ++ "\n"
    where
      loc                      = show (spanLineCol span)
      commaSep                 = intercalate ", "
      doSpec (arrayVar, spec)  =
             show (fixSpec spec) ++ " :: " ++ commaSep (map realName arrayVar)
      realName v               = v `fromMaybe` (v `M.lookup` nm)
      fixSpec (Specification (Right (Dependency vs b))) =
          Specification (Right (Dependency (map realName vs) b))
      fixSpec s                = s

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)

--------------------------------------------------
--         Stencil specification checking       --
--------------------------------------------------

check :: Filename -> F.ProgramFile Annotation -> String
check filename =
    -- Append filename to any outputs
    (\x -> if null x then "" else "\n" ++ filename ++ "\n" ++ x)
    -- Collect output
  . (intercalate "\n")
    -- Applying checking mechanism
  -- . (FAR.underRenaming (stencilChecking . FAB.analyseBBlocks))
  . stencilChecking
  . FAB.analyseBBlocks
  . FAR.analyseRenames
  . FA.initAnalysis

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End: