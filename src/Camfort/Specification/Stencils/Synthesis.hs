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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Specification.Stencils.Synthesis where

import Data.Data
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Syntax

import Camfort.Analysis.Annotations
import Camfort.Helpers.Vec
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers hiding (lineCol, spanLineCol)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Language.Fortran.Util.Position
import Data.Map hiding (map)

-- Format inferred specifications
formatSpec ::
    Maybe String
 -> FAR.NameMap
 -> (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
 -> String
formatSpec prefix nm (span, Right (evalInfo,name)) =
     prefix'
  ++ evalInfo
  ++ (if name /= "" then " :: " ++ realName name else "") ++ "\n"
  where
    realName v = v `fromMaybe` (v `M.lookup` nm)
    prefix' = case prefix of
                Nothing -> show span ++ "    "
                Just pr -> pr

formatSpec prefix nm (span, Left []) = ""
formatSpec prefix nm (span, Left specs) =
  (intercalate "\n" $ map (\s -> prefix' ++ doSpec s) specs)
    where
      prefix' = case prefix of
                   Nothing -> show span ++ "    "
                   Just pr -> pr
      commaSep                 = intercalate ", "
      doSpec (arrayVar, spec)  =
             show (fixSpec spec) ++ " :: " ++ commaSep (map realName arrayVar)
      realName v               = v `fromMaybe` (v `M.lookup` nm)
      fixSpec s                = s

------------------------
a = (head $ FA.initAnalysis [unitAnnotation]) { FA.insLabel = Just 0 }
s = SrcSpan (Position 0 0 0) (Position 0 0 0)

-- Make indexing expression for variable 'v' from an offset.
-- essentially inverse to `ixToOffset` in StencilSpecification
offsetToIx :: F.Name -> Int -> F.Index (FA.Analysis A)
offsetToIx v o
  | o == absoluteRep
              = F.IxSingle a s Nothing (F.ExpValue a s (F.ValInteger "0"))
  | o == 0    = F.IxSingle a s Nothing (F.ExpValue a s (F.ValVariable v))
  | o  > 0    = F.IxSingle a s Nothing (F.ExpBinary a s F.Addition
                                 (F.ExpValue a s (F.ValVariable v))
                                 (F.ExpValue a s (F.ValInteger $ show o)))
  | otherwise = F.IxSingle a s Nothing (F.ExpBinary a s F.Subtraction
                                 (F.ExpValue a s (F.ValVariable v))
                                 (F.ExpValue a s (F.ValInteger $ show (abs o))))
