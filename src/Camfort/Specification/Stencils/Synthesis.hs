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

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Camfort.Specification.Stencils.Syntax

import Camfort.Analysis.Annotations

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Util.Position as FU

import Language.Fortran.Util.Position

-- Format inferred specifications
formatSpec :: F.MetaInfo -> Int -> Char
 -> (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
 -> String
formatSpec mi indent marker (span, Right (evalInfo, name)) = buildCommentText mi indent $
     marker : " "
  ++ evalInfo
  ++ (if name /= "" then " :: " ++ name else "") ++ "\n"

formatSpec _ _ _ (_, Left []) = ""
formatSpec mi indent marker (span, Left specs) =
  (intercalate "\n" $ map (\s -> buildCommentText mi indent (marker : " " ++ doSpec s)) specs)
    where
      commaSep                 = intercalate ", "
      doSpec (arrayVar, spec)  =
             show (fixSpec spec) ++ " :: " ++ commaSep arrayVar
      fixSpec s                = s


-- | Format inferred specifications, but do not format as a comment.
formatSpecNoComment ::
  (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
  -> String
formatSpecNoComment (span, Right (evalInfo, name)) =
  show span ++ "    " ++ evalInfo ++ (if name /= "" then " :: " ++ name else "") ++ "\n"
formatSpecNoComment (_, Left []) = ""
formatSpecNoComment (span, Left specs) =
  intercalate "\n" . map (\s -> show span ++ "    " ++ doSpec s) $ specs
    where
      commaSep                 = intercalate ", "
      doSpec (arrayVar, spec)  =
             show (fixSpec spec) ++ " :: " ++ commaSep arrayVar
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
