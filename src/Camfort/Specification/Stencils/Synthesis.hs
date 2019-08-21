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

module Camfort.Specification.Stencils.Synthesis
  ( formatSpec
  , formatSpecNoComment
  , offsetToIx
  ) where

import           Camfort.Analysis.Annotations
import           Camfort.Specification.Stencils.Syntax
import           Data.List hiding (span)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import           Language.Fortran.Util.Position
import qualified Language.Fortran.Util.Position as FU
import           Prelude hiding (span)

-- Format inferred specifications
formatSpec :: F.MetaInfo -> Int -> Char
 -> (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
 -> String
formatSpec mi indent marker (_, Right (evalInfo, name)) =
  buildCommentText mi indent $
       marker : " "
    ++ evalInfo
    ++ (if name /= "" then " :: " ++ name else "") ++ "\n"

formatSpec _ _ _ (_, Left []) = ""
formatSpec mi indent marker (_, Left specs) =
  intercalate "\n" $ map commentText specs
    where
      commentText s = buildCommentText mi indent (marker : " " ++ doSpec s)
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
  where
    a = (head $ FA.initAnalysis [unitAnnotation]) { FA.insLabel = Just 0 }
    s = SrcSpan (Position 0 0 0 "" Nothing) (Position 0 0 0 "" Nothing)

