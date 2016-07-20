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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Camfort.Specification.Stencils.Annotation where

import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Stencils.Syntax
import qualified Camfort.Specification.Stencils.Grammar as Gram

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA


{- *** Routines for associating annotations to ASTs -}

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable (FA.Analysis Annotation) Gram.Specification where
  annotateWithAST ann ast =
    onPrev (\ann -> ann { stencilSpec = Just $ Left ast }) ann

instance Linkable (FA.Analysis Annotation) where
  link ann (b@(F.BlDo {})) =
      onPrev (\ann -> ann { stencilBlock = Just b }) ann
  link ann (b@(F.BlStatement _ _ _ (F.StExpressionAssign {}))) =
      onPrev (\ann -> ann { stencilBlock = Just b }) ann
  link ann b = ann
