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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Transformation.DeadCode where

import Camfort.Analysis.Annotations
import Camfort.Analysis.LVA
import Camfort.Analysis.Syntax
import Camfort.Transformation.Syntax
import Camfort.Traverse
import Language.Fortran

import Camfort.Helpers

import GHC.Generics

import Debug.Trace

import Data.Generics.Uniplate.Operations

deadCode :: Bool -> (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
deadCode flag (fname, p) =
             let (r, p') = mapM ((transformBi elimEmptyFseq) . transformBiM (elimDead flag)) (lva p)
             in if r == "" then (r, (fname, p'))
                           else (r, (fname, p')) >>= (deadCode flag)

elimEmptyFseq :: Fortran Annotation -> Fortran Annotation
elimEmptyFseq (FSeq _ _ (NullStmt _ _) n2@(NullStmt _ _)) = n2
elimEmptyFseq f = f

elimDead :: Bool -> Fortran Annotation -> (Report, Fortran Annotation)
elimDead flag x@(Assg a sp@(s1, s2) e1 e2) | (pRefactored a) == flag =
       let lOut = liveOut a
          -- currently assumes an assign defines only one access (which is usual)
       in if ((varExprToAccesses e1) == []) || ((head $ varExprToAccesses e1) `elem` lOut) then 
              return x
          else let report = "o" ++ (show . srcLineCol $ s1) ++ ": removed dead code\n"
               in (report, NullStmt (a { refactored = (Just s1) }) (dropLine sp))
elimDead _ x = return x
