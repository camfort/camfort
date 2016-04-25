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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Analysis.Loops where

import Data.Data
import Data.List
import Data.Ord

import Language.Fortran
import Language.Fortran.Pretty


import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Debug.Trace

import Camfort.Analysis.LVA
import Camfort.Analysis.Annotations
import Camfort.Analysis.Syntax
import Camfort.Analysis.Types

import Camfort.Helpers
import Camfort.Traverse

import Camfort.Transformation.Syntax -- <- for doing reassociation

import qualified Data.Map.Lazy as Map hiding (map, (\\))

--  when travesing whole program collect all declarations with bounds 
--  collect all constants (#1) 
--  identify all loop 'variables' (#2) 
--   - identify all variables indexed by the loop variables

-- loopBody :: Fortran t -> State (TypeEnvStack t) (Fortran ([String], [String], [String]))
-- loopBody (For _ v@(VarName _ s) e1 e2 e3 body) = 
--     let
--         anno = (
--     in For anno v e1 e2 e3 body
--  
-- newFrame gammas = []:gammas
-- pushVar v t (g:gs) = ((v, t):g):gs
-- popVar (((v,t):g):gs) = (g:gs)
-- popFrame (g:gs) = (g, gs)

-- ap (fmap ((,[""]),[""]))

loopAnalyse :: Program a -> Program Annotation
loopAnalyse p = map ((descendBi arrayIndices) . ix . lvaOnUnit . (transformBi reassociate) . (fmap (const unitAnnotation))) p

analyse' :: Program Annotation -> Program Annotation
analyse' p = map ((descendBi arrayIndices) . ix . lvaOnUnit . (transformBi reassociate))  p

-- collect: from an association list to a map with list-based bins for matching keys
collect :: (Eq a, Ord k) => [(k, a)] -> Map.Map k [a]
collect = Map.fromListWith union . map (fmap (:[]))

arrayIndices :: Block Annotation -> Block Annotation
arrayIndices x = 
    let tenv = typeEnv x
        
        arrIxsF :: Fortran Annotation -> Annotation
        arrIxsF y = let readIxs = [(v, mfmap (const ()) e) | 
                                     (Var _ _ [(VarName _ v, e)]) <- rhsExpr y,
                                     length e > 0,
                                     isArrayType tenv v]

                        writeIxs = [(v, mfmap (const ()) e) |
                                     (Var _ _ [(VarName _ v, e)]) <- lhsExpr y,
                                     length e > 0,
                                     isArrayType tenv v]

                    in (tag y) { arrsRead = (collect readIxs), arrsWrite = (collect writeIxs) } 
    in extendBi arrIxsF x               

ix :: ProgUnit Annotation -> ProgUnit Annotation
ix = let ixF :: Fortran Annotation -> Annotation
         ixF f = (tag f) { indices = (nub [v | (For _ _ (VarName _ v) _ _ _ _) <- ((universeBi f)::[Fortran Annotation])])}
     in extendBi ixF

loopVariables :: ProgUnit Annotation -> [String]
loopVariables f = (nub [v | (For _ _ (VarName _ v) _ _ _ _) <- ((universeBi f)::[Fortran Annotation])])
