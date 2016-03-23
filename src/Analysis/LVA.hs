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
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Provides live-variable analysis for Fortran code. One of the simpler analyses in the suite (a good
starting point for any new developers). This demonstrates the use of the "zipper" format, and 
various helpers from 'Analysis.Syntax' and 'Traverse'. 

'lva' is the top-level definition here.

-}
module Analysis.LVA where
    
import Data.Data
import Data.List

import Data.Generics.Zipper
import Data.Generics.Uniplate.Operations

import Language.Fortran

import Analysis.Annotations
import Analysis.Syntax
import Transformation.Syntax
import Analysis.IntermediateReps
import Traverse


{-| live-variable analysis on a program -}
-- Recall: type Program a = [ProgUnit a]
lva :: Program Annotation -> Program Annotation
lva x = map lvaOnUnit x
        
{-| live-variable analysis at the level of a unit, not whole-program,iterates @lva1@ until a fixed-point is reached -}
lvaOnUnit :: ProgUnit Annotation -> ProgUnit Annotation
lvaOnUnit x = let y = fromZipper . (everywhere lva1) . toZipper $ numberStmts . (transformBi reassociate) $ x
              in if (y == x) then y else lvaOnUnit y

{-| Single iteration of live-variable analysis over the zipper for an AST -}
lva1 :: Zipper (ProgUnit Annotation) -> Zipper (ProgUnit Annotation)

lva1 z = case (getHole z)::(Maybe (Fortran Annotation)) of
            Just f ->  let anns =  map tag ((successors z)::[Fortran Annotation]) -- annotations of the successors
                           liveOut = nub $ concat $ map (fst . lives) anns
                           killV = kill f
                           genV  = gen f
                           liveIn = nub $ union genV (liveOut \\ killV)
                           annotation = (tag f) { lives = (liveIn, liveOut), successorStmts = map number anns }
                       in setHole (refill f annotation) z
            Nothing -> z

{-| Variables killed by the current statement -}
kill :: Fortran Annotation -> [Access]
kill (Assg _ _ e1 _) = killForLhsVar e1 
                         where
                           {-| variable killed by expressions on the left-hand side -}
                           killForLhsVar :: Expr Annotation -> [Access]
                           killForLhsVar (Var a p xes) = map (\((VarName _ v), _) -> VarA v) xes
                           killForLhsVar _            = []
kill t = concatMap accesses (lhsExpr t)

{-| Variables generated (made live) by the current statement -}
gen :: Fortran Annotation -> [Access]
gen t@(Assg _ _ e1 e2) = (concatMap accesses (rhsExpr t)) ++ (genForLhsVar e1)
                          where
                            {-| variables generated on the left-hand side -}
                            genForLhsVar :: Expr Annotation -> [Access]                                 
                            genForLhsVar t@ (Var _ _  xes) = concatMap (\(_, es) -> accesses es) xes
                            genForLhsVar _            = []
gen t = concatMap accesses (rhsExpr t)  




          







{-
 successorAnnotations :: Zipper (ProgUnit Annotation) -> [Annotation]
 successorAnnotations x = goRight x ++ (case (up x) of
                                          Just ux -> case (getHole ux)::(Maybe (Fortran Annotation)) of
                                                       Just f -> map tag (successors f) ++ (goRight ux)
                                                       Nothing -> (goRight ux)
                                          Nothing -> []) 
                           where goRight :: Zipper (ProgUnit Annotation) -> [Annotation]
                                 goRight z = (case (getHole z)::(Maybe (Fortran Annotation)) of 
                                                Just f -> [tag f]
                                                Nothing -> []) ++
                                             (case (right z) of
                                                Just rz -> goRight rz
                                                Nothing -> [])
               
                 -}
