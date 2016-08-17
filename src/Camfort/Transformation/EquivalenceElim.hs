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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}

module Camfort.Transformation.EquivalenceElim where

import Data.Data
import Data.List
import qualified Data.Map as M
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis.Types as FAT (analyseTypes, TypeEnv)
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis as FA

import Camfort.Output
import Camfort.Helpers
import Camfort.Helpers.Syntax
import Camfort.Analysis.Annotations
import Camfort.Transformation.DeadCode

import Debug.Trace

type RfEqState = ([[F.Expression A]], Int, Report)

refactorEquivalences ::
    (Filename, F.ProgramFile A) -> (Report, (Filename, F.ProgramFile A))
refactorEquivalences (fname, pf) =
    let ?fname = fname
    in do
        let (pf'', typeEnv) = FAT.analyseTypes pf'
        pf''' <- transformBiM (equivalences typeEnv) pf''
        deadCode True (fname, fmap (FA.prevAnnotation) pf''')
  where
    -- initialise analysis
    pf'   = FAR.analyseRenames . FA.initAnalysis $ pf
    equivalences :: (?fname :: String) => FAT.TypeEnv -> F.Block Annotation -> (Report, F.Block Annotation)
    equivalences tenv b = (report, b')
      where
         (b', (_, _, report)) = runState equiv ([], 0, "")
         equiv = do b' <- transformBiM perStatementRmEquiv b
                    descendBiM (addCopy tenv) b'

addCopy :: FAT.TypeEnv -> [F.Block A] -> State RfEqState [F.Block A]
addCopy tenv stmts = do
    stmtss <- mapM (addCopyS tenv) stmts
    return $ concat stmtss

addCopyS :: FAT.TypeEnv -> F.Block A -> State RfEqState [F.Block A]
addCopyS tenv x@(F.BlStatement a0 s0 lab
                 (F.StExpressionAssign a sp@(FU.SrcSpan s1 s2) e1 e2))
  | not (pRefactored a) = do
    eqs <- equivalents e1
    if (length eqs <= 1)
      then return [x]
      else
        let
         a' = a { refactored = Just s1 }
         sp' = refactorSpan sp
         -- remove self from list
         eqs' = deleteBy (\x -> \y -> (af x) == (af y)) e1 eqs
         eqs'' = map mkCopy (zip [0..(length eqs')] eqs')
         -- see if two expressions are variables and have the same type
         equalTypes e e' = do
           v1 <- varExprToVariable e
           v2 <- varExprToVariable e'
           t1 <- M.lookup v1 tenv
           t2 <- M.lookup v2 tenv
           if (t1 == t2) then Just t1 else Nothing
         -- Create copy statements
         mkCopy (n, e') = F.BlStatement a' sp' Nothing $
            case equalTypes e1 e' of
              -- Types not equal, so create a transfer
              Nothing -> F.StExpressionAssign a' sp' e' call
                where
                  call = F.ExpFunctionCall a' sp' transf argst
                  transf = F.ExpValue a' sp' (F.ValVariable "transfer")
                  argst  = Just (F.AList a' sp' args)
                  args   = map (F.Argument a' sp' Nothing) [e1, e']
              -- Types are equal, simple a assignment
              Just t -> F.StExpressionAssign a' sp' e' e1
         -- Reporting
         (FU.Position _ l c) = s1
         reportF i = show (l + i, c) ++ ": addded copy due to refactored equivalence"
         report n = concatMap reportF [n..(n + length eqs'')]
        in do
          -- Update refactoring state
          (equivs, n, r) <- get
          put (equivs, n + (length eqs'), r ++ (report n))
          -- Sequence original assignment with new assignments
          return $ x : eqs''

addCopyS tenv x = do
   x' <- descendBiM (addCopy tenv) x
   return [x']

perStatementRmEquiv :: F.Statement A -> State RfEqState (F.Statement A)
perStatementRmEquiv f@(F.StEquivalence a sp@(FU.SrcSpan spL spU) equivs) = do
    (ess, n, r) <- get
    let report = r ++ (show spL) ++ ": removed equivalence \n"
    put (((map F.aStrip) . F.aStrip $ equivs) ++ ess, n - 1, report)
    let a' = a -- {refactored = (Just $ spL)} -- TODO UPDATE
    return (F.StEquivalence a' (dropLine sp) equivs)
perStatementRmEquiv f = return f

-- 'equivalents e' returns a list of variables/memory cells
-- that have been equivalenced with "e".
equivalents :: F.Expression A -> State RfEqState [F.Expression A]
equivalents x = do
    (equivs, _, _) <- get
    return (inGroup x equivs)
  where
    inGroup x [] = []
    inGroup x (xs:xss) =
        if (AnnotationFree x `elem` (map AnnotationFree xs))
        then xs
        else inGroup x xss