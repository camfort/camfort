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
import Data.Map hiding (map)
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import qualified Language.Fortran.AST as F
import Language.Fortran.Analysis.Types (analyseTypes, TypeEnv)
import qualified Language.Fortran.Util.Position as FU

import Camfort.Output
import Camfort.Traverse
import Camfort.Helpers

import Camfort.Analysis.Annotations
import Camfort.Analysis.Syntax
import Camfort.Transformation.DeadCode

import Debug.Trace

type RfEqState = ([[F.Expression A]], Int, Report)

refactorEquivalences :: (Filename, F.ProgramFile A) -> (Report, (Filename, F.ProgramFile A))
refactorEquivalences (fname, p) =
    let ?fname = fname
    in do
       (typeEnv, p') <- analyseTypes p
       p'' <- mapM (transformBiM (equivalences typeEnv)) p'
       return p''
       --deadCode True (fname, p')
  where equivalences :: (?fname :: String) => TypeEnv -> F.Block Annotation -> (Report, F.Block Annotation)
        equivalences tenv b = (report, b')
           where
             (b', (_, _, r)) = runState equiv ([], 0, "")
             equiv = do b' <- transforBiM rmEquivalences b
                        descendBiM (addCopy tenv) b'

addCopy :: (?fname :: String) => TypeEnv -> [F.Block A] -> State RfEqState [F.Block A]
addCopy tenv stmts = do
    stmtss <- mapM (addCopyS tenv) stmts
    return $ concat stmtss

addCopyS :: (?fname :: String) => TypeEnv -> F.Block A -> State RfEqState [F.Block A]
addCopyS tenv x@(F.BlStatement a0 s0 lab (F.StExpressionAssign a sp@(SrcSpan s1 s2) e1 e2)) | not (pRefactored a) =
   do eqs <- equivalents e1
      if (length eqs > 1) then 

       
         let a' = a -- TODO: Put back - a { refactored = Just s1 }
             sp' = refactorSpan 0 sp
             eqs' = deleteBy (\x -> \y -> (af x) == (af y)) e1 eqs -- remove self from list

             -- Create copy statements
             mkCopy (n, e') = let sp' = refactorSpan n sp
                              in F.BlStatement a0 s0 lab $ 
                                case ((varExprToVariableF e1) >>= (\v1' -> varExprToVariableF e' >>= (\v' -> return $ eqType v1' v' tenv))) of
                                 Nothing    -> F.StExpressionAssign a' sp' e' e1 -- could be an error
                                 Just False -> F.StExpressionAssign a' sp' e' call
                                                where call = F.ExpFunctionCall a' sp' transf argst
                                                      transf = F.ExpValue a' sp' (F.ValVariable "transfer")
                                                      argst = Just (F.AList a' sp' args)
                                                      args = map (F.Argument a' sp' Nothing) [e1, e']
                                 Just True  -> F.StExpressionAssign a' sp' e' e1
             eqs'' = map mkCopy (zip [0..(length eqs')] eqs')

             -- Reporting
             l = posLine s1
             c = posColumn s1
             -- TODO put back pprint
--             reportF (e', i) = ?fname ++ show (l + i, c) ++ ": addded copy: " ++ (pprint e') ++ " due to refactored equivalence\n"
             reportF (e', i) = ?fname ++ show (l + i, c) ++ ": addded copy due to refactored equivalence\n"
             report n = concatMap reportF (zip eqs'' [n..(n + length eqs'')])

         in do -- Update refactoring state
               (equivs, n, r) <- get
               put (equivs, n + (length eqs'), r ++ (report n))

               -- Sequence original assignment with new assignments
               return $ x : eqs''
        else
           return [x]
addCopyS tenv x = do
   x' <- descendBiM (addCopy tenv) x
   return [x']

perStatement :: (?fname :: String) => F.Statement A -> State RfEqState (F.Statement A)
perStatement f@(F.StEquivalence a sp@(SrcSpan spL spU) equivs) = do
    (ess, n, r) <- get
    put (((map F.aStrip) . F.aStrip $ equivs) ++ ess, n - 1, r ++ ?fname ++ (show spL) ++ ": removed equivalence \n")
    let a' = a -- {refactored = (Just $ spL)} -- TODO UPDATE
    return (F.StEquivalence a' (dropLineF sp) equivs)
perStatement f = return f

-- 'equivalents e' returns a list of variables/memory cells
-- that have been equivalenced with "e".
equivalents :: (?fname :: String) => F.Expression A -> State RfEqState [F.Expression A]
equivalents x = let inGroup x [] = []
                    inGroup x (xs:xss) = if (AnnotationFree x `elem` (map AnnotationFree xs)) then xs
                                         else inGroup x xss
                in do (equivs, _, _) <- get
                      return (inGroup x equivs)

refactorSpan :: Int -> SrcSpan -> SrcSpan
refactorSpan n (SrcSpan (Position ol ll cl) (Position ou lu cu)) =
    SrcSpan (Position ol (lu+1+n) 0) (Position ou (lu+n) cu)
