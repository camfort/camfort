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

module Transformation.EquivalenceElim where

import Data.Data
import Data.List

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import Language.Fortran

import Output
import Traverse
import Helpers

import Language.Fortran.Pretty

import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types

import Transformation.DeadCode
import Transformation.Syntax

import Debug.Trace

type RfEqState = ([[Expr Annotation]], Int, Report)

refactorEquivalences :: (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
refactorEquivalences (fname, p) = 
                        let ?fname = fname
                        in do p' <- mapM (transformBiM equivalences) p
                              deadCode True (fname, p')
                        where equivalences :: (?fname :: String) => Block Annotation -> (Report, Block Annotation)
                              equivalences b = let equiv = do b' <- rmEquivalences b
                                                              transformBiM (addCopy (typeEnv b)) b'
                                                   (b', (_, _, r)) = runState equiv ([], 0, "")
                                               in (r, b')

addCopy :: (?fname :: String) => TypeEnv Annotation -> Fortran Annotation -> State RfEqState (Fortran Annotation)
addCopy tys x@(Assg a sp@(s1, s2) e1 e2) | not (pRefactored a) =
   do eqs <- equivalents e1
      if (length eqs > 1) then 

       
         let a' = a { refactored = Just s1 }
             sp' = refactorSpan sp
             eqs' = deleteBy (\x -> \y -> (af x) == (af y)) e1 eqs -- remove self from list

             -- Create copy statements
             mkCopy (n, e') = let sp' = refactorSpanN n sp
                              in 
                                case ((varExprToVariable e1) >>= (\v1' -> varExprToVariable e' >>= (\v' -> return $ eqType v1' v' tys))) of
                                 Nothing    -> Assg a' sp' e' e1 -- could be an error
                                 Just False -> Assg a' sp' e' (Var a' sp' [(VarName a' "transfer", [e1, e'])])
                                 Just True  -> Assg a' sp' e' e1
             eqs'' = map mkCopy (zip [0..(length eqs')] eqs')

             -- Reporting
             (l, c) = srcLineCol s1
             reportF (e', i) = ?fname ++ show (l + i, c) ++ ": addded copy: " ++ (pprint e') ++ " due to refactored equivalence\n"
             report n = concatMap reportF (zip eqs'' [n..(n + length eqs'')])

         in do -- Update refactoring state
               (equivs, n, r) <- get
               put (equivs, n + (length eqs'), r ++ (report n))

               -- Sequence original assignment with new assignments
               return $ FSeq a sp x (foldl1 (FSeq a' sp') eqs'')
        else
           return x
addCopy tys x = return x 


rmEquivalences :: (?fname :: String) =>  (Block Annotation) -> State RfEqState (Block Annotation)
rmEquivalences = transformBiM rmEquiv'
                   where rmEquiv' ::  Decl Annotation -> State RfEqState (Decl Annotation)
                         rmEquiv' f@(Equivalence a sp equivs) =
                                     do (ess, n, r) <- get
                                        put (equivs:ess, n - 1, r ++ ?fname ++ (show . srcLineCol . fst $ sp) ++ ": removed equivalence \n")
                                        return (NullDecl (a { refactored = (Just $ fst sp) }) (dropLine sp))
                         rmEquiv' f = return f

-- equivalents e" returns a list of variables/memory cells that have been equivalenced with "e". 
--                                     
equivalents :: (?fname :: String) => Expr Annotation -> State RfEqState [Expr Annotation]
equivalents x = let inGroup x [] = []
                    inGroup x (xs:xss) = if (AnnotationFree x `elem` (map AnnotationFree xs)) then xs
                                         else inGroup x xss
                in do (equivs, _, _) <- get 
                      return (inGroup x equivs)
