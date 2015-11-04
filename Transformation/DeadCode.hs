{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Transformation.DeadCode where

import Analysis.Annotations
import Analysis.LVA
import Analysis.Syntax
import Transformation.Syntax
import Traverse
import Language.Fortran

import Helpers

import Generics.Deriving.Copoint
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
              
