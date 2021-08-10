{-# LANGUAGE LambdaCase #-}

module Camfort.Transformation.LabeledDo where

import Camfort.Analysis
import Camfort.Analysis.Annotations
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.Analysis as FA
import Camfort.Helpers.Syntax

import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Generics.Uniplate.Operations
import Control.Monad (guard)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Monoid (Any(..), (<>))
import Data.Void (Void)

import Debug.Trace

type LabeledDoAway = PureAnalysis Void Void

labeledDoAway :: F.ProgramFile A -> LabeledDoAway (F.ProgramFile A)
labeledDoAway = return . transformBi go
  where
    go :: F.Block A -> F.Block A
    go = \case
      F.BlDo   a            ss@(FU.SrcSpan s1 s2) mLabel mName (Just tl) mDoSpec body mEndLabel ->
        trace "found a matching BlDo" $ F.BlDo (aMark s1 a) ss                    mLabel mName Nothing      mDoSpec body Nothing
      x -> x
    --aMark :: FU.Position -> FA.Analysis A -> FA.Analysis A
    --aMark s = onPrev $ \ap -> ap { refactored = Just s }
    aMark :: FU.Position -> A -> A
    aMark s a = a { refactored = Just s }
