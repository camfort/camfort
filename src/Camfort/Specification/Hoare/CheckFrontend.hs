{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

{-# OPTIONS_GHC -Wall #-}

{-|

This module is responsible for finding annotated program units, and running the
functionality in "Camfort.Specification.Hoare.CheckBackend" on each of them.

-}
module Camfort.Specification.Hoare.CheckFrontend
  (
    -- * Invariant Checking
    invariantChecking

    -- * Analysis Types
  , HoareAnalysis
  , HoareFrontendError(..)
  , HoareFrontendWarning(..)
  ) where

import           Control.Applicative                      (liftA2)
import           Control.Exception
import           Control.Lens
import           Control.Monad                            (unless)
import           Control.Monad.Writer.Lazy                hiding (Product)
import           Data.Generics.Uniplate.Operations
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (catMaybes)
import           Data.Void                                (absurd)

import qualified Language.Fortran.Analysis                as F
import qualified Language.Fortran.AST                     as F
import qualified Language.Fortran.Util.Position           as F

import           Camfort.Analysis
import qualified Camfort.Analysis.Annotations             as CA
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Specification.Parser             (SpecParseError)

import           Language.Fortran.Model.Repr.Prim

import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Parser.Types (HoareParseError)
import           Camfort.Specification.Hoare.Syntax

import           Control.DeepSeq

--------------------------------------------------------------------------------
--  Invariant Checking
--------------------------------------------------------------------------------

{-|
Runs invariant checking on every annotated program unit in the given program
file. Expects the program file to have basic block and unique analysis
information.

The 'PrimReprSpec' argument controls how Fortran data types are treated
symbolically. See the documentation in "Language.Fortran.Mode.Repr.Prim" for a
detailed explanation.
-}
invariantChecking :: PrimReprSpec -> F.ProgramFile HA -> HoareAnalysis [HoareCheckResult]
invariantChecking primSpec pf = do
  let parserWithAnns = F.initAnalysis . fmap (const CA.unitAnnotation) <$> hoareParser

  pf' <- annotateComments parserWithAnns parseError pf
  annotatedPUs <- findAnnotatedPUs pf'

  let checkAndReport apu = do
        let nm = F.puName (apu ^. apuPU)
            prettyName = describe $ case F.puSrcName (apu ^. apuPU) of
              F.Named x -> x
              _         -> show nm
        logInfo' (apu ^. apuPU) $ "Verifying program unit: " <> prettyName
        loggingAnalysisError . mapAnalysisT BackendError absurd $ checkPU apu primSpec

  catMaybes <$> traverse checkAndReport annotatedPUs

--------------------------------------------------------------------------------
--  Results and errors
--------------------------------------------------------------------------------

type HoareAnalysis = AnalysisT HoareFrontendError HoareFrontendWarning IO

data HoareFrontendError
  = ParseError (SpecParseError HoareParseError)
  | InvalidPUConditions F.ProgramUnitName [SpecOrDecl InnerHA]
  | BackendError HoareBackendError

instance NFData HoareFrontendError where
  rnf _ = ()

data HoareFrontendWarning
  = OrphanDecls F.ProgramUnitName

instance NFData HoareFrontendWarning where
  rnf _ = ()

instance Describe HoareFrontendError where
  describeBuilder = \case
    ParseError spe -> "parse error: " <> describeBuilder (displayException spe)
    InvalidPUConditions nm conds ->
      "invalid specification types attached to PU with name " <> describeBuilder (show nm) <> ": " <>
      describeBuilder (show conds)
    BackendError e -> describeBuilder e

instance Describe HoareFrontendWarning where
  describeBuilder = \case
    OrphanDecls nm ->
      "auxiliary variable declared for a program unit with no annotations with name " <>
      describeBuilder (show nm) <> "; skipping invariant checking for this program unit"

--------------------------------------------------------------------------------
--  Internal
--------------------------------------------------------------------------------

parseError :: F.SrcSpan -> SpecParseError HoareParseError -> HoareAnalysis ()
parseError sp err = logError' sp (ParseError err)

-- | Finds all annotated program units in the given program file. Throws errors
-- for program units that are incorrectly annotated. Returns a list of program
-- units which are correctly annotated at the top level.
findAnnotatedPUs :: F.ProgramFile HA -> HoareAnalysis [AnnotatedProgramUnit]
findAnnotatedPUs pf =
  let pusByName :: Map F.ProgramUnitName (F.ProgramUnit HA)
      pusByName = Map.fromList [(F.puName pu, pu) | pu <- universeBi pf]

      -- Each annotation may get linked with one program unit. However, for this
      -- analysis we want to collect all of the annotations that are associated
      -- with the same program unit. For this we need to do some extra work
      -- because the comment annotator can't directly deal with this situation.
      sodsByPU :: Map F.ProgramUnitName [SpecOrDecl InnerHA]
      sodsByPU = Map.fromListWith (++)
        [ (nm, [sod])
        | ann <- universeBi pf :: [HA]
        , nm  <- F.prevAnnotation ann ^.. hoarePUName . _Just
        , sod <- F.prevAnnotation ann ^.. hoareSod    . _Just
        ]

      -- For a given program unit and list of associated specifications, create
      -- an annotated program unit, and report an error if something is wrong.
      collectUnit
        :: F.ProgramUnit HA -> [SpecOrDecl InnerHA]
        -> HoareAnalysis (Maybe AnnotatedProgramUnit)
      collectUnit pu sods = do
        let pres  = sods ^.. traverse . _SodSpec . _SpecPre
            posts = sods ^.. traverse . _SodSpec . _SpecPost
            decls = sods ^.. traverse . _SodDecl

            errors = filter (isn't (_SodSpec . _SpecPre ) .&&
                             isn't (_SodSpec . _SpecPost) .&&
                             isn't _SodDecl)
                     sods
              where (.&&) = liftA2 (&&)

            result = AnnotatedProgramUnit pres posts decls pu

        unless (null errors) $ logError' pu (InvalidPUConditions (F.puName pu) errors)

        if null pres && null posts
          then do
            unless (null decls) $ logWarn' pu (OrphanDecls (F.puName pu))
            return Nothing
          else return $ Just result

      apus :: [HoareAnalysis (Maybe AnnotatedProgramUnit)]
      apus = map snd . Map.toList $ Map.intersectionWith collectUnit pusByName sodsByPU

  in catMaybes <$> sequence apus
