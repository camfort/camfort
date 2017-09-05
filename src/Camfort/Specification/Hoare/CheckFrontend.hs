{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.CheckFrontend where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Writer.Strict              hiding (Product)
import           Data.Either                              (partitionEithers)
import           Data.Generics.Uniplate.Operations
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.Maybe                               (catMaybes)
import           Data.Void                                (Void)

import qualified Language.Fortran.Analysis                as F
import qualified Language.Fortran.AST                     as F
import qualified Language.Fortran.Util.Position           as F

import           Camfort.Analysis
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Specification.Parser             (SpecParseError)

import           Language.Fortran.Model.Repr.Prim

import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Parser.Types (HoareParseError)
import           Camfort.Specification.Hoare.Syntax

--------------------------------------------------------------------------------
--  Results and errors
--------------------------------------------------------------------------------

type HoareAnalysis = AnalysisT HoareFrontendError Void IO

data HoareFrontendError
  = ParseError (SpecParseError HoareParseError)
  | InvalidPUConditions F.ProgramUnitName [PrimSpec ()]
  | BackendError HoareBackendError
  -- deriving (Show)

instance Describe HoareFrontendError where
  describeBuilder = \case
    ParseError spe -> "parse error: " <> describeBuilder (displayException spe)
    InvalidPUConditions nm conds ->
      "invalid specification types attached to PU with name " <> describeBuilder (show nm) <> ": " <>
      describeBuilder (show conds)
    BackendError e -> describeBuilder e

parseError :: F.SrcSpan -> SpecParseError HoareParseError -> HoareAnalysis ()
parseError sp err = logError' sp (ParseError err)

-- | Finds all annotated program units in the given program file. Returns errors
-- for program units that are incorrectly annotated, along with a list of
-- program units which are correctly annotated at the top level.
findAnnotatedPUs :: F.ProgramFile HA -> HoareAnalysis [AnnotatedProgramUnit]
findAnnotatedPUs pf =
  let pusByName :: Map F.ProgramUnitName (F.ProgramUnit HA)
      pusByName = Map.fromList [(F.puName pu, pu) | pu <- universeBi pf]

      -- Each annotation may get linked with one program unit. However, for this
      -- analysis we want to collect all of the annotations that are associated
      -- with the same program unit. For this we need to do some extra work
      -- because the comment annotator can't directly deal with this situation.
      specsByPU :: Map F.ProgramUnitName [PrimSpec ()]
      specsByPU = Map.fromListWith (++)
        [(nm, [spec])
        | ann <- universeBi pf :: [HA]
        , Just nm <- [F.prevAnnotation ann ^. hoarePUName]
        , Just spec <- [F.prevAnnotation ann ^. hoareSpec]]

      pusWithSpecs :: [(F.ProgramUnit HA, [PrimSpec ()])]
      pusWithSpecs = map snd . Map.toList $ Map.intersectionWith (,) pusByName specsByPU

      -- For program units, we care about specifications which are preconditions
      -- or postconditions. Any other kind of specification attached to a
      -- program unit is an error.
      preOrPost :: PrimSpec () -> Either (PrimSpec ()) (Either (PrimFormula ()) (PrimFormula ()))
      preOrPost spec@(Specification { _specType = ty, _specFormula = f }) =
        case ty of
          SpecPre  -> Right (Left f)
          SpecPost -> Right (Right f)
          _        -> Left spec

      -- For a given program unit and list of associated specifications, either
      -- create an annotated program unit, or report an error if something is
      -- wrong.
      collectUnit :: (F.ProgramUnit HA, [PrimSpec ()]) -> HoareAnalysis (Maybe AnnotatedProgramUnit)
      collectUnit (pu, specs) =
        let (errors, results) = partitionEithers (map preOrPost specs)
            (preconds, postconds) = partitionEithers results
        in if null errors
           then return (Just (AnnotatedProgramUnit preconds postconds pu))
           else logError' pu (InvalidPUConditions (F.puName pu) errors) >> return Nothing

  in catMaybes <$> traverse collectUnit pusWithSpecs


invariantChecking :: PrimReprSpec -> F.ProgramFile HA -> HoareAnalysis [HoareCheckResult]
invariantChecking primSpec pf = do
    -- Attempt to parse comments to specifications
  pf' <- annotateComments hoareParser parseError pf

  annotatedPUs <- findAnnotatedPUs pf'

  let checkAndReport apu = do
        let nm = F.puName (apu ^. apuPU)
            prettyName = describe $ case F.puSrcName (apu ^. apuPU) of
              F.Named x -> x
              _         -> show nm
        logInfo' (apu ^. apuPU) $ "Verifying program unit: " <> prettyName
        loggingAnalysisError . mapAnalysisT BackendError id $ checkPU apu primSpec

  catMaybes <$> traverse checkAndReport annotatedPUs

--------------------------------------------------------------------------------
--  Other
--------------------------------------------------------------------------------


-- tryWalkPU :: [PrimSpec ()] -> F.ProgramUnit HA -> HoareAnalysis a ()
-- tryWalkPU specs (F.PUFunction ann _ _ _ nm args _ body subprograms) = do
--   let walkStatement = \case
--         F.StDeclaration _ _ _ _ _ -> debugLog "declaration"
--         F.StImplicit _ _ _ -> debugLog "implicit"
--         F.StExpressionAssign _ _ _ _ -> debugLog "expression assign"
--         st@_ -> debugLog (show st ++ "")

--       walkBlock = \case
--         F.BlStatement _ _ _ st -> walkStatement st
--         F.BlIf _ _ _ _ _ bodies _ -> do
--           debugLog "if"
--           traverse_ (\b -> do debugLog "else"; traverse_ walkBlock b) bodies
--           debugLog "end if"
--         F.BlDoWhile _ _ _ _ _ body _ -> do
--           debugLog "while"
--           traverse_ walkBlock body
--           debugLog "end while"
--         F.BlComment _ _ c -> debugLog "comment"

--       walkSpec spec = debugLog $ "spec with type " ++ show (spec^.specType)

--   debugLog $ "Entering function: " ++ nm

--   traverse_ walkSpec specs
--   traverse_ walkBlock body

-- tryWalkPU _ _ = tell (mkReport "Found non-function program unit")

-- tryWalkHA :: HA -> HoareAnalysis a ()
-- tryWalkHA F.Analysis { F.prevAnnotation = ha } =
--   case (ha ^. annHoarePU, ha ^. annHoareSpecs) of
--     (Just pu, specs) -> tryWalkPU specs pu
--     _                -> return ()



-- printPUType :: F.ProgramUnit ann -> CA.SimpleAnalysis x ()
-- printPUType F.PUBlockData{}  = debugLog "block data"
-- printPUType F.PUComment{}    = debugLog "comment"
-- printPUType F.PUFunction{}   = debugLog "function"
-- printPUType F.PUMain{}       = debugLog "main"
-- printPUType F.PUModule{}     = debugLog "module"
-- printPUType F.PUSubroutine{} = debugLog "subroutine"
