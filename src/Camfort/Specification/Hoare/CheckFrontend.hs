{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.CheckFrontend where

import           Control.Lens
import           Control.Monad.Writer.Strict              hiding (Product)
import           Data.Either                              (partitionEithers)
import           Data.Generics.Uniplate.Operations
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import Control.Exception

-- import qualified Language.Fortran.Analysis.BBlocks        as F
-- import qualified Language.Fortran.Analysis.DataFlow       as F
import qualified Language.Fortran.Analysis                as F
import qualified Language.Fortran.AST                     as F
import qualified Language.Fortran.Util.Position           as F

import           Camfort.Analysis                         hiding (Analysis)
import qualified Camfort.Analysis                         as CA
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Specification.Parser             (SpecParseError)

import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Parser.Types (HoareParseError)
import           Camfort.Specification.Hoare.Syntax

-- TODO: Update frontend!

--------------------------------------------------------------------------------
--  Results and errors
--------------------------------------------------------------------------------

data HoareFrontendError
  = ParseError (SpecParseError HoareParseError)
  | InvalidPUConditions F.ProgramUnitName [PrimSpec ()]
  | BackendError HoareBackendError
  deriving (Show)

instance Exception HoareFrontendError where
  displayException = \case
    ParseError sp spe -> "at " ++ show sp ++ " parse error: " ++ displayException spe
    InvalidPUConditions sp nm conds ->
      "at " ++ show sp ++ " invalid specification types attached to PU with name " ++ show nm ++ ": " ++
      show conds
    BackendError e -> displayException e

parseError :: F.SrcSpan -> SpecParseError HoareParseError -> HoareCheckResult
parseError sp err = HFail (ParseError sp err) mempty

-- | Finds all annotated program units in the given program file. Returns errors
-- for program units that are incorrectly annotated, along with a list of
-- program units which are correctly annotated at the top level.
findAnnotatedPUs :: F.ProgramFile HA -> ([HoareFrontendError], [AnnotatedProgramUnit])
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
          SpecPre -> Right (Left f)
          SpecPost -> Right (Right f)
          _ -> Left spec

      -- For a given program unit and list of associated specifications, either
      -- create an annotated program unit, or report an error if something is
      -- wrong.
      collectOrReport :: (F.ProgramUnit HA, [PrimSpec ()]) -> Either HoareFrontendError AnnotatedProgramUnit
      collectOrReport (pu, specs) =
        let (errors, results) = partitionEithers (map preOrPost specs)
            (preconds, postconds) = partitionEithers results
        in if null errors
           then Right (AnnotatedProgramUnit preconds postconds pu)
           else Left (InvalidPUConditions (F.getSpan pu) (F.puName pu) errors)

  in partitionEithers (map collectOrReport pusWithSpecs)


invariantChecking :: F.ProgramFile HA -> HoareAnalysis HoareCheckResult
invariantChecking = do
  pf <- analysisInput

    -- Attempt to parse comments to specifications
  let (pf', annResults) = runWriter $ annotateComments hoareParser (\srcSpan err -> tell [parseError srcSpan err]) pf

      (errors, annotatedPUs) = findAnnotatedPUs pf'

      checkAndReport apu = do
        let nm = F.puName (apu ^. apuPU)
            prettyName = case F.puSrcName (apu ^. apuPU) of
              F.Named x -> x
              _         -> show nm
        (result, logs) <- checkPU apu

        let
          logsHead = mkReport $ "Verifying program unit: " ++ prettyName ++ "\n"
          logs' = logsHead <> logs

        case result of
          Right ok -> return (HOkay ok logs')
          Left err -> return (HFail (BackendError err) logs')

  checkResults <- traverse checkAndReport annotatedPUs

  return (annResults ++ checkResults ++ map (flip HFail mempty) errors)

-- prettyInvariantChecking :: CA.SimpleAnalysis (F.ProgramFile HA) [Report]
-- prettyInvariantChecking = do
--   results <- invariantChecking

--   let prettyResult (HOkay (HoareCheckResult b) logs) =
--         logs <> mkReport (if b then " - OK" else " - Cannot verify")
--       prettyResult (HFail e logs) = logs <> mkReport (displayException e)

--   return $ map prettyResult results

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
