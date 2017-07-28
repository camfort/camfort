{- |
Module      :  Camfort.Specification.Units.Analysis.Consistent
Description :  Analysis to verify units are consistent.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE ExistentialQuantification #-}

module Camfort.Specification.Units.Analysis.Consistent
  ( ConsistencyError
  , ConsistencyReport(Consistent, Inconsistent)
  , checkUnits
  ) where

import           Control.Monad.State (get)
import           Data.Data
import           Data.Generics.Uniplate.Operations
import           Data.List (find, group, intercalate, sort)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList, mapMaybe, maybe)

import           Camfort.Analysis.Annotations
import           Camfort.Analysis.Fortran
  (analysisInput, analysisModFiles, analysisParams, writeDebug)
import           Camfort.Specification.Units.Analysis (UnitsAnalysis, runInference)
import qualified Camfort.Specification.Units.Annotation as UA
import           Camfort.Specification.Units.Environment
import           Camfort.Specification.Units.InferenceBackend  (constraintsToMatrices)
import           Camfort.Specification.Units.Monad

import qualified Language.Fortran.AST           as F
import qualified Language.Fortran.Util.Position as FU

-- | A report that summarises unit consistency.
data ConsistencyReport
    -- | All units were consistent.
  = forall a. Consistent (F.ProgramFile a) Int
    -- | An inconsistency was found in units of the program.
  | Inconsistent ConsistencyError

instance Show ConsistencyReport where
  show (Consistent pf nVars) = concat ["\n", fname, ": Consistent ", show nVars, " variables checked."]
    where fname = F.pfGetFilename pf
  show (Inconsistent e) = show e

data ConsistencyError =
  Inconsistency (F.ProgramFile UA) UnitState Constraints

instance Show ConsistencyError where
  show (Inconsistency pf state cons) = concat [ "\n", fname, ": Inconsistent:\n", reportErrors, "\n\n"
                                       , unlines (map showSrcConstraint constraints)]
    where
      fname = F.pfGetFilename pf
      showSrcConstraint :: (Constraint, FU.SrcSpan) -> String
      showSrcConstraint (con, srcSpan) = show srcSpan ++ ": " ++ show con
      reportErrors = unlines [ maybe "" showSS ss ++ str | (ss, str) <- reports ]
        where
          reports = map head . group . sort . map reportError . filter relevantConstraints $ cons
          showSS  = (++ ": ") . (" - at "++) . showSpanStart

          relevantConstraints c = not (isPolymorphic0 c) && not (isReflexive c)

          isPolymorphic0 (ConEq UnitParamLitAbs{} _) = True
          isPolymorphic0 (ConEq _ UnitParamLitAbs{}) = True
          isPolymorphic0 _                         = False

          isReflexive (ConEq u1 u2) = u1 == u2
          isReflexive _ = error "isReflexive without ConEq"

      reportError con = (errSpan, pprintConstr (orient (unrename con)) ++ additionalInfo)
        where
          errSpan = findCon con
          -- Create additional info for errors
          additionalInfo =
             if null errorInfo
             then ""
             else "\n    instead" ++ intercalate "\n" (mapNotFirst (pad 10) errorInfo)
          -- Create additional info about inconsistencies involving variables
          errorInfo =
              [" '" ++ sName ++ "' is '" ++ pprintUnitInfo (unrename u) ++ "'"
                | UnitVar (vName, sName) <- universeBi con
                , u                       <- findUnitConstrFor vName ]
          -- Find unit information for variable constraints
          findUnitConstrFor v = mapMaybe (\con' -> if con == con'
                                                   then Nothing
                                                   else constrainedTo v con')
                                (concat $ M.elems templateMap)
          constrainedTo v (ConEq (UnitVar (v', _)) u) | v == v' = Just u
          constrainedTo v (ConEq u (UnitVar (v', _))) | v == v' = Just u
          constrainedTo _ _ = Nothing

          mapNotFirst _ [] = []
          mapNotFirst f (x : xs) =  x : fmap f xs

          orient (ConEq u (UnitVar v)) = ConEq (UnitVar v) u
          orient (ConEq u (UnitParamVarUse v)) = ConEq (UnitParamVarUse v) u
          orient c = c

          pad o = (++) (replicate o ' ')

      findCon :: Constraint -> Maybe FU.SrcSpan
      findCon con = lookupWith (eq con) constraints
        where eq c1 c2 = or [ conParamEq c1 c2' | c2' <- universeBi c2 ]
      templateMap = usTemplateMap state
      constraints = [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Expression UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Statement UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Argument UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    [ (c, srcSpan)
                    | x <- universeBi pf :: [F.Declarator UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ] ++

                    -- Why reverse? So that PUFunction and PUSubroutine appear
                    -- first in the list, before PUModule.
                    reverse [ (c, srcSpan)
                    | x <- universeBi pf :: [F.ProgramUnit UA]
                    , let srcSpan = FU.getSpan x
                    , c <- maybeToList (UA.getConstraint x)
                    ]

{-| Check units-of-measure for a program -}
checkUnits :: UnitsAnalysis (F.ProgramFile Annotation) ConsistencyReport
checkUnits = do
  pf <- analysisInput
  (eCons, state, logs) <- runInference runInconsistentConstraints
    -- number of 'real' variables checked, e.g. not parametric
  let
    nVars = M.size . M.filter (not . isParametricUnit) $ usVarUnitMap state
    pfUA :: F.ProgramFile UA
    pfUA = usProgramFile state -- the program file after units analysis is done
  writeDebug logs
  pure $ case eCons of
           Nothing     -> Consistent pf nVars
           (Just cons) -> Inconsistent $ Inconsistency pfUA state cons
  where
    isParametricUnit u = case u of UnitParamPosAbs {} -> True; UnitParamPosUse {} -> True
                                   UnitParamVarAbs {} -> True; UnitParamVarUse {} -> True
                                   _ -> False

lookupWith :: (a -> Bool) -> [(a,b)] -> Maybe b
lookupWith f = fmap snd . find (f . fst)

-- | Return a possible list of unsolvable constraints.
runInconsistentConstraints :: UnitSolver (Maybe Constraints)
runInconsistentConstraints = do
  cons <- usConstraints `fmap` get
  pure $ inconsistentConstraints cons

-- clear out the unique names in the UnitInfos.
unrename :: Data a => a -> a
unrename = transformBi $ \ x -> case x of
  UnitVar (_, s)                      -> UnitVar (s, s)
  UnitParamVarAbs ((_, f), (_, s))    -> UnitParamVarAbs ((f, f), (s, s))
  UnitParamVarUse ((_, f), (_, s), i) -> UnitParamVarUse ((f, f), (s, s), i)
  UnitParamEAPAbs (_, s)              -> UnitParamEAPAbs (s, s)
  UnitParamEAPUse ((_, s), i)         -> UnitParamEAPUse ((s, s), i)
  u                                   -> u

-- | Show only the start position of the 'SrcSpan'.
showSpanStart :: FU.SrcSpan -> String
showSpanStart (FU.SrcSpan l _) = show l

-- | Returns just the list of constraints that were identified as
-- being possible candidates for inconsistency, if there is a problem.
inconsistentConstraints :: Constraints -> Maybe Constraints
inconsistentConstraints [] = Nothing
inconsistentConstraints cons
  | null inconsists = Nothing
  | otherwise       = Just [ con | (con, i) <- zip cons [0..], i `elem` inconsists ]
  where
    (_, _, inconsists, _, _) = constraintsToMatrices cons
