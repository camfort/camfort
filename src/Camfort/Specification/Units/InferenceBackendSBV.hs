{-
   Copyright 2017, Matthew Danish, Vilem Liepelt, Dominic Orchard, Andrew Rice, Mistral Contrastin

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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Specification.Units.InferenceBackendSBV
  ( inconsistentConstraints, criticalVariables, inferVariables, genUnitAssignments )
where

import           Camfort.Specification.Units.BackendTypes
import           Camfort.Specification.Units.Environment
import qualified Camfort.Specification.Units.InferenceBackend as MatrixBackend
import           Control.Monad
import           Data.Function (on)
import           Data.List (partition, sortBy, groupBy, nub)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Ord (comparing)
import           Data.SBV hiding (engine, name)
import           Data.SBV.Control
import           Prelude hiding (pred)
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------

-- | Identifies the variables that need to be annotated in order for
-- inference or checking to work.
criticalVariables :: Constraints -> [UnitInfo]
criticalVariables cons = case engine cons of
  Left _ -> []
  Right (_, suggests) -> suggests

-- | Returns just the list of constraints that were identified as
-- being possible candidates for inconsistency, if there is a problem.
inconsistentConstraints :: Constraints -> Maybe Constraints
inconsistentConstraints [] = Nothing
inconsistentConstraints cons = case engine cons of
  -- assuming that SBV provides a list of label names in its unsat 'core'
  Left (core, labeledCons) -> Just . normalise . map fst . catMaybes . map (flip lookup labeledCons) $ core
  Right (_, _) -> Nothing
  where
    normalise = map (dimToConstraint . constraintToDim)

-- | Returns list of formerly-undetermined variables and their units.
inferVariables :: Constraints -> [(VV, UnitInfo)]
inferVariables cons = solvedVars
  where
    -- We are only interested in reporting the solutions to variables.
    solvedVars = [ (vv, unit) | (UnitVar vv, unit) <- unitAssignments ] ++
                 [ (vv, unit) | (UnitParamVarAbs (_, vv), unit) <- unitAssignments ]
    unitAssignments = genUnitAssignments cons

--------------------------------------------------

-- FIXME
approxEq :: Double -> Double -> Bool
approxEq a b = abs (b - a) < epsilon
epsilon :: Double
epsilon = 0.001 -- arbitrary

--------------------------------------------------

negateCons :: [UnitInfo] -> [UnitInfo]
negateCons = map (\ (UnitPow u k) -> UnitPow u (-k))

-- negatePosAbs :: UnitInfo -> UnitInfo
-- negatePosAbs (UnitPow (UnitParamPosAbs x) k) = UnitPow (UnitParamPosAbs x) (-k)
-- negatePosAbs u                               = u

--------------------------------------------------

-- Units that should appear on the right-hand-side of the equations during solving
isUnitRHS :: UnitInfo -> Bool
isUnitRHS (UnitPow (UnitName _) _)        = True
isUnitRHS (UnitPow (UnitParamEAPAbs _) _) = True
isUnitRHS _                               = False

type ShiftedConstraint = ([UnitInfo], [UnitInfo])
type ShiftedConstraints = [ShiftedConstraint]

-- | Shift UnitNames/EAPAbs poly units to the RHS, and all else to the LHS.
shiftTerms :: (UnitInfo -> Bool) -> ([UnitInfo], [UnitInfo]) -> ShiftedConstraint
shiftTerms isUnitRHS' (lhs, rhs) = (lhsOk ++ negateCons rhsShift, rhsOk ++ negateCons lhsShift)
  where
    (lhsOk, lhsShift)           = partition (not . isUnitRHS') lhs
    (rhsOk, rhsShift)           = partition isUnitRHS' rhs

-- | Translate all constraints into a LHS, RHS side of units.
flattenConstraints :: Constraints         -> [([UnitInfo], [UnitInfo])]
flattenConstraints = map (\ (ConEq u1 u2) -> (flattenUnits u1, flattenUnits u2))

--------------------------------------------------

-- type Z3 a   = Symbolic a
type Symbol = SInteger

-- type UnitZ3Map = M.Map (UnitInfo, UnitInfo) Symbol

type LhsUnit         = UnitInfo
type RhsUnit         = UnitInfo
type NameUnitInfoMap = M.Map String (LhsUnit, RhsUnit)
type NameSIntegerMap = M.Map String SInteger

gatherRhsUnitInfoNames :: [[UnitInfo]] -> [(String, RhsUnit)]
gatherRhsUnitInfoNames rhses
  | null rhsNames = [("bogus", UnitName "bogus")]
  | otherwise     = rhsNames

  where
    rhsNames = concatMap eachRow rhses

    eachRow  = map eachCol

    eachCol (UnitPow u _) = (show u, u)
    eachCol u             = (show u, u)

gatherLhsUnitInfoNames :: (String, RhsUnit) -> [[UnitInfo]] -> [(String, (LhsUnit, RhsUnit))]
gatherLhsUnitInfoNames (rhsName, rhsUnit) = concatMap eachRow
  where
    eachRow                               = map eachCol

    eachCol (UnitPow u _) = (show u ++ "_" ++ rhsName, (u, rhsUnit))
    eachCol u             = (show u ++ "_" ++ rhsName, (u, rhsUnit))

gatherNameUnitInfoMap :: [([UnitInfo], [UnitInfo])] -> NameUnitInfoMap
gatherNameUnitInfoMap shiftedCons = M.fromListWith (curry fst) lhsNames
  where
    lhsNames                      = concatMap (flip gatherLhsUnitInfoNames lhsRows) rhsNames
    lhsRows                       = map fst shiftedCons

    rhsNames = gatherRhsUnitInfoNames rhsRows
    rhsRows = map snd shiftedCons

-- | Map of RHS Names to initial powers (0). Forms the basis of the
-- solution for every unit variable.
type BasisMap = M.Map String Integer

genBasisMap :: ShiftedConstraints -> BasisMap
genBasisMap shiftedCons = baseRhsMap
  where
    rhsNames :: [(String, UnitInfo)]
    rhsNames = gatherRhsUnitInfoNames (map snd shiftedCons)

    -- start off with every RHS mapped to a power of zero.
    baseRhsMap = M.fromList [ (name, 0) | (name, _) <- rhsNames ]

genUnitAssignments :: Constraints -> [(UnitInfo, UnitInfo)]
genUnitAssignments cons = case engine cons of
  Left _ -> []
  Right (sub, _) -> subToList sub

basicOptimisations :: Constraints -> Constraints
basicOptimisations cons = cons'
  where
    cons' = filter (not . identicalSides) cons
    identicalSides (ConEq lhs rhs) = lhs == rhs
    identicalSides _               = False

type EngineResult = Either ([String], [(String, AugConstraint)]) (Sub, [UnitInfo])

-- main working function
engine :: Constraints -> EngineResult
engine cons = unsafePerformIO $ do
  let shiftedCons :: ShiftedConstraints
      shiftedCons = map (shiftTerms isUnitRHS) . flattenConstraints $ basicOptimisations cons

  let nameUIMap = gatherNameUnitInfoMap shiftedCons

  let genVar :: String -> Symbolic (String, SInteger)
      genVar name = (name,) <$> sInteger name

  -- basis of the solution, a.k.a. the primitive units specified by the user
  let basisMap = genBasisMap shiftedCons

  let pred :: Symbolic EngineResult
      pred = do
        setOption $ ProduceUnsatCores True
        -- pregenerate all of the necessary existentials
        nameSIntMap <- M.fromList <$> mapM genVar (M.keys nameUIMap)

        -- temporary arrangement for now to identify constraints
        let encCons = encodeConstraints basisMap nameUIMap nameSIntMap shiftedCons
        labeledCons <- forM (zip [(1::Int)..] encCons) $ \ (i, (sbool, augCon)) -> do
          namedConstraint ("c"++show i) sbool
          return ("c"++show i, augCon)

        query $ do
          -- obtain at least 1 name, value mapping for each variable if consistent
          e_nvMap <- computeInitialNVMap nameSIntMap
          case e_nvMap of
            Left core -> return $ Left (core, labeledCons) -- inconsistent
            Right nvMap -> do
              -- interpret the suggested values as a list of substitutions
              assignSubs <- interpret nameUIMap nvMap

              -- convert to Dim format
              let dims = map (\ (lhs, rhs) -> (dimFromUnitInfos (lhs ++ negateCons rhs))) shiftedCons

              -- apply known substitutions from solver
              let dims' = filter (not . isIdentDim) $ map (applySub assignSubs) dims

              -- convert to Constraint format
              let polyCons = map dimToConstraint dims'

              let criticals = MatrixBackend.criticalVariables polyCons

              -- feed back into old solver to figure out polymorphic equations
              let polyAssigns = MatrixBackend.genUnitAssignments polyCons

              -- convert polymorphic assignments into substitution format
              let polySubs = subFromList [ (u, dimFromUnitInfo units)
                                         | ([UnitPow u@(UnitParamVarAbs _) k], units) <- polyAssigns
                                         , k `approxEq` 1 ]


              -- for now we'll suggest all underdetermined units but
              -- this should be cut down by considering the
              -- relationships between variables, much like we would
              -- do for polymorphic vars.
              let suggests = [ v | v@(UnitVar {}) <- criticals ] ++
                             [ v | v@(UnitParamVarUse {}) <- criticals ] ++
                             [ v | v@(UnitParamPosAbs {}) <- criticals ]

              (return . Right . (,suggests) $ composeSubs polySubs assignSubs)

  runSMTWith z3 { transcript = Just "backend-sbv.smt2" } -- SMT-LIB dump
             pred

-- Assumes unitinfo was already simplified & flattened: extracts a
-- name and power
getUnitNamePow :: UnitInfo -> (String, Integer)
getUnitNamePow (UnitPow u p) = (uName, floor p * p')
  where (uName, p') = getUnitNamePow u
getUnitNamePow u = (show u, 1)

-- augmented constraint also includes the "RHS name"
type AugConstraint = (Constraint, String)

encodeConstraints :: BasisMap -> NameUnitInfoMap -> NameSIntegerMap -> ShiftedConstraints -> [(SBool, AugConstraint)]
encodeConstraints basisMap _ nameSIntMap shiftedCons = do
  let getLhsSymbol :: String -> UnitInfo -> (Symbol, Integer)
      getLhsSymbol rhsName (UnitPow u p) = (uSym, floor p * p')
        where (uSym, p') = getLhsSymbol rhsName u
      getLhsSymbol rhsName u = (s, 1)
        where n = show u ++ "_" ++ rhsName
              s = error ("missing variable for " ++ n) `fromMaybe` M.lookup n nameSIntMap

  -- for each RHS name and corresponding power build an equation of the form:
  --   lhs1_RHS * pow1 + lhs2_RHS * pow2 + ... + lhsN_RHS powN = pow_RHS
  let eachRhs :: Constraint -> [UnitInfo] -> (String, Integer) -> Maybe (SBool, AugConstraint)
      eachRhs con lhs (rhsName, rhsPow)
        | null lhsTerms = Just (0 .== literal rhsPow, (con, rhsName))
        | otherwise     = Just (sum lhsTerms .== literal rhsPow, (con, rhsName))
        where
          -- lhsTerms = [lhs1_RHS * pow1, lhs2_RHS * pow2, ..., lhsN_RHS powN]
          lhsTerms :: [SInteger]
          lhsTerms = [ lhsSym * literal lhsPow | lhs_i <- lhs
                                               , let (lhsSym, lhsPow) = getLhsSymbol rhsName lhs_i ]
          -- msg = intercalate " + " [ lhsName ++ "(" ++ rhsName ++ ") * " ++ show lhsPow
          --                         | lhs_i <- lhs
          --                         , let (lhsName, lhsPow) = getUnitNamePow lhs_i ] ++
          --       " == " ++ rhsName ++ " * " ++ show rhsPow

  -- for each constraint having a set of LHS terms and a set of RHS terms:
  let eachConstraint :: ([UnitInfo], [UnitInfo]) -> [(SBool, AugConstraint)]
      eachConstraint (lhs, rhs) = res
        where
          con       = ConEq (foldUnits lhs) (foldUnits rhs)
          -- msg       = "eachConstraint " ++ show (lhs, rhs) ++ " = " ++ show res
          res       = catMaybes . map (eachRhs con lhs) $ rhsPowers
          -- map every RHS to its corresponding power (including 0 for those not mentioned)
          rhsPowers = M.toList . M.unionWith (+) basisMap . M.fromListWith (+) . map getUnitNamePow $ rhs

  concatMap eachConstraint shiftedCons

-- showConstraints :: BasisMap -> ShiftedConstraints -> [String]
-- showConstraints basisMap = map mkMsg
--   where
-- --    mkMsg ([], rhs) = ""
--     mkMsg (lhs, rhs) = intercalate "\n" . filter (not . null) $ map (perRhs lhs) rhsPowers
--       where
--         rhsPowers = M.toList . M.unionWith (+) basisMap . M.fromListWith (+) . map getUnitNamePow $ rhs

--     perRhs lhs (rhsName, rhsPow) = msg
--       where
--         msg = intercalate " + " [ lhsName ++ "(" ++ rhsName ++ ") * " ++ show lhsPow
--                                 | lhs_i <- lhs
--                                 , let (lhsName, lhsPow) = getUnitNamePow lhs_i ] ++
--               " == " ++ rhsName ++ " * " ++ show rhsPow


data ValueInfo
  = VISet [Integer]
  | VISuggest
  | VIParametric Integer
  deriving (Show, Eq, Ord)

type NameValueInfoMap = M.Map String ValueInfo

computeInitialNVMap :: NameSIntegerMap -> Query (Either [String] NameValueInfoMap)
computeInitialNVMap nameSIntMap = do
  cs <- checkSat
  case cs of
    Unsat -> Left <$> getUnsatCore
    Sat -> do
      nvMap <- extractSIntValues nameSIntMap
      push 1
      disallowValues nameSIntMap nvMap
      cs' <- checkSat
      case cs' of
        Sat -> do
          nvMap' <- extractSIntValues nameSIntMap
          let nvMap'' = M.unionWith nvUnion nvMap nvMap'
          pop 1
          return $ Right nvMap''
        _   -> do
          pop 1
          return $ Right nvMap
    _ -> error "unknown"

-- identifyMultipleVISet :: NameUnitInfoMap -> NameValueInfoMap -> [UnitInfo]
-- identifyMultipleVISet nameUIMap = nub . map fst . catMaybes . map (`M.lookup` nameUIMap) . M.keys . M.filter isMultipleVISet

-- isMultipleVISet :: ValueInfo -> Bool
-- isMultipleVISet (VISet (_:_:_)) = True
-- isMultipleVISet _               = False

nvUnion :: ValueInfo -> ValueInfo -> ValueInfo
nvUnion (VISet xs) (VISet ys) = VISet . nub $ xs ++ ys
nvUnion x y = error $ "nvUnion on (" ++ show x ++ ", " ++ show y ++ ")"

extractSIntValues :: NameSIntegerMap -> Query NameValueInfoMap
extractSIntValues = (M.fromList <$>) . mapM convert . M.toList
  where convert (name, sInt) = ((name,) . VISet . (:[])) <$> getValue sInt

disallowValues :: NameSIntegerMap -> NameValueInfoMap -> Query ()
disallowValues nameSIntMap nvMap = constrain . sOr . catMaybes $ map mkNotEq (M.toList nvMap)
  where
    mkNotEq (name, VISet vs@(_:_))
      | Just sInt <- M.lookup name nameSIntMap = Just . sAnd $ map ((sInt ./=) . literal) vs
    mkNotEq _                                = Nothing

-- disallowCurrentValues :: NameSIntegerMap -> Query ()
-- disallowCurrentValues nameSIntMap = extractSIntValues nameSIntMap >>= disallowValues nameSIntMap

-- Interpret results.

-- The nameUIMap stores the mapping between each SInteger name and
-- its corresponding (lhsU, rhsU). Therefore we sort and group each
-- entry by its lhsU, and then check the solved integer value of the
-- SInteger name. That solved integer value corresponds to rhsU raised
-- to that power. Take all of the rhsUs, raised to their respective
-- powers, and combine them into a single UnitMul for each lhsU.

interpret :: NameUnitInfoMap -> NameValueInfoMap -> Query Sub
interpret nameUIMap nvMap = do
  let getLhsU = fst . snd
  let unitGroups = groupBy ((==) `on` getLhsU) . sortBy (comparing getLhsU) $ M.toList nameUIMap

  -- unitGroups =
  --   [ [(name1_1, (lhs1, rhs1)), (name1_2, (lhs1, rhs2)), ...]
  --   , [(name2_1, (lhs2, rhs1)), (name2_2, (lhs2, rhs2)), ...]
  --   , ...]

  let eachName :: (String, (LhsUnit, RhsUnit)) -> Query (Maybe UnitInfo)
      eachName (lhsName, (_, rhsU)) = do
        case M.lookup lhsName nvMap of
          Just (VISet [0]) -> return . Just $ UnitlessVar
          Just (VISet [x]) -> return . Just $ UnitPow rhsU (fromInteger x)
          _                -> return Nothing

  -- each group corresponds to a LHS variable
  let eachGroup :: [(String, (LhsUnit, RhsUnit))] -> Query (Maybe (LhsUnit, Dim))
      eachGroup unitGroup = do
        let (_, (lhsU, _)):_ = unitGroup -- grouped by lhsU, so pick out one of them
        rawUnits <- catMaybes <$> mapM eachName unitGroup
        case rawUnits of
          [] -> return Nothing
          _  -> return $ Just (lhsU, dimFromUnitInfos rawUnits)

  (subFromList . catMaybes) <$> mapM eachGroup unitGroups
