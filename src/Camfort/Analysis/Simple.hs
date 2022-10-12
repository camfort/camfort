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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

{- Simple syntactic analysis on Fortran programs -}

module Camfort.Analysis.Simple
 ( countVariableDeclarations
 , checkImplicitNone
 , ImplicitNoneReport(..)
 , checkAllocateStatements
 , checkFloatingPointUse
 , checkModuleUse
 , checkArrayUse )
where

import Prelude hiding (unlines)
import Control.Monad
import Control.DeepSeq
import Data.Data
import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Semigroup as SG
import Data.Monoid ((<>))
import Data.Generics.Uniplate.Operations
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text (unlines, intercalate, pack)
import Data.List (sort, nub, nubBy, tails)
import GHC.Generics
import qualified Data.List.NonEmpty as NE

import Data.Graph.Inductive

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis.SemanticTypes as FAS
import qualified Language.Fortran.Util.Position as F
import qualified Language.Fortran.Analysis as F
import qualified Language.Fortran.Analysis.DataFlow as F
import qualified Language.Fortran.Analysis.BBlocks as F
import Language.Fortran.Util.ModFile

import Camfort.Analysis (analysisModFiles,  ExitCodeOfReport(..), atSpanned, atSpannedInFile, Origin
                        , logError, describe, describeBuilder
                        , PureAnalysis, Describe )
import Camfort.Analysis.ModFile (withCombinedEnvironment)

{-| Counts the number of declarations (of variables) in a whole program -}

newtype VarCountReport = VarCountReport Int deriving Generic
instance NFData VarCountReport
instance ExitCodeOfReport VarCountReport where
  exitCodeOf _ = 0
instance Describe VarCountReport where
  describeBuilder (VarCountReport c) = Builder.fromText $ describe c

countVariableDeclarations :: forall a. Data a => F.ProgramFile a -> PureAnalysis () () VarCountReport
countVariableDeclarations pf = return . VarCountReport $ length (universeBi pf :: [F.Declarator a])

type PULoc = (F.ProgramUnitName, Origin)
data ImplicitNoneReport
  = ImplicitNoneReport [PULoc] -- ^ list of program units identified as needing implicit none
  deriving Generic
instance NFData ImplicitNoneReport
instance SG.Semigroup ImplicitNoneReport where
  ImplicitNoneReport r1 <> ImplicitNoneReport r2 = ImplicitNoneReport $ r1 ++ r2

instance Monoid ImplicitNoneReport where
  mempty = ImplicitNoneReport []
  mappend = (SG.<>)

-- If allPU is False then function obeys host scoping unit rule:
-- 'external' program units (ie. appear at top level) must have
-- implicit-none statements, which are then inherited by internal
-- subprograms. Also, subprograms of interface blocks must have
-- implicit-none statements. If allPU is True then simply checks all
-- program units. FIXME: when we have Fortran 2008 support then
-- submodules must also be checked.
checkImplicitNone :: forall a. Data a => Bool -> F.ProgramFile a -> PureAnalysis String () ImplicitNoneReport
checkImplicitNone allPU pf = do
  checkedPUs <- if allPU
                then sequence [ puHelper pu | pu <- universeBi pf :: [F.ProgramUnit a] ]
                     -- host scoping unit rule + interface exception:
                else sequence ( [ puHelper pu | pu <- childrenBi pf :: [F.ProgramUnit a] ] ++
                                [ puHelper pu | int@(F.BlInterface {}) <- universeBi pf :: [F.Block a]
                                              , pu <- childrenBi int :: [F.ProgramUnit a] ] )
  forM_ checkedPUs $ \ r -> case r of
     ImplicitNoneReport [(F.Named name, orig)] -> logError orig name
     _ -> return ()

  return $!! mconcat checkedPUs

  where
    isUseStmt (F.BlStatement _ _ _ (F.StUse {})) = True
    isUseStmt _ = False
    isComment (F.BlComment {}) = True
    isComment _ = False
    isUseOrComment b = isUseStmt b || isComment b

    isImplicitNone (F.BlStatement _ _ _ (F.StImplicit _ _ Nothing)) = True; isImplicitNone _ = False
    isImplicitSome (F.BlStatement _ _ _ (F.StImplicit _ _ (Just _))) = True; isImplicitSome _ = False

    checkPU :: F.ProgramUnit a -> Bool
    checkPU pu = case pu of
      F.PUMain _ _ _ bs _              -> checkBlocks bs
      F.PUModule _ _ _ bs _            -> checkBlocks bs
      F.PUSubroutine _ _ _ _ _ bs _    -> checkBlocks bs
      F.PUFunction _ _ _ _ _ _ _ bs _  -> checkBlocks bs
      _                                -> True

    checkBlocks bs = all (not . isImplicitSome) bs && all isUseOrComment useStmts && not (null rest) && all (not . isUseStmt) rest
      where
        (useStmts, rest) = break isImplicitNone bs

    puHelper pu
      | checkPU pu = return mempty
      | otherwise = fmap (\ o -> ImplicitNoneReport [(F.getName pu, o)]) $ atSpanned pu


instance Describe ImplicitNoneReport where
  describeBuilder (ImplicitNoneReport results)
    | null results = "no cases detected"
    | null (tail results) = "1 case detected"
    | otherwise = Builder.fromText $ describe (length results) <> " cases detected"

instance ExitCodeOfReport ImplicitNoneReport where
  exitCodeOf (ImplicitNoneReport []) = 0
  exitCodeOf (ImplicitNoneReport _)  = 1

--------------------------------------------------

data CheckAllocReport
  = CheckAllocReport { unbalancedAllocs :: [(F.Name, PULoc)]
                     , outOfOrder       :: [(F.Name, PULoc)]}
  deriving Generic
instance NFData CheckAllocReport
instance SG.Semigroup CheckAllocReport where
  CheckAllocReport a1 b1 <> CheckAllocReport a2 b2 = CheckAllocReport (a1 ++ a2) (b1 ++ b2)

instance Monoid CheckAllocReport where
  mempty = CheckAllocReport [] []
  mappend = (SG.<>)

checkAllocateStatements :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () CheckAllocReport
checkAllocateStatements pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf

  let checkPU :: F.ProgramUnit a -> CheckAllocReport
      checkPU pu = CheckAllocReport {..}
        where
          allocs =
            [ (v, (F.getName pu, atSpannedInFile file e))
            | F.StAllocate _ _ _ (F.AList _ _ es) _ <- universeBi (F.programUnitBody pu) :: [F.Statement a]
            , e <- es
            , v <- take 1 [ v | F.ExpValue _ _ (F.ValVariable v) <- universeBi e :: [F.Expression a] ]
            ]
          deallocs =
            [ (v, (F.getName pu, atSpannedInFile file e))
            | F.StDeallocate _ _ (F.AList _ _ es) _ <- universeBi (F.programUnitBody pu) :: [F.Statement a]
            , e <- es
            , v <- take 1 [ v | F.ExpValue _ _ (F.ValVariable v) <- universeBi e :: [F.Expression a] ]
            ]
          isDealloced v = not . null $ filter ((==v) . fst) deallocs
          unbalancedAllocs = filter (not . isDealloced . fst) allocs
          outOfOrder = concat $ zipWith (\ v1 v2 -> if fst v1 == fst v2 then [] else [v1, v2]) (nubBy ((==) `on` fst) allocs) (nubBy ((==) `on` fst) $ reverse deallocs)

  let reports = map checkPU (universeBi pf)

  return $!! mconcat reports


instance Describe CheckAllocReport where
  describeBuilder (CheckAllocReport {..})
    | null (unbalancedAllocs ++ outOfOrder) = "no cases detected"
    | otherwise = Builder.fromText . unlines $
      [ describe orig <> " unbalanced allocation or deallocation for " <> pack name
      | (name, (_, orig)) <- unbalancedAllocs ] ++
      [ describe orig <> " out-of-order (de)allocation " <> pack name
      | (name, (_, orig)) <- outOfOrder ]

instance ExitCodeOfReport CheckAllocReport where
  exitCodeOf (CheckAllocReport {..})
    | null (unbalancedAllocs ++ outOfOrder) = 0
    | otherwise = 1

--------------------------------------------------

data CheckFPReport
  = CheckFPReport { badEquality :: [PULoc] }
  deriving Generic
instance NFData CheckFPReport
instance SG.Semigroup CheckFPReport where
  CheckFPReport a1 <> CheckFPReport a2 = CheckFPReport (a1 ++ a2)

instance Monoid CheckFPReport where
  mempty = CheckFPReport []
  mappend = (SG.<>)

checkFloatingPointUse :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () CheckFPReport
checkFloatingPointUse pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf
  mfs <- analysisModFiles
  let (pf', _, _) = withCombinedEnvironment mfs pf
  let pvm         = combinedParamVarMap mfs
  let pf''        = F.analyseConstExps . F.analyseParameterVars pvm . F.analyseBBlocks $ pf'

  let checkPU :: F.ProgramUnit (F.Analysis a) -> CheckFPReport
      checkPU pu = CheckFPReport {..}
        where
          candidates :: [F.Expression (F.Analysis a)]
          candidates = [ e | e@(F.ExpBinary _ _ op x y) <- universeBi (F.programUnitBody pu)
                           , op `elem` [F.EQ, F.NE]
                           , Just (F.IDType (Just st) _) <- [F.idType (F.getAnnotation x), F.idType (F.getAnnotation y)]
                           , isFloatingPointType st ]
          badEquality = nub [ (F.getName pu, atSpannedInFile file e) | e <- candidates ]

  let reports = map checkPU (universeBi pf'')

  return $!! mconcat reports

isFloatingPointType :: FAS.SemType -> Bool
isFloatingPointType = \case
  FAS.TReal    _ -> True
  FAS.TComplex _ -> True
  _              -> False

instance Describe CheckFPReport where
  describeBuilder (CheckFPReport {..})
    | null (badEquality) = "no cases detected"
    | otherwise = Builder.fromText . unlines $
      [ describe orig <> " equality operation used on floating-point numbers."
      | (_, orig) <- badEquality ]

instance ExitCodeOfReport CheckFPReport where
  exitCodeOf (CheckFPReport {..})
    | null (badEquality) = 0
    | otherwise = 1

--------------------------------------------------

data CheckUseReport
  = CheckUseReport { missingOnly :: [PULoc]
                   , duppedOnly  :: [(String, PULoc)]
                   , unusedNames :: [(String, PULoc)]
                   }
  deriving Generic
instance NFData CheckUseReport
instance SG.Semigroup CheckUseReport where
  CheckUseReport a1 b1 c1 <> CheckUseReport a2 b2 c2 = CheckUseReport (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

instance Monoid CheckUseReport where
  mempty = CheckUseReport [] [] []
  mappend = (SG.<>)

checkModuleUse :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () CheckUseReport
checkModuleUse pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf
  mfs <- analysisModFiles
  let (pf', _, _) = withCombinedEnvironment mfs pf

  let checkPU :: F.ProgramUnit (F.Analysis a) -> CheckUseReport
      checkPU pu = CheckUseReport {..}
        where
          statements :: [F.Statement (F.Analysis a)]
          statements  = universeBi (F.programUnitBody pu)
          expressions :: [F.Expression (F.Analysis a)]
          expressions = universeBi pu
          missingOnly = nub [ (F.getName pu, atSpannedInFile file s)
                            | s <- [ s | s@(F.StUse _ _ _ _ F.Permissive _) <- statements ] ]
          duppedOnly  = [ (n, (F.getName pu, atSpannedInFile file $ F.getSpan (ss, ss')))
                        | F.StUse _ ss (F.ExpValue _ _ (F.ValVariable n)) _ _ _:rest <- tails statements
                        , F.StUse _ ss' (F.ExpValue _ _ (F.ValVariable n')) _ _ _ <- rest
                        , n == n' ]
          extractUseName (F.UseID _ ss (F.ExpValue _ _ (F.ValVariable n)))       = (n, ss)
          extractUseName (F.UseRename _ ss (F.ExpValue _ _ (F.ValVariable n)) _) = (n, ss)
          extractUseName u = error $ "checkModuleUse: extractUseName: invalid AST: " ++ show (fmap (const ()) u)
          unusedNames = [ (n, (F.getName pu, atSpannedInFile file ss))
                        | F.StUse _ _ _ _ _ (Just (F.AList _ _ uses)) <- statements
                        , (n, ss) <- map extractUseName uses
                        , null (drop 1 [ () | F.ExpValue _ _ (F.ValVariable n') <- expressions, n == n' ]) ]

  let reports = map checkPU (universeBi pf')

  return $!! mconcat reports

instance Describe CheckUseReport where
  describeBuilder (CheckUseReport {..})
    | null missingOnly && null duppedOnly && null unusedNames = "no cases detected"
    | otherwise = Builder.fromText . unlines $
      [ describe orig <> " USE statement missing ONLY attribute."
      | (_, orig) <- missingOnly ] ++
      [ describe orig <> " multiple USE statements for same module '" <> describe name <> "'"
      | (name, (_, orig)) <- duppedOnly ] ++
      [ describe orig <> " local name '" <> describe name <> "' imported but unused in program unit."
      | (name, (_, orig)) <- unusedNames ]

instance ExitCodeOfReport CheckUseReport where
  exitCodeOf (CheckUseReport {..})
    | null missingOnly && null duppedOnly && null unusedNames = 0
    | otherwise = 1

--------------------------------------------------

data CheckArrayReport
  = CheckArrayReport { nestedIdx, missingIdx :: [([String], PULoc)] }
  deriving Generic
instance NFData CheckArrayReport
instance SG.Semigroup CheckArrayReport where
  CheckArrayReport a1 b1 <> CheckArrayReport a2 b2 = CheckArrayReport (a1 ++ a2) (b1 ++ b2)

instance Monoid CheckArrayReport where
  mempty = CheckArrayReport [] []
  mappend = (SG.<>)

-- Look at array subscripting, especially with regard to order of
-- indices and perhaps whether there is a missing data dependency on
-- an index variable (e.g. due to copy/paste error).
checkArrayUse :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () CheckArrayReport
checkArrayUse pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf
  mfs <- analysisModFiles
  let (pf', _, _) = withCombinedEnvironment mfs pf
  let pvm         = combinedParamVarMap mfs
  let pf''        = F.analyseConstExps . F.analyseParameterVars pvm . F.analyseBBlocks $ pf'
  let bm          = F.genBlockMap pf''
  let dm          = F.genDefMap bm

  let checkPU :: F.ProgramUnit (F.Analysis a) -> CheckArrayReport
      checkPU pu | F.Analysis { F.bBlocks = Just _ } <- F.getAnnotation pu = CheckArrayReport {..}
        where
          F.Analysis { F.bBlocks = Just gr } = F.getAnnotation pu
          bedges = F.genBackEdgeMap (F.dominators gr) $ F.bbgrGr gr
          ivmap  = F.genInductionVarMapByASTBlock bedges gr
          rdmap  = F.reachingDefinitions dm gr
          flTo   = F.genFlowsToGraph bm dm gr rdmap
          flFrom = grev flTo

          blocks :: [F.Block (F.Analysis a)]
          blocks = F.programUnitBody pu

          nestedIdx = [ (ivars, (F.getName pu, atSpannedInFile file ss)) | (ivars, ss) <- getNestedIdx [] blocks ]

          -- find subscripts where the order of indices doesn't match
          -- the order of introduction of induction variables by
          -- nested do-loops.
          getNestedIdx :: [F.Name] -> [F.Block (F.Analysis a)] -> [([String], F.SrcSpan)]
          getNestedIdx _ [] = []
          getNestedIdx vs (b@(F.BlDo _ _ _ _ _ _ body _):bs)
            | v:_ <- F.blockVarDefs b = getNestedIdx (v:vs) body ++ getNestedIdx vs bs
            | otherwise               = getNestedIdx vs bs
          getNestedIdx vs (b:bs) = bad ++ getNestedIdx vs bs
            where
              vset = S.fromList vs
              vmap = zip vs ([0..] :: [Int])
              subs = [ (ivars, ss)
                     | F.ExpSubscript _ ss _ (F.AList _ _ is) <- universeBi b :: [F.Expression (F.Analysis a)]
                     , let ivars = [ (F.varName e, F.srcName e)
                                   | i <- is
                                   , e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi i :: [F.Expression (F.Analysis a)]
                                   , F.varName e `S.member` vset ] ]
              -- 'bad' subscripts are where the ordering doesn't match the nesting.
              bad = [ (map snd ivars, ss)
                    | (ivars, ss) <- subs, let nums = mapMaybe (flip lookup vmap . fst) ivars, nums /= sort nums ]

          missingIdx = [ (missing, (F.getName pu, atSpannedInFile file ss)) | b <- blocks
                                                                            , (missing, ss) <- getMissingUse [] b ]

          -- Seek any possible missing uses of an induction variable
          -- within subscript expressions, looking through nested
          -- blocks. We can exclude variables for which If/Case
          -- control-flow depends on it instead of needing it to be
          -- directly used by a subscript expression.
          getMissingUse :: forall a'. Data a' => [String] -> F.Block (F.Analysis a') -> [([String], F.SrcSpan)]
          getMissingUse excls (F.BlDo _ _ _ _ _ _ bs _)      = concatMap (getMissingUse excls) bs
          getMissingUse excls (F.BlDoWhile _ _ _ _ _ _ bs _) = concatMap (getMissingUse excls) bs
          getMissingUse excls (F.BlForall _ _ _ _ _ bs _)    = concatMap (getMissingUse excls) bs
          getMissingUse excls b@(F.BlIf F.Analysis{F.insLabel = Just i} _ss _l _nm clauses mBlockElse _ml)
            -- check If statement conditions
            | any (eligible i (length excls)) conds = bads ++ rest
            | otherwise                             = rest
            where
              clauses' = NE.toList clauses
              conds  = map fst clauses'
              blocks = mcons mBlockElse $ map snd clauses'
              -- find any induction variables that are referenced by If-Elseif expressions
              excl' = getExcludes b
              rest = concatMap (getMissingUse (excls ++ excl')) $ concat blocks
              bads = getMissingUse' excls b
          getMissingUse excls b@(F.BlCase F.Analysis{F.insLabel = Just i} _ _ _ e _ bss _)
            -- check Case statement scrutinee
            | eligible i (length excls) e = bads ++ rest
            | otherwise                   = rest
            where
              rest = concatMap (getMissingUse (excls ++ getExcludes b)) $ concat bss
              bads = map (fmap (const (F.getSpan e))) $ getMissingUse' excls b
          getMissingUse excls b@(F.BlStatement F.Analysis{F.insLabel = Just i} _ _ st)
            | eligible i (length excls) st = getMissingUse' excls b
            | otherwise                    = []
          getMissingUse _ F.BlInterface{}  = []
          getMissingUse _ F.BlComment{}    = []
          getMissingUse _ b = error $ "checkArrayUse: getMissingUse: missing insLabel: " ++ show (fmap F.insLabel b)

          getMissingUse' :: forall a'. Data a' => [F.Name] -> F.Block (F.Analysis a') -> [([F.Name], F.SrcSpan)]
          getMissingUse' excls b
            | Just i            <- F.insLabel (F.getAnnotation b)
            -- obtain the live induction variables at this program point
            , Just ivarSet      <- IM.lookup i ivmap
            -- find the definitions that flowed into this program point
            , flFroms           <- drop 1 $ bfs i flFrom
            -- get their AST Blocks
            , Just flFromBlocks <- sequence $ map (flip IM.lookup bm) flFroms
            -- find out what variables they define
            , flFromBlockDefSet <- S.fromList $ concatMap F.blockVarDefs flFromBlocks
            -- subtract the excludes and the defined variables from
            -- the live induction variables in order to find out the
            -- 'missing' or unaccounted-for induction vars.
            , missingIVars      <- ivarSet `S.difference` S.fromList excls `S.difference` flFromBlockDefSet
            -- Try to look up source-names for the missing ivars in
            -- each of the flow-from blocks.
            , missingIVars'     <- S.map (\ v -> "unk" `fromMaybe` findSrcNameInDefMap v) missingIVars
            , not (S.null missingIVars) = [(S.toList missingIVars', F.getSpan b)]
          getMissingUse' _ _ = []

          -- eligible bits of AST are those that contain subscripting
          -- expressions with a length equivalent to the number of
          -- currently live induction variables.
          eligible :: forall a' b'. (Data a', Data (b' (F.Analysis a'))) => Int -> Int -> b' (F.Analysis a') -> Bool
          eligible i numExcls x
            | Just ivars <- IM.lookup i ivmap =
                not $ null [ () | F.ExpSubscript _ _ _ (F.AList _ _ idxs) <- universeBi x :: [F.Expression (F.Analysis a')]
                                , length idxs == S.size ivars - numExcls ]
            | otherwise = False

          -- check the derived induction maps to find out if a given
          -- expression depends on any induction variable and
          -- therefore can be used to exclude that induction variable
          -- from the 'missing' list.
          getExcludes b
            | Just i            <- F.insLabel (F.getAnnotation b)
            -- obtain the live induction variables at this program point
            , Just ivarSet      <- IM.lookup i ivmap
            -- find the definitions that flowed into this program point
            , flFroms           <- drop 1 $ bfs i flFrom
            -- get their AST Blocks
            , Just flFromBlocks <- sequence $ map (flip IM.lookup bm) flFroms
            -- find out what variables they define
            , flFromBlockDefSet <- S.fromList $ concatMap F.blockVarDefs flFromBlocks
            = S.toList (ivarSet `S.intersection` flFromBlockDefSet)
            | otherwise = []

          -- look through the DefMap and BlockMap for instances of
          -- variable v in order to retrieve its 'source name'.
          findSrcNameInDefMap v = do
            defSet <- M.lookup v dm
            bs     <- mapM (flip IM.lookup bm) $ IS.toList defSet
            msum (map (findSrcName v) bs)

      checkPU _ = mempty
  let reports = map checkPU (universeBi pf'')

  return $!! mconcat reports

-- | /O(1)/. Maybe cons. Borrowed from Agda.
--   @mcons ma as = maybeToList ma ++ as@
mcons :: Maybe a -> [a] -> [a]
mcons ma as = maybe as (:as) ma

-- Look through a piece of AST for the source name of a given var name.
findSrcName :: forall a. Data a => F.Name -> F.Block (F.Analysis a) -> Maybe F.Name
findSrcName v b = listToMaybe
  [ F.srcName e | e@(F.ExpValue _ _ F.ValVariable{}) <- universeBi b :: [F.Expression (F.Analysis a)]
                , F.varName e == v ]

instance Describe CheckArrayReport where
  describeBuilder (CheckArrayReport {..})
    | null nestedIdx && null missingIdx = "no cases detected"
    | otherwise = Builder.fromText . unlines $
      [ describe orig <> " possibly less efficient order of subscript indices: " <> intercalate ", " (map describe ivars)
      | (ivars, (_, orig)) <- nestedIdx ] ++
      [ describe orig <> " possibly missing use of variable(s) in array subscript: " <> intercalate ", " (map describe ivars)
      | (ivars, (_, orig)) <- missingIdx ]

instance ExitCodeOfReport CheckArrayReport where
  exitCodeOf (CheckArrayReport {..})
    | null nestedIdx = 0
    | otherwise = 1
