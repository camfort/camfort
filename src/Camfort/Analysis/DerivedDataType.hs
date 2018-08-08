{-
   Copyright 2018, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Analysis.DerivedDataType
       ( infer, refactor, check, synth, compile
       , DerivedDataTypeReport(..)
       ) where
import Prelude hiding (unlines)
import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Strict
import GHC.Generics (Generic)
import Control.Arrow (first, second, (&&&))
import Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (fromJust, maybeToList, isJust)
import qualified Data.Strict.Either as SE
import Data.List (sort, foldl')
import qualified Data.List as List
import Data.Text (Text, unlines, intercalate, pack)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Monoid ((<>))
import Control.Lens
import qualified Data.Semigroup as SG
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Util.ModFile

import Camfort.Analysis
import Camfort.Analysis.ModFile
import Camfort.Analysis.Annotations (onPrev, buildCommentText, A, Annotation(..))
import Camfort.Analysis.CommentAnnotator (annotateComments, ASTEmbeddable(..), Linkable(..))
import Camfort.Analysis.DerivedDataType.Parser (ddtParser, DDTStatement(..))
import Camfort.Helpers (Filename)

--------------------------------------------------
-- ddt-infer reporting infra

-- | map of information about constants used to index arrays
type AMap = M.Map F.Name (IM.IntMap (S.Set (Maybe Int)))

-- | map of info about vars
type VMap = M.Map F.Name (S.Set VInfo)

-- | map of specs about (var, dim)s
type SMap = M.Map (F.Name, Int) (S.Set Essence)

-- | info about a variable: source name, source filename, source span
data VInfo = VInfo { vSrcName :: F.Name, vFileName :: String, vSrcSpan :: FU.SrcSpan }
  deriving (Generic, Show, Eq)

instance Ord VInfo where
  VInfo s1 f1 ss1 `compare` VInfo s2 f2 ss2 = (f1, ss1, s1) `compare` (f2, ss2, s2)

instance Binary VInfo

data DerivedDataTypeReport
  = DerivedDataTypeReport { ddtrAMap :: AMap
                          , ddtrVMap :: VMap
                          , ddtrSMap :: SMap
                          , ddtrIDE  :: (S.Set IndexDupError)
                          , ddtrCE   :: ConflictErrors
                          , ddtrBLE  :: BadLabelErrors
                          , ddtrBDE  :: BadDimErrors }
  deriving (Generic)

instance Binary DerivedDataTypeReport

instance SG.Semigroup DerivedDataTypeReport where
  DerivedDataTypeReport m1 v1 s1 ide1 ce1 ble1 bde1 <> DerivedDataTypeReport m2 v2 s2 ide2 ce2 ble2 bde2 =
    DerivedDataTypeReport (M.unionWith (IM.unionWith S.union) m1 m2) (M.unionWith S.union v1 v2) newSMap
                          (S.union ide1 ide2) (M.unionsWith S.union [ce1, ce2, newCE])
                          (M.unionWith S.union ble1 ble2)
                          (M.unionWith S.union bde1 bde2)
    where
      newCE   = M.map SE.fromLeft $ M.filter SE.isLeft e_smap
      newSMap = M.map (S.singleton . SE.fromRight) $ M.filter SE.isRight e_smap

      -- bring together the SMaps while looking for new conflicts
      e_smap = M.fromListWith combine [ (v, SE.Right e) | (v, eSet) <- M.toList s1 ++ M.toList s2, e <- S.toList eSet ]
      combine (SE.Left e1) _              = SE.Left e1
      combine _ (SE.Left e2)              = SE.Left e2
      combine (SE.Right e1) (SE.Right e2) = combineEssences e1 e2

-- | Combine compatible essences (SE.Right) or else indicate conflict (SE.Left)
combineEssences :: Essence -> Essence -> SE.Either (S.Set Essence) Essence
combineEssences e1@(Essence ty1 labMap1 vinfoSet1) e2@(Essence ty2 labMap2 vinfoSet2)
  | ty1 == ty2
  , mlabMap <- IM.unionWith (\ l1 l2 -> if l1 == l2 then l1 else Nothing) (IM.map Just labMap1) (IM.map Just labMap2)
  , all isJust (IM.elems mlabMap)
  = SE.Right $ Essence ty1 (IM.map fromJust mlabMap) (S.union vinfoSet1 vinfoSet2)
  | otherwise
  = SE.Left $ S.fromList [e1, e2]

instance Monoid DerivedDataTypeReport where
  mempty = DerivedDataTypeReport M.empty M.empty M.empty S.empty M.empty M.empty M.empty
  mappend = (SG.<>)

successful r = and [S.null (ddtrIDE r), M.null (ddtrCE r), M.null (ddtrBLE r), M.null (ddtrBDE r)]

instance ExitCodeOfReport DerivedDataTypeReport where
  exitCodeOf r
    | successful r = 0
    | otherwise    = 1

instance Describe DerivedDataTypeReport where
  describeBuilder r@(DerivedDataTypeReport amap vmap smap ide ce ble bde)
    | not (successful r) = Builder.fromText errorReport
    | M.null smap = "no cases detected"
    | otherwise = Builder.fromText specReport
    where
      specReport = unlines . concat $
        [ ("\n"<>pack fileName<>":\n"):
          [ describe ss <> " ddt " <> pack ty <> "(" <> describeLabels labs <> ") :: " <>
            pack src <> "(dim=" <> describe dim <> ")" | (VInfo src _ ss, ty, labs, dim) <- sort fileStuff ]
        | (fileName, fileStuff) <- M.toList byFile ]
        where
          byFile = M.fromListWith (++) stuff
          stuff = [ (vFileName vinfo, [(vinfo, ty, labs, dim)])
                  | ((v, dim), essenceSet) <- M.toList smap
                  , Essence ty labs vinfoSet <- S.toList essenceSet
                  , vinfo <- S.toList vinfoSet ]

      ideLines = [ (vinfo, [pack ty <> " has duplicated indice(s) [" <>
                            intercalate ", " (map describe ints) <> "] for variable: " <> pack (vSrcName vinfo)])
                 | IndexDupError ty vinfo ints <- S.toList ide ]
      ceLines  = [ (vinfo, [pack ty0 <> "(" <> describeLabels l0 <> ") :: " <> pack (vSrcName vinfo) <> "(dim=" <>
                            describe dim <> ")\nconflicts with\n" <>
                            unlines [ pack fn <> ": " <> describe ss <> " " <> pack ty <> "(" <> describeLabels labs <> ")"
                                    | Essence ty labs vinfoSet <- essences
                                    , VInfo src fn ss <- S.toList vinfoSet ]])
                 | ((_, dim), essenceSet) <- M.toList ce
                 , not (S.null essenceSet)
                 , let Essence ty0 l0 vinfoSet0:essences = S.toList essenceSet
                 , vinfo <- take 1 (S.toList vinfoSet0) ]
      bleLines = [ (vinfo, ["duplicated label '" <> pack lab <> "', associated with variable " <>
                            pack (vSrcName vinfo) <> "(dim=" <> describe dim <> ")"])
                 | ((_, dim), badSet) <- M.toList ble
                 , (lab, vinfo) <- S.toList badSet ]
      bdeLines = [ (vinfo, [pack (vSrcName vinfo) <> ": bad dim " <> describe dim <>
                            if maxDim == 0 then " less than 1"
                            else " not in range 1.." <> describe maxDim])
                 | ((_, dim), badSet) <- M.toList bde
                 , (vinfo, maxDim) <- S.toList badSet ]

      errorReport = linesByFile $ ideLines ++ ceLines ++ bleLines ++ bdeLines
      describeLabels labs = intercalate ", " [describe i <> "=>" <> pack l | (i,l) <- IM.toList labs]

linesByFile :: [(VInfo, [Text])] -> Text
linesByFile vinfoTexts = unlines $ concat [ ("\n"<>pack fileName<>":\n") : [ describe ss <> " " <> text
                                                                           | (ss, text) <- sort sstexts ]
                                          | (fileName, sstexts) <- M.toList mapByFile ]
  where
    mapByFile = M.fromListWith (++) [ (vFileName vinfo, map (vSrcSpan vinfo,) texts) | (vinfo, texts) <- vinfoTexts ]


--------------------------------------------------
-- ddt-check reporting infrastructure

-- 'essence' of a specification
-- type name, map of int => label, variable info set
data Essence = Essence String (IM.IntMap String) (S.Set VInfo)
  deriving (Show, Generic)

instance Binary Essence

instance Eq Essence where
  Essence ty1 l1 _ == Essence ty2 l2 _ = (ty1, l1) == (ty2, l2)

instance Ord Essence where
  Essence ty1 l1 _ `compare` Essence ty2 l2 _ = (ty1, l1) `compare` (ty2, l2)

-- given type name, (source name, span), dupped labels
data IndexDupError = IndexDupError String VInfo [Int]
  deriving (Show, Eq, Ord, Generic)

instance Binary IndexDupError

type ConflictErrors = M.Map (F.Name, Int) (S.Set Essence) -- variable => essence set

type BadLabelErrors = M.Map (F.Name, Int) (S.Set (String, VInfo))  -- variable => (label, var info) set

type BadDimErrors = M.Map (F.Name, Int) (S.Set (VInfo, Int))

--------------------------------------------------
-- Linking DDT specifications with associated AST-blocks.

type DA = FA.Analysis DDTAnnotation
data DDTAnnotation = DDTAnnotation {
    prevAnnotation :: A,
    ddtSpec       :: Maybe DDTStatement,
    ddtBlock      :: Maybe (F.Block DA), -- ^ linked variable declaration
    ddtPU         :: Maybe (F.ProgramUnit DA) -- ^ linked program unit
  } deriving (Data, Typeable, Show)
ddtAnnotation0 a = DDTAnnotation a Nothing Nothing Nothing

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable DA DDTStatement where
  annotateWithAST ann ast =
    onPrev (\ann' -> ann' { ddtSpec = Just ast }) ann

-- Link annotation comments to declaration statements
instance Linkable DA where
  link ann (b@(F.BlStatement _ _ _ F.StDeclaration {})) =
      onPrev (\ann' -> ann' { ddtBlock = Just b }) ann
  link ann _ = ann
  linkPU ann pu@F.PUFunction{} =
      onPrev (\ann' -> ann' { ddtPU = Just pu }) ann
  linkPU ann pu@F.PUSubroutine{} =
      onPrev (\ann' -> ann' { ddtPU = Just pu }) ann
  linkPU ann _ = ann

--------------------------------------------------
-- interfaces

-- | Generate report about derived datatypes in given program file
infer :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
infer pf = do
  mfs <- analysisModFiles
  let (report, pf') = genProgramFileReport mfs pf
  return report

-- | Check annotations relating to derived datatypes in given program file
check :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
check pf = do
  mfs <- analysisModFiles
  return . fst $ genProgramFileReport mfs pf

-- | Generate and insert comments about derived datatypes
synth :: Char -> [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
synth marker pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf'@(F.ProgramFile mi _)) = genProgramFileReport mfs pf
    let pf'' = descendBi (synthBlocks (mi, marker) report) pf'
    -- FIXME: generate datatype names, labels & insert comments
    return $ (report, Right (fmap (prevAnnotation . FA.prevAnnotation) pf''))

-- | Refactor derived datatypes based on marked comments
refactor :: [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
refactor pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf') = genProgramFileReport mfs pf
    -- FIXME: gather marked comments and apply
    return $ (report, Right pf)

-- | Compile a program to a 'ModFile' containing derived datatype information.
compile :: () -> ModFiles -> F.ProgramFile A -> IO ModFile
compile _ mfs pf = do
  let (report, pf') = genProgramFileReport mfs pf
  return $ genDDTModFile pf' report

--------------------------------------------------
-- Analysis helpers

-- forEachProgramFile :: [F.ProgramFile A] -> (F.ProgramFile A -> PureAnalysis String () (DerivedDataTypeReport, Either String (F.ProgramFile A))) -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
forEachProgramFile :: (Monad m, Monoid r) => [a] -> (a -> m (r, b)) -> m (r, [b])
forEachProgramFile pfs f = do
  results <- mapM f pfs
  return (foldl' (<>) mempty (map fst results), map snd results)

-- | Used to represent an array and any global (non-induction)
-- variables used to index it
data BulkDataArrayInfo = Array
  {
    arrayName  :: F.Name
  -- , declPos    :: FU.Position
  , srcSpan :: FU.SrcSpan
  , indexInfo :: [(Int, Maybe Int)]
  }
  deriving (Show, Eq)

genProgramFileReport :: ModFiles -> F.ProgramFile A -> (DerivedDataTypeReport, F.ProgramFile DA)
genProgramFileReport mfs (pf@(F.ProgramFile F.MetaInfo{ F.miFilename = srcFile } _)) = (report, pf')
  where
    (analysisOutput, pf', tenv) = analysis mfs pf
    amap = filterCondensedCategoryOne $ condenseCategoryOne analysisOutput
    vars = S.fromList $ M.keys amap
    vls1 = [ (v, S.singleton $ VInfo (FA.srcName e) srcFile ss)
           | F.DeclArray _ ss e _ _ _ <- universeBi pf' :: [F.Declarator DA]
           , let v = FA.varName e
           , v `S.member` vars ]
    vls2 = [ (v, S.singleton $ VInfo (FA.srcName e) srcFile ss)
           | F.DeclVariable _ ss e _ _ <- universeBi pf' :: [F.Declarator DA]
           , let v = FA.varName e
           , v `S.member` vars ]
    vmap = M.fromListWith S.union $ vls1 ++ vls2

    specs = [ (spec, b) | DDTAnnotation { ddtSpec = Just spec, ddtBlock = Just b } <- universeBi pf' ]
    -- boil down the parsed specs
    e_essences = [ essence | (spec@DDTSt { ddtStVarDims = vardims } , b) <- specs
                           , (var, dim) <- vardims
                           , (declVarName, vinfo) <- declaredVars srcFile b
                           , vSrcName vinfo == var
                           , let essence = ((declVarName, dim), distil spec vinfo) ]
    (errors, essences) = List.partition (SE.isLeft . snd) e_essences
    indexErrors = S.fromList $ map (SE.fromLeft . snd) errors
    essences' :: M.Map (F.Name, Int) (S.Set Essence)
    essences' = M.fromListWith S.union $ map (second (S.singleton . SE.fromRight)) essences
    -- conflicting specs
    conflicts = M.filter ((>1) . S.size) essences'
    -- dupped labels
    f (Essence _ labMap vinfoSet) = [ (lab, vinfo) | lab <- labs, vinfo <- vinfos ]
      where
        vinfos = S.toList vinfoSet
        labs = M.keys . M.filter (>1) . M.fromListWith (+) $ map (,1) (IM.elems labMap)
    badLabels = M.filter (not . null) $ M.map (S.fromList . concatMap f . S.toList) essences'

    -- badly specified 'dim' attributes
    badDims = [ ((declVarName, dim), S.singleton (vinfo, maxDim))
              | (spec@DDTSt { ddtStVarDims = vardims } , b) <- specs
              , (srcName, dim) <- vardims
              , (declVarName, vinfo) <- declaredVars srcFile b
              , vSrcName vinfo == srcName
              , Just maxDim <- [ do guard (dim < 1)
                                    return 0  -- stand-in for 'dim violates the lower bound' case
                                 <|>
                                 do FA.IDType { FA.idCType = Just (FA.CTArray dims) } <- M.lookup declVarName tenv
                                    let maxDim = length dims
                                    guard $ not (null dims || dim <= maxDim)
                                    -- there is a known upper-bound and dim violates it
                                    return maxDim ] ]

    -- distil specs from the inferred amap
    smapFromAMap = M.fromList [ ((v, dim), essenceSet) | (v, aminfoMap) <- M.toList amap
                                                       , (dim, aminfo) <- IM.toList aminfoMap
                                                       , all isJust (S.toList aminfo)
                                                       , Just vinfoSet <- [M.lookup v vmap]
                                                       , let essence = distilArrayInfo v vinfoSet (S.map fromJust aminfo)
                                                       , let essenceSet = S.singleton essence ]
    -- specs in the file override inferred specs
    smap = M.unionWith (curry fst) essences' smapFromAMap
    ide  = indexErrors
    ce   = conflicts
    ble  = badLabels
    bde  = M.fromList badDims
    report = DerivedDataTypeReport amap vmap smap ide ce ble bde <> combinedDerivedDataTypeReport mfs

-- analyse bulk data info
analysis :: ModFiles -> F.ProgramFile A -> ([(String, BulkDataArrayInfo)], F.ProgramFile DA, FAT.TypeEnv)
analysis mfs pf = (accessInfo, linkedPF, tenv)
  where
     filename       = F.pfGetFilename pf
     (pf', _, tenv) = withCombinedEnvironment mfs (fmap ddtAnnotation0 pf)
     pf''           = FAD.analyseConstExps $ FAB.analyseBBlocks pf'

     perArray :: [F.Index DA] -> [(Int, Maybe Int)]
     perArray is = [ (n, do F.IxSingle _ _ _ e <- Just ix
                            FAD.ConstInt i <- FA.constExp (F.getAnnotation e)
                            return (fromIntegral i))
                   | (n, ix) <- zip [1..] is ]
     accessInfo =
       [ (filename, Array (FA.varName a) ss $ perArray (F.aStrip is))
       | F.ExpSubscript _ ss a@(F.ExpValue _ _ (F.ValVariable _)) is <- universeBi pf'' ]

     (linkedPF, _) = runWriter $ annotateComments ddtParser
       (\ srcSpan err -> tell $ "Error " ++ show srcSpan ++ ": " ++ show err) pf''

-- convert analysis info into a map of information about the constants used to index arrays
condenseCategoryOne :: [(Filename, BulkDataArrayInfo)] -> AMap
condenseCategoryOne c1 = amap
  where
    amap = M.fromListWith (IM.unionWith S.union) [ (arrayName bdai, doIndices (indexInfo bdai)) | (_, bdai) <- c1 ]
    doIndices is = IM.fromList $ map (fmap S.singleton) is

-- filter only the interesting ones: at least one of the parameters
-- was populated only by a range of constants where each constant is
-- no more than 3 away from the adjacent ones.
filterCondensedCategoryOne :: AMap -> AMap
filterCondensedCategoryOne = M.filter (not . IM.null . IM.filter valid) -- accept if at least one valid parameter
  where
    diffs = uncurry (zipWith subtract) . (id &&& drop 1) . sort -- compute differences between consecutive numbers
    -- valid set if...
    valid = and . sequence [ not . null                       -- (1) non-empty list
                           , all isJust                       -- (2) of constants only, no wildcards
                           , all (< 3) . diffs . map fromJust -- (3) no more than 3 away from adjacent constants
                           ] . S.toList

--------------------------------------------------
-- Synthesis helpers

synthBlocks :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> [F.Block DA] -> [F.Block DA]
synthBlocks marker report = concatMap (synthBlock marker report)

synthBlock :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> F.Block DA -> [F.Block DA]
synthBlock (mi, marker) DerivedDataTypeReport { ddtrAMap = amap } b = case b of
  F.BlStatement a ss _ F.StDeclaration{}
    | vars <- ofInterest b -> genComment vars ++ [b]
    where
      ofInterest b = filter (flip M.member amap . fst) $
        [ (FA.varName e, FA.srcName e) | F.DeclVariable _ _ e _ _ <- universeBi b :: [F.Declarator DA] ] ++
        [ (FA.varName e, FA.srcName e) | F.DeclArray _ _ e _ _ _ <- universeBi b :: [F.Declarator DA] ]

      genComment = map $ \ var ->
        F.BlComment newA newSS . F.Comment . buildCommentText mi space $ marker:genCommentText amap var

      -- drill down two levels of annotation to set the refactored flag
      newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                    prevAnnotation = (prevAnnotation (FA.prevAnnotation a)) {
                        refactored = Just lp } } }
      newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
      FU.SrcSpan lp _ = ss
      space = FU.posColumn lp - 1
  _ -> [b]

-- Generate the text that goes into the specification.
genCommentText amap (v, s)
  | Just pmap <- M.lookup v amap
  , ty <- v ++ "_type"
  , (dim, set):_ <- IM.toList $ IM.filter (all isJust . S.toList) pmap
  , nums <- map fromJust $ S.toList set
  , labs <- List.intercalate ", " [ str ++ "=>" ++ "label" ++ str | num <- nums, let str = show num ] =
      " ddt " ++ ty ++ "(" ++ labs ++ ") :: " ++ s ++ "(dim=" ++ show dim ++ ")"

--------------------------------------------------
-- Check helpers

declaredVars :: String -> F.Block DA -> [(F.Name, VInfo)]
declaredVars srcFile x =
  [ (FA.varName e, VInfo (FA.srcName e) srcFile (FU.getSpan e))
  | F.DeclVariable _ _ e _ _  <- universeBi x :: [F.Declarator DA]] ++
  [ (FA.varName e, VInfo (FA.srcName e) srcFile (FU.getSpan e))
  | F.DeclArray _ _ e _ _ _   <- universeBi x :: [F.Declarator DA]]

distil :: DDTStatement -> VInfo -> SE.Either IndexDupError Essence
distil (DDTSt { ddtStTypeName = tyname, ddtStLabels = labels }) vinfo
  -- if no duplicated nums
  | length (List.nub nums) == length nums = SE.Right $ Essence tyname (IM.fromList [ (n, lname) | (n, lname) <- labels' ]) (S.singleton vinfo)
  -- if there's a problem
  | otherwise                             = SE.Left $ IndexDupError tyname vinfo (nums List.\\ List.nub nums)
  where
    labels' = map (first read) labels
    nums = map fst labels'

distilArrayInfo :: F.Name -> S.Set VInfo -> S.Set Int -> Essence
distilArrayInfo v vinfoSet = perDim
  where
    perDim dimSet = Essence tyname labMap vinfoSet
      where
        tyname = v ++ "_type"
        labMap = IM.fromList [ (n, lname) | n <- S.toList dimSet, let lname = "label" ++ show n ]

--------------------------------------------------
-- Compilation helpers

-- | Generate a new ModFile containing derived datatype information.
genDDTModFile :: Data a => F.ProgramFile (FA.Analysis a) -> DerivedDataTypeReport -> ModFile
genDDTModFile pf ddtr = alterModFileData f ddtCompiledDataLabel $ genModFile pf
  where
    f _ = Just . LB.toStrict $ encode ddtr

mfDerivedDataTypeReport :: ModFile -> DerivedDataTypeReport
mfDerivedDataTypeReport mf = case lookupModFileData ddtCompiledDataLabel mf of
  Nothing -> mempty
  Just bs -> case decodeOrFail (LB.fromStrict bs) of
    Left _ -> mempty
    Right (_, _, ddtr) -> ddtr

combinedDerivedDataTypeReport :: ModFiles -> DerivedDataTypeReport
combinedDerivedDataTypeReport = foldMap mfDerivedDataTypeReport

ddtCompiledDataLabel :: String
ddtCompiledDataLabel = "derived-datatypes-compiled-data"
