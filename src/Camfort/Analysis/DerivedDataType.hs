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
type AMap = M.Map F.Name (M.Map Int (S.Set (Maybe Integer)))

-- | map of info about vars
type VMap = M.Map F.Name (S.Set VInfo)

data VInfo = VInfo { vSrcName :: F.Name, vFileName :: String, vSrcSpan :: FU.SrcSpan }
  deriving (Generic, Eq)

instance Ord VInfo where
  VInfo s1 f1 ss1 `compare` VInfo s2 f2 ss2 = (f1, ss1, s1) `compare` (f2, ss2, s2)

instance Binary VInfo

data DerivedDataTypeReport = DerivedDataTypeReport AMap VMap
  deriving (Generic)

instance Binary DerivedDataTypeReport

instance SG.Semigroup DerivedDataTypeReport where
  DerivedDataTypeReport m1 v1 <> DerivedDataTypeReport m2 v2 =
    DerivedDataTypeReport (M.unionWith (M.unionWith S.union) m1 m2) (M.unionWith S.union v1 v2)

instance Monoid DerivedDataTypeReport where
  mempty = DerivedDataTypeReport M.empty M.empty
  mappend = (SG.<>)

instance ExitCodeOfReport DerivedDataTypeReport where
  exitCodeOf (DerivedDataTypeReport m _) | M.null m  = 0
                                         | otherwise = 1

instance Describe DerivedDataTypeReport where
  describeBuilder (DerivedDataTypeReport amap vmap)
    | M.null amap = "no cases detected"
    | otherwise = Builder.fromText . unlines . concat $
      [ ("\n"<>pack fileName<>":\n"): [ describe srcSpan <> pack (genCommentText amap (v, srcName))
                                      | (VInfo srcName _ srcSpan, v, pstrs) <- sort srcsPstrs ]
      | (fileName, srcsPstrs) <- M.toList byFile]

    where
      perParam Nothing = "*"; perParam (Just k) = describe k
      perName (n, consts) = "{" <> intercalate ";" (map perParam (S.toList consts)) <> "}"
      namePstrs = [ (name, pstrs) | (name, pmap) <- M.toList $ filterCondensedCategoryOne amap
                                  , let pstrs = map perName $ M.toList pmap ]
      srcsPstrs = [ (vFileName vinfo, [(vinfo, name, pstrs)])
                  | (name, pstrs) <- namePstrs
                  , vinfo <- join (maybeToList (S.toList <$> M.lookup name vmap)) ]
      byFile = M.fromListWith (++) srcsPstrs

--------------------------------------------------
-- ddt-check reporting infrastructure

-- 'essence' of a specification
-- type name, map of int => label, (source name, span)
data Essence = Essence String (IM.IntMap String) (F.Name, FU.SrcSpan)
  deriving (Show)

instance Eq Essence where
  Essence ty1 l1 _ == Essence ty2 l2 _ = (ty1, l1) == (ty2, l2)

instance Ord Essence where
  Essence ty1 l1 _ `compare` Essence ty2 l2 _ = (ty1, l1) `compare` (ty2, l2)

type SrcNameSpan = (F.Name, FU.SrcSpan)

-- given type name, (source name, span), dupped labels
data LabelDupError = LabelDupError String SrcNameSpan [Int]
  deriving (Show, Eq, Ord)

type ConflictErrors = M.Map F.Name (S.Set Essence) -- variable => essence set

type BadLabelErrors = M.Map F.Name (S.Set (String, SrcNameSpan))  -- variable => (label, (source name, span)) set

data DerivedDataTypeCheckReport
  = DerivedDataTypeCheckReport (S.Set LabelDupError) ConflictErrors BadLabelErrors
    deriving (Eq)

instance SG.Semigroup DerivedDataTypeCheckReport where
  DerivedDataTypeCheckReport a1 b1 c1 <> DerivedDataTypeCheckReport a2 b2 c2 =
    DerivedDataTypeCheckReport (S.union a1 a2) (M.unionWith S.union b1 b2) (M.unionWith S.union c1 c2)

instance Monoid DerivedDataTypeCheckReport where
  mempty = DerivedDataTypeCheckReport S.empty M.empty M.empty
  mappend = (SG.<>)

instance ExitCodeOfReport DerivedDataTypeCheckReport where
  exitCodeOf r | r == mempty = 0
               | otherwise   = 1

instance Describe DerivedDataTypeCheckReport where
  describeBuilder (DerivedDataTypeCheckReport lde ce ble) = Builder.fromText . unlines . concat $
    [ if S.null lde then [] else ["Duplicated indices:"]
    , [ describe ss <> " " <> pack ty <> " has duplicated indice(s) [" <> intercalate ", " (map describe ints) <>
        "] for variable: " <> pack srcName | LabelDupError ty (srcName, ss) ints <- S.toList lde ]
    , if M.null ce then [] else ["Conflicts:"]
    , [ describe ss0 <> " " <> pack ty0 <> "(" <> describeLabels l0 <> ") :: " <> pack src0 <>
        "\nconflicts with\n" <> unlines [ describe ss <> " " <> pack ty <> "(" <> describeLabels labs <> ") :: " <> pack src
                                        | Essence ty labs (src, ss) <- essences ]
      | (_, essenceSet) <- M.toList ce
      , not (S.null essenceSet)
      , let Essence ty0 l0 (src0, ss0):essences = S.toList essenceSet ]
    , if M.null ble then [] else ["Bad Labels:"]
    , [ describe ss <> " label '" <> pack lab <> "', associated with variable " <> pack src
      | (_, badSet) <- M.toList ble
      , (lab, (src, ss)) <- S.toList badSet ]
    ]

    where
      describeLabels labs = intercalate ", " [describe i <> "=>" <> pack l | (i,l) <- IM.toList labs]

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
  return . fst $ genProgramFileReport mfs pf

-- | Check annotations relating to derived datatypes in given program file
check :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeCheckReport
check pf = do
  mfs <- analysisModFiles
  let (report, pf') = genProgramFileReport mfs (fmap ddtAnnotation0 pf)
  let (linkedPF, _) =
        runWriter $ annotateComments ddtParser
        (\srcSpan err -> tell $ "Error " ++ show srcSpan ++ ": " ++ show err) pf'
  let specs = [ (spec, b) | DDTAnnotation { ddtSpec = Just spec, ddtBlock = Just b } <- universeBi linkedPF ]

  -- boil down the parsed specs
  let e_essences = [ essence | (spec@DDTSt { ddtStVarDims = vardims } , b) <- specs
                             , (var, dim) <- vardims
                             , (declVarName, declSrcName, ss) <- declaredVars b
                             , declSrcName == var
                             , let essence = (declVarName, distil spec (declSrcName, ss)) ]
  let (errors, essences) = List.partition (SE.isLeft . snd) e_essences
  let lde = S.fromList $ map (SE.fromLeft . snd) errors
  let essences' = M.fromListWith S.union $ map (second (S.singleton . SE.fromRight)) essences
  -- conflicting specs
  let conflicts = M.filter ((>1) . S.size) essences'
  -- dupped labels
  let f (Essence _ labMap ss) = map (,ss) . M.keys . M.filter (>1) . M.fromListWith (+) $ map (,1) (IM.elems labMap)
  let badLabels = M.filter (not . null) $ M.map (S.fromList . concatMap f . S.toList) essences'
  -- FIXME: gather annotations and check consistency from modfiles
  return $ DerivedDataTypeCheckReport lde conflicts badLabels

-- | Generate and insert comments about derived datatypes
synth :: Char -> [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
synth marker pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf'@(F.ProgramFile mi _)) = genProgramFileReport mfs pf
    let pf'' = descendBi (synthBlocks (mi, marker) report) pf'
    -- FIXME: generate datatype names, labels & insert comments
    return $ (report, Right (fmap FA.prevAnnotation pf''))

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
  , indexInfo :: [(Int, Maybe Integer)]
  }
  deriving (Show, Eq)

genProgramFileReport :: Data a => ModFiles -> F.ProgramFile a -> (DerivedDataTypeReport, F.ProgramFile (FA.Analysis a))
genProgramFileReport mfs (pf@(F.ProgramFile F.MetaInfo{ F.miFilename = srcFile } _)) = (report, pf')
  where
    (analysisOutput, pf') = analysis mfs pf
    amap = condenseCategoryOne analysisOutput
    vars = S.fromList $ M.keys amap
    vls1 = [ (v, S.singleton $ VInfo (FA.srcName e) srcFile ss)
           | F.DeclArray _ ss e _ _ _ <- universeBi pf' :: [F.Declarator (FA.Analysis A)]
           , let v = FA.varName e
           , v `S.member` vars ]
    vls2 = [ (v, S.singleton $ VInfo (FA.srcName e) srcFile ss)
           | F.DeclVariable _ ss e _ _ <- universeBi pf' :: [F.Declarator (FA.Analysis A)]
           , let v = FA.varName e
           , v `S.member` vars ]
    vmap = M.fromListWith S.union $ vls1 ++ vls2
    report = DerivedDataTypeReport amap vmap <> combinedDerivedDataTypeReport mfs

-- analyse bulk data info
analysis :: Data a => ModFiles -> F.ProgramFile a -> ([(String, BulkDataArrayInfo)], F.ProgramFile (FA.Analysis a))
analysis mfs pf = (accessInfo, pf'')
  where
     filename       = F.pfGetFilename pf
     (pf', _, tenv) = withCombinedEnvironment mfs pf
     pf''           = FAD.analyseConstExps $ FAB.analyseBBlocks pf'

     perArray :: [F.Index (FA.Analysis A)] -> [(Int, Maybe Integer)]
     perArray is = [ (n, do F.IxSingle _ _ _ e <- Just ix; FAD.ConstInt i <- FA.constExp (F.getAnnotation e); return i)
                   | (n, ix) <- zip [1..] is ]
     accessInfo =
       [ (filename, Array (FA.varName a) ss $ perArray (F.aStrip is))
       | F.ExpSubscript _ ss a@(F.ExpValue _ _ (F.ValVariable _)) is <- universeBi pf'' ]


-- convert analysis info into a map of information about the constants used to index arrays
condenseCategoryOne :: [(Filename, BulkDataArrayInfo)] -> AMap
condenseCategoryOne c1 = amap
  where
    amap = M.fromListWith (M.unionWith S.union) [ (arrayName bdai, doIndices (indexInfo bdai)) | bdai <- map snd c1 ]
    doIndices is = M.fromList $ map (fmap S.singleton) is

-- filter only the interesting ones: at least one of the parameters
-- was populated only by a range of constants where each constant is
-- no more than 3 away from the adjacent ones.
filterCondensedCategoryOne :: AMap -> AMap
filterCondensedCategoryOne = M.filter (not . M.null . M.filter valid) -- accept if at least one valid parameter
  where
    diffs = uncurry (zipWith subtract) . (id &&& drop 1) . sort -- compute differences between consecutive numbers
    -- valid set if...
    valid = and . sequence [ not . null                       -- (1) non-empty list
                           , all isJust                       -- (2) of constants only, no wildcards
                           , all (< 3) . diffs . map fromJust -- (3) no more than 3 away from adjacent constants
                           ] . S.toList

--------------------------------------------------
-- Synthesis helpers

synthBlocks :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> [F.Block (FA.Analysis A)] -> [F.Block (FA.Analysis A)]
synthBlocks marker report = concatMap (synthBlock marker report)

synthBlock :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> F.Block (FA.Analysis A) -> [F.Block (FA.Analysis A)]
synthBlock (mi, marker) (DerivedDataTypeReport amap _) b = case b of
  F.BlStatement a ss _ F.StDeclaration{}
    | vars <- ofInterest b -> genComment vars ++ [b]
    where
      ofInterest b = filter (flip M.member amap . fst) $
        [ (FA.varName e, FA.srcName e) | F.DeclVariable _ _ e _ _ <- universeBi b :: [F.Declarator (FA.Analysis A)] ] ++
        [ (FA.varName e, FA.srcName e) | F.DeclArray _ _ e _ _ _ <- universeBi b :: [F.Declarator (FA.Analysis A)] ]

      genComment = map $ \ var ->
        F.BlComment newA newSS . F.Comment . buildCommentText mi space $ marker:genCommentText amap var

      newA = a { FA.prevAnnotation = (FA.prevAnnotation a) { refactored = Just lp } }
      newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
      FU.SrcSpan lp _ = ss
      space = FU.posColumn lp - 1
  _ -> [b]

-- Generate the text that goes into the specification.
genCommentText amap (v, s)
  | Just pmap <- M.lookup v amap
  , ty <- v ++ "_type"
  , (dim, set):_ <- M.toList $ M.filter (all isJust . S.toList) pmap
  , nums <- map fromJust $ S.toList set
  , labs <- List.intercalate ", " [ str ++ "=>" ++ "label" ++ str | num <- nums, let str = show num ] =
      " ddt " ++ ty ++ "(" ++ labs ++ ") :: " ++ s ++ "(dim=" ++ show dim ++ ")"

--------------------------------------------------
-- Check helpers

declaredVars :: F.Block DA -> [(F.Name, F.Name, FU.SrcSpan)]
declaredVars x =
  [ (FA.varName e, FA.srcName e, FU.getSpan e) | F.DeclVariable _ _ e _ _  <- universeBi x :: [F.Declarator DA]] ++
  [ (FA.varName e, FA.srcName e, FU.getSpan e) | F.DeclArray _ _ e _ _ _   <- universeBi x :: [F.Declarator DA]]

distil :: DDTStatement -> SrcNameSpan -> SE.Either LabelDupError Essence
distil (DDTSt { ddtStTypeName = tyname, ddtStLabels = labels }) ss
  -- if no duplicated nums
  | length (List.nub nums) == length nums = SE.Right $ Essence tyname (IM.fromList [ (n, lname) | (n, lname) <- labels' ]) ss
  -- if there's a problem
  | otherwise                             = SE.Left $ LabelDupError tyname ss (nums List.\\ List.nub nums)
  where
    labels' = map (first read) labels
    nums = map fst labels'

--------------------------------------------------
-- Compilation helpers

-- | Generate a new ModFile containing derived datatype information.
genDDTModFile :: F.ProgramFile (FA.Analysis A) -> DerivedDataTypeReport -> ModFile
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
