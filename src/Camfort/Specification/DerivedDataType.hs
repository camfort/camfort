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
{-# LANGUAGE RecordWildCards #-}

module Camfort.Specification.DerivedDataType
       ( infer, refactor, check, synth, compile
       , DerivedDataTypeReport(..), successful )
where

import           Camfort.Analysis
import           Camfort.Analysis.Annotations (onPrev, buildCommentText, A, Annotation(..))
import           Camfort.Analysis.CommentAnnotator (annotateComments, ASTEmbeddable(..), Linkable(..))
import           Camfort.Analysis.ModFile
import           Camfort.Helpers.Syntax (afterAligned, toCol0, deleteLine)
import           Camfort.Specification.DerivedDataType.Parser (ddtParser, DDTStatement(..))
import           Control.Applicative
import           Control.Arrow (first, second, (&&&))
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.RWS.Strict
import           Control.Monad.Writer.Strict
import           Data.Binary (Binary, decodeOrFail, encode)
import           Data.Data
import           Data.Function (on)
import           Data.Generics.Uniplate.Operations hiding (rewrite)
import qualified Data.IntMap.Strict as IM
import           Data.List (sort, foldl', groupBy)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, fromMaybe, fromJust, isJust)
import           Data.Monoid ((<>))
import qualified Data.Semigroup as SG
import qualified Data.Set as S
import qualified Data.Strict.Either as SE
import           Data.Text (Text, unlines, intercalate, pack)
import qualified Data.Text.Lazy.Builder as Builder
import           GHC.Generics (Generic)
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Analysis.Types as FAT
import           Language.Fortran.Util.ModFile
import qualified Language.Fortran.Util.Position as FU
import           Prelude hiding (unlines, minBound, maxBound)
import           Language.Fortran.Repr (fromConstInt)

ddtShort :: String
ddtShort = "ddt"

--------------------------------------------------
-- Linking DDT specifications with associated AST-blocks.

-- | Annotation used for derived-datatype info.
data DDTAnnotation = DDTAnnotation {
    prevAnnotation :: A,
    ddtSpec        :: Maybe DDTStatement,      -- ^ parsed spec on comments, if any
    ddtBlock       :: Maybe (F.Block DA)       -- ^ linked variable declaration, if any
  } deriving (Data, Typeable, Show)
-- | Initialize DDTAnnotation
ddtAnnotation0 :: A -> DDTAnnotation
ddtAnnotation0 a = DDTAnnotation a Nothing Nothing

-- | Annotation used by most analysis in this file.
type DA = FA.Analysis DDTAnnotation

-- | Modify top-level annotation.
onOrigAnnotation :: (A -> A) -> DA -> DA
onOrigAnnotation f = onPrev $ \ a -> a { prevAnnotation = f (prevAnnotation a) }

-- | Strip annotations used by this file.
stripAnnotations :: Functor f => f DA -> f A
stripAnnotations = fmap (prevAnnotation . FA.prevAnnotation)

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable DA DDTStatement where
  annotateWithAST ann ast = onPrev (\ ann' -> ann' { ddtSpec = Just ast }) ann

-- Link annotation comments to declaration statements
instance Linkable DA where
  link ann b@(F.BlStatement _ _ _ F.StDeclaration{}) = onPrev (\ ann' -> ann' { ddtBlock = Just b }) ann
  link ann _ = ann
  linkPU ann _ = ann -- annotation on PU not needed/supported

--------------------------------------------------
-- Reporting data structures.

-- | Map of information about constants used to index arrays. Nothing
-- implies a non-constant is used to index the array at the given
-- dimension.
--
-- Variable name (unique name) => (dim => maybe constants)
type AMap = M.Map F.Name (IM.IntMap (S.Set (Maybe Int)))

-- | Map of info about vars (unique name).
type VMap = M.Map F.Name (S.Set VInfo)

-- | Map of specification essences connected to var-unique-name(dim)s.
type SMap = M.Map (F.Name, Int) (S.Set Essence)

-- | Info about a variable: source name, source filename, source span.
data VInfo = VInfo { vSrcName :: F.Name, vFileName :: String, vSrcSpan :: FU.SrcSpan }
  deriving (Generic, Show, Eq)
instance NFData VInfo
instance Binary VInfo

-- special ord instance compares by filename, source span and then name.
instance Ord VInfo where
  VInfo s1 f1 ss1 `compare` VInfo s2 f2 ss2 = (f1, ss1, s1) `compare` (f2, ss2, s2)

-- | The 'essence' of a specification, in comparable form.
data Essence = Essence { essTypeName :: String            -- ^ specified type name
                       , essLabelMap :: IM.IntMap String  -- ^ specified index => label map
                       , essVInfoSet :: S.Set VInfo       -- ^ location of specified vars
                       , essStarred  :: Bool              -- ^ is this essence 'starred'?
                       }
  deriving (Show, Generic)
instance NFData Essence
instance Binary Essence

-- special eq instance: only compare type-name and labels
instance Eq Essence where
  Essence ty1 l1 _ _ == Essence ty2 l2 _ _ = (ty1, l1) == (ty2, l2)

-- special ord instance: only compare type-name and labels
instance Ord Essence where
  Essence ty1 l1 _ _ `compare` Essence ty2 l2 _ _ = (ty1, l1) `compare` (ty2, l2)

-- given type name, (source name, span), dupped labels
data IndexError = IndexDupError String VInfo [Int]
                | IndexOOBError String VInfo [Int]
  deriving (Show, Eq, Ord, Generic)
instance NFData IndexError
instance Binary IndexError

-- | Variable(dim)s and any conflicting spec essences.
type ConflictErrors = M.Map (F.Name, Int) (S.Set Essence)

-- | Variable(dim)s where the specification has a bad (e.g. duplicated) label name
type BadLabelErrors = M.Map (F.Name, Int) (S.Set (String, VInfo))

-- | Variable(dim)s where the specification has a bad (e.g. duplicated) index
type BadDimErrors = M.Map (F.Name, Int) (S.Set (Int, VInfo))

-- | Collection of information comprising a 'derived datatype' report
-- for CamFort output purposes.
data DerivedDataTypeReport
  = DerivedDataTypeReport { ddtrAMap  :: AMap
                          , ddtrVMap  :: VMap
                          , ddtrSMap  :: SMap
                          , ddtrSpecs :: [DDTStatement]
                          , ddtrIDE   :: (S.Set IndexError)
                          , ddtrCE    :: ConflictErrors
                          , ddtrBLE   :: BadLabelErrors
                          , ddtrBDE   :: BadDimErrors
                          , ddtrCheck :: Bool }
  deriving Generic
instance NFData DerivedDataTypeReport
instance Binary DerivedDataTypeReport

-- | These reports can be combined, e.g. from multiple files, and the
-- key part is that the problem reports are also combined, and new
-- problems can be identified from the combination of the SMaps.
instance SG.Semigroup DerivedDataTypeReport where
  DerivedDataTypeReport m1 v1 s1 sp1 ide1 ce1 ble1 bde1 ch1 <> DerivedDataTypeReport m2 v2 s2 sp2 ide2 ce2 ble2 bde2 ch2 =
    DerivedDataTypeReport (M.unionWith (IM.unionWith S.union) m1 m2) (M.unionWith S.union v1 v2) newSMap
                          (sp1 ++ sp2)
                          (S.union ide1 ide2) (M.unionsWith S.union [ce1, ce2, newCE])
                          (M.unionWith S.union ble1 ble2)
                          (M.unionWith S.union bde1 bde2)
                          (ch1 && ch2)
    where
      -- new conflicts found, if any
      newCE   = M.map SE.fromLeft $ M.filter SE.isLeft e_smap
      -- combined SMaps
      newSMap = M.map (S.singleton . SE.fromRight) $ M.filter SE.isRight e_smap

      -- | Combine the SMaps while looking for new conflicts.
      e_smap = M.fromListWith combine [ (v, SE.Right e) | (v, eSet) <- M.toList s1 ++ M.toList s2, e <- S.toList eSet ]

      combine (SE.Left e1) _              = SE.Left e1
      combine _ (SE.Left e2)              = SE.Left e2
      combine (SE.Right e1) (SE.Right e2) = combineEssences e1 e2

-- | Combine compatible essences (SE.Right) or else indicate conflict (SE.Left)
combineEssences :: Essence -> Essence -> SE.Either (S.Set Essence) Essence
combineEssences e1 e2
  | Essence ty1 labMap1 vinfoSet1 s1 <- e1
  , Essence ty2 labMap2 vinfoSet2 s2 <- e2 =
    fromMaybe (SE.Left $ S.fromList [e1, e2]) $ do
      -- In case of conflict, starred essence info overwrites unstarred essence info.
      --
      -- Compatible type name:
      ty <- case () of _ | ty1 == ty2   -> pure ty1
                         | s1 && not s2 -> pure ty1
                         | not s1 && s2 -> pure ty2
                         | otherwise    -> mzero
      -- Compatible label names:
      let labelStarCombine l1 l2 | l1 == l2     = l1
                                 | s1 && not s2 = l1
                                 | not s1 && s2 = l2
                                 | otherwise    = mzero
      -- Combined label map:
      labMap <- sequenceIntMap $ IM.unionWith labelStarCombine (IM.map pure labMap1) (IM.map pure labMap2)
      pure . SE.Right $ Essence ty labMap (S.union vinfoSet1 vinfoSet2) (s1 || s2)

instance Monoid DerivedDataTypeReport where
  mempty = DerivedDataTypeReport M.empty M.empty M.empty [] S.empty M.empty M.empty M.empty False
  mappend = (SG.<>)

-- | True iff no errors are reported.
successful :: DerivedDataTypeReport -> Bool
successful r = and [S.null (ddtrIDE r), M.null (ddtrCE r), M.null (ddtrBLE r), M.null (ddtrBDE r)]

instance ExitCodeOfReport DerivedDataTypeReport where exitCodeOf r | successful r = 0 | otherwise = 1

instance Describe DerivedDataTypeReport where
  describeBuilder r@DerivedDataTypeReport{..}
    | not (successful r)    = Builder.fromText errorReport
    | M.null ddtrSMap       = "no cases detected"
    | ddtrCheck && num == 1 = "1 specification checked."
    | ddtrCheck             = Builder.fromText $ describe num <> " specifications checked."
    | otherwise             = Builder.fromText specReport
    where
      num = length ddtrSpecs
      specReport = linesByFile specLines
      specLines = [ (vinfo, [pack $ genCommentText r (var, vSrcName)])
                  | ((var, _), essenceSet)   <- M.toList ddtrSMap
                  , Essence{..}              <- S.toList essenceSet
                  , vinfo@VInfo{..}          <- S.toList essVInfoSet ]

      ideLines = [ (vinfo, [pack ty <> " has duplicated indice(s) [" <>
                            intercalate ", " (map describe ints) <> "] for variable: " <> pack (vSrcName vinfo)])
                 | IndexDupError ty vinfo ints <- S.toList ddtrIDE ]
      oobLines = [ (vinfo, [pack ty <> " has out-of-bounds indice(s) [" <>
                            intercalate ", " (map describe ints) <> "] for variable: " <> pack (vSrcName vinfo)])
                 | IndexOOBError ty vinfo ints <- S.toList ddtrIDE ]
      ceLines  = [ (vinfo, [pack ty0 <> "(" <> describeLabels l0 <> ") :: " <> pack (vSrcName vinfo) <> "(dim=" <>
                            describe dim <> ")\nconflicts with\n" <>
                            unlines [ pack fn <> ": " <> describe ss <> " " <> pack ty <> "(" <> describeLabels labs <> ")"
                                    | Essence ty labs vinfoSet _ <- essences
                                    , VInfo _ fn ss <- S.toList vinfoSet ]])
                 | ((_, dim), essenceSet) <- M.toList ddtrCE
                 , not (S.null essenceSet)
                 , let Essence ty0 l0 vinfoSet0 _:essences = S.toList essenceSet
                 , vinfo <- take 1 (S.toList vinfoSet0) ]
      bleLines = [ (vinfo, ["duplicated label '" <> pack lab <> "', associated with variable " <>
                            pack (vSrcName vinfo) <> "(dim=" <> describe dim <> ")"])
                 | ((_, dim), badSet) <- M.toList ddtrBLE
                 , (lab, vinfo) <- S.toList badSet ]
      bdeLines = [ (vinfo, [pack (vSrcName vinfo) <> ": bad dim " <> describe dim <>
                            if maxDim == 0 then " less than 1"
                            else " not in range 1.." <> describe maxDim])
                 | ((_, dim), badSet) <- M.toList ddtrBDE
                 , (maxDim, vinfo) <- S.toList badSet ]

      errorReport = linesByFile $ ideLines ++ oobLines ++ ceLines ++ bleLines ++ bdeLines
      describeLabels labs = intercalate ", " [describe i <> "=>" <> pack l | (i,l) <- IM.toList labs]

-- | Return combined text where entries are grouped and sorted by filename, source span
linesByFile :: [(VInfo, [Text])] -> Text
linesByFile vinfoTexts = unlines $ concat [ ("\n"<>pack fileName<>":\n") : [ describe ss <> " " <> text
                                                                           | (ss, text) <- sort sstexts ]
                                          | (fileName, sstexts) <- M.toList mapByFile ]
  where
    mapByFile = M.fromListWith (++) [ (vFileName vinfo, map (vSrcSpan vinfo,) texts) | (vinfo, texts) <- vinfoTexts ]

--------------------------------------------------
-- External Functionality Interface

-- | Generate report about derived datatypes in given program file
infer :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
infer pf = do
  mfs <- analysisModFiles
  return . fst $ genProgramFileReport mfs pf

-- | Check annotations relating to derived datatypes in given program file
check :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
check pf = do
  mfs <- analysisModFiles
  return (fst $ genProgramFileReport mfs pf) { ddtrCheck = True }
  -- FIXME: check that a user-supplied spec doesn't conflict with something disqualifying in the code

-- | Generate and insert comments about derived datatypes
synth :: Char -> [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
synth marker pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf'@(F.ProgramFile mi _)) = genProgramFileReport mfs pf
    let synthedPF = descendBi (synthBlocks (mi, marker) report) pf'
    let strippedPF | successful report = Right (stripAnnotations synthedPF)
                   | otherwise         = Left "error"
    return $ (report, strippedPF)

-- | Refactor derived datatypes based on marked comments
refactor :: [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
refactor pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf') = genProgramFileReport mfs pf
    let smap = M.filter (not . S.null) . M.map (S.filter essStarred) $ ddtrSMap report
    let amap = M.filter (not . IM.null) . M.mapWithKey cullDims $ ddtrAMap report
          where cullDims var = IM.filterWithKey (\ dim _ -> M.member (var, dim) smap)
    let report' = report { ddtrSMap = smap, ddtrAMap = amap }
    if M.null smap
      then return (report', Left "nothing to do")
      else return (report', Right . stripAnnotations $ refactorPF report' pf')

-- | Compile a program to a 'ModFile' containing derived datatype information.
compile :: () -> ModFiles -> F.ProgramFile A -> IO ModFile
compile _ mfs pf = do
  let (report, pf') = genProgramFileReport mfs pf
  return $ genDDTModFile pf' report

--------------------------------------------------
-- Analysis helpers

-- | Perform monadic action on each, which returns a pair. The fst
-- elements of the pairs is combined with (<>) and the second elements
-- of the pairs are returned in a list.
forEachProgramFile :: (Monad m, Monoid r) => [a] -> (a -> m (r, b)) -> m (r, [b])
forEachProgramFile pfs f = do
  results <- mapM f pfs
  return (foldl' (<>) mempty (map fst results), map snd results)

-- | Performs analysis about a single program file and returns a
-- report containing the essences and problems detected, as well as an
-- annotated program file for further usage.
genProgramFileReport :: ModFiles -> F.ProgramFile A -> (DerivedDataTypeReport, F.ProgramFile DA)
genProgramFileReport mfs (pf@(F.ProgramFile F.MetaInfo{ F.miFilename = srcFile } _)) = (report, pf')
  where
    (amap, pf', tenv) = analysis mfs pf
    vars = S.fromList $ M.keys amap
    vls  = [ (v, S.singleton $ VInfo (FA.srcName e) srcFile ss)
           | F.Declarator _ ss e _ _ _ <- universeBi pf' :: [F.Declarator DA]
           , let v = FA.varName e
           , v `S.member` vars ]
    vmap = M.fromListWith S.union vls

    specs = [ (spec, b) | DDTAnnotation { ddtSpec = Just spec, ddtBlock = Just b } <- universeBi pf' ]

    -- Boil down the specs parsed from the comments.
    e_essences = [ essMapping | (spec@DDTSt{..}, b)  <- specs
                              , (var, dim)           <- ddtStVarDims
                              , (declVarName, vinfo) <- declaredVars srcFile b
                              , vSrcName vinfo == var
                              , let essMapping = ((declVarName, dim), distil spec vinfo) ]
    (l_errors, r_essences) = List.partition (SE.isLeft . snd) e_essences

    -- Dupped indices:
    dupIndices = S.fromList $ map (SE.fromLeft . snd) l_errors

    essences :: M.Map (F.Name, Int) (S.Set Essence)
    essences = M.fromListWith S.union $ map (second (S.singleton . SE.fromRight)) r_essences

    -- Conflicting specs:
    conflicts = M.filter ((>1) . S.size) essences
    findDupLabels Essence{..} = [ (lab, vinfo) | lab <- labs, vinfo <- vinfos ]
      where
        vinfos = S.toList essVInfoSet
        -- Labels that appear more than once in the label map:
        labs = M.keys . M.filter (>1) . M.fromListWith (+) . map (,1::Int) $ IM.elems essLabelMap

    -- Dupped labels:
    badLabels = M.filter (not . null) $ M.map (setConcatMap findDupLabels) essences

    -- Badly specified 'dim' attributes (e.g. out of bounds):
    badDims = [ ((declVarName, dim), S.singleton (maxDim, vinfo))
              | (DDTSt{..}, b)       <- specs
              , (srcName, dim)       <- ddtStVarDims
              , (declVarName, vinfo) <- declaredVars srcFile b
              , vSrcName vinfo == srcName
              , Just maxDim <- [ do guard (dim < 1)
                                    return 0  -- Stand-in number for 'dim violates the lower bound' case.
                                 <|>
                                 do FA.IDType { FA.idCType = Just (FA.CTArray dims) } <- M.lookup declVarName tenv
                                    let maxDim = length dims
                                    guard $ not (null dims || dim <= maxDim)
                                    -- There is a known upper-bound and dim violates it.
                                    return maxDim ] ]

    -- Index out-of-bounds of statically-known array dimensions:
    oobIndices = [ IndexOOBError tyname vinfo indices
                 | (DDTSt{..}, b)       <- specs
                 , (srcName, dim)       <- ddtStVarDims
                 , (declVarName, vinfo) <- declaredVars srcFile b
                 , let tyname = ""
                 , vSrcName vinfo == srcName
                 , Just indices <- [ do FA.IDType { FA.idCType = Just (FA.CTArray dims) } <- M.lookup declVarName tenv
                                        let maxDim = length dims
                                        guard $ dim >= 1 && dim <= maxDim
                                        let (mminBound, mmaxBound) = dims !! (dim - 1)
                                        let minBound = 1 `fromMaybe` mminBound
                                        maxBound <- mmaxBound
                                        let indices = [ read i | (i, _) <- ddtStLabels ]
                                        let oob = filter (uncurry (||) . ((< minBound) &&& (> maxBound))) indices
                                        guard . not . null $ oob
                                        -- oob is the list of indices out-of-bounds
                                        return oob ] ]

    -- Distil specs from the inferred AMap:
    smapFromAMap = M.fromList [ ((v, dim), essenceSet) | (v, aminfoMap) <- M.toList amap
                                                       , (dim, aminfo)  <- IM.toList aminfoMap
                                                       , all isJust (S.toList aminfo)
                                                       , Just vinfoSet  <- [M.lookup v vmap]
                                                       , let e = distilArrayInfo v vinfoSet (S.map fromJust aminfo)
                                                       , let essenceSet = S.singleton e ]
    -- Specs in the file override inferred specs:
    smap = M.unionWith (curry fst) essences smapFromAMap

    ide  = dupIndices `S.union` S.fromList oobIndices
    ce   = conflicts
    ble  = badLabels
    bde  = M.fromList badDims
    -- Combine this report with those reports read from modfiles:
    report = DerivedDataTypeReport amap vmap smap (map fst specs) ide ce ble bde False <> combinedDerivedDataTypeReport mfs

-- | Analyse and return an AMap, a spec-linked ProgramFile and a type
-- environment (convenience). The AMap contains derived information
-- about array accesses that appear to be in a 'category 1' pattern,
-- namely whether the array is accessed at the particular dimension
-- only by constants.
analysis :: ModFiles -> F.ProgramFile A -> (AMap, F.ProgramFile DA, FAT.TypeEnv)
analysis mfs pf = (amap', linkedPF, tenv)
  -- FIXME: check for violations of the 'only one array deref in a path' rule
  where
     (pf', _, tenv) = withCombinedEnvironment mfs (fmap ddtAnnotation0 pf)
     pf''           = FAD.analyseConstExps $ FAB.analyseBBlocks pf'

     -- Link specifications (in comments) to associated AST-blocks:
     (linkedPF, _) = runWriter $ annotateComments ddtParser
       (\ srcSpan err -> tell $ "Error " ++ show srcSpan ++ ": " ++ show err) pf''

     -- Attempt to gather any constant-expression information from indices.
     perArray :: [F.Index DA] -> [(Int, Maybe Int)]
     perArray is = [ (n, do F.IxSingle _ _ Nothing e <- Just ix
                            i                        <- FA.constExp (F.getAnnotation e) >>= fromConstInt
                            return (fromIntegral i))
                   | (n, ix) <- zip [1..] is ]

     -- Gathered array information for each observed array access:
     accessInfo :: [(F.Name, IM.IntMap (S.Set (Maybe Int)))]
     accessInfo = [ (FA.varName a, makeIntMapSet observation)
                  | F.ExpSubscript _ _ a@(F.ExpValue _ _ (F.ValVariable _)) is <- universeBi pf''
                  , let observation = perArray $ F.aStrip is ]

     -- Convert access info into a map of information about the
     -- constants used to index arrays.
     amap :: M.Map F.Name (IM.IntMap (S.Set (Maybe Int)))
     amap = M.fromListWith (IM.unionWith S.union) accessInfo

     -- Filter only the interesting ones: at least one of the
     -- parameters was populated only by a range of constants where
     -- each constant is no more than 3 away from the adjacent ones.
     amap' = M.filter (not . IM.null . IM.filter valid) amap -- accept if at least one valid parameter
       where
         diffs = uncurry (zipWith subtract) . (id &&& drop 1) . sort -- compute differences between consecutive numbers
         -- valid set if...
         valid = and . sequence [ not . null                       -- (1) non-empty list
                                , all isJust                       -- (2) of constants only, no wildcards
                                , all (< 3) . diffs . map fromJust -- (3) no more than 3 away from adjacent constants
                                ] . S.toList
--------------------------------------------------
-- Refactoring helpers

type RefactorM = RWS DerivedDataTypeReport [Essence] Bool

refactorPF :: DerivedDataTypeReport -> F.ProgramFile DA -> F.ProgramFile DA
refactorPF r pf = pf'
  where
    -- FIXME: possibly use the 'essences' writer slot to instead
    -- gather derived-type declarations and put them somewhere
    -- central.
    (pf', _, _) = runRWS (descendBiM refactorBlocks pf) r False

refactorBlocks :: [F.Block DA] -> RefactorM [F.Block DA]
refactorBlocks = fmap concat . mapM refactorBlock

refactorBlock :: F.Block DA -> RefactorM [F.Block DA]
refactorBlock b = ask >>= \ DerivedDataTypeReport{..} -> case b of
  -- Rewrite a type declaration of the variable to be converted into the new form.
  -- FIXME: handle references to other converted variables that are found in initialisation expressions
  F.BlStatement a ss lab (F.StDeclaration stA stSS ty attrs (F.AList alA alSS decls)) -> do
    let declNames = map ((FA.varName . declExp) &&& id) decls
    -- Partition declared variables into 'refactor' and 'remain' sets.
    let (declsRef, declsRem) = List.partition ((`M.member` ddtrAMap) . fst) declNames
    let a' | null declsRef = a
           | otherwise     = flip onOrigAnnotation a $ \ orig -> orig { refactored = Just (afterAligned ss) }

    -- Process a variable (and its corresponding declaration).
    let eachVar (var, decl)
          | Just dimMap   <- M.lookup var ddtrAMap
            -- find the DimDecls for the array we are working on
          , dimDeclALists <- universeBi b :: [F.AList F.DimensionDeclarator DA]
          , not (null dimDeclALists)
            -- if DimDecls are specified twice, take the last one:
          , F.AList alDDA alDDSS dimList <- last dimDeclALists
          , dims          <- IM.keys dimMap
          , minDim        <- minimum dims
          , maxDim        <- maximum dims
          , dimDeclAList' <- F.AList alDDA alDDSS (take (minDim - 1) dimList)
          , dimEssences   <- sort $ mapMaybe (\ dim -> (dim,) <$> M.lookup (var, dim) ddtrSMap) dims
          , (_, esset1):_ <- dimEssences
          , ess1:_        <- S.toList esset1 =
          let
            F.TypeSpec tyA tySS _ msel = ty
            ty' = F.TypeSpec tyA tySS (F.TypeCustom $ essTypeName ess1) msel
            a'' = flip onOrigAnnotation a' $ \ orig -> orig { newNode = True }
            ss' = FU.SrcSpan (toCol0 lp) lp where lp = afterAligned ss
            -- FIXME: character length, what to do
            decl' ddAList
              | dds <- F.aStrip ddAList
              , null dds  = F.Declarator (F.getAnnotation decl) (FU.getSpan decl) (declExp decl) F.ScalarDecl Nothing Nothing
              | otherwise = F.Declarator (F.getAnnotation decl) (FU.getSpan decl) (declExp decl) (F.ArrayDecl ddAList) Nothing Nothing

            -- The list of attributes minus any dimension attributes.
            attrs' = descendBi (List.filter (not . isAttrDimension)) attrs
            attrs'' | Just (F.AList _ _ []) <- attrs' = Nothing
                    | otherwise                       = attrs'

            -- Each dimension index number along with its associated type-name.
            dimTypes = flip map dimEssences . second $ \ esset -> case S.toList esset of
              Essence{..}:_ -> F.TypeSpec tyA tySS (F.TypeCustom $ essTypeName) Nothing
              _ -> error "dimTypes: something broken badly: no essences in set"

            -- Process each dimension and return the AST-blocks that define the derived type.
            eachDim :: [F.DimensionDeclarator DA] -> Int -> [(Int, F.TypeSpec DA)] -> [F.Block DA]
            eachDim dimList' maxDim' ((dim, F.TypeSpec _ _ (F.TypeCustom tyName) _):(_, nextTy):rest)
              | Just (Essence{..}:_) <- fmap S.toList $ M.lookup (var, dim) ddtrSMap = let
                  mInit | null rest = declInitialiser decl
                        | otherwise = Nothing
                  dimDeclAList = F.AList a ss $ drop dim dimList'
                  eachLabel (_, lab')
                    | maxDim' == dim &&
                      dim < length dimList' = F.Declarator a ss (FA.genVar a ss lab') (F.ArrayDecl dimDeclAList) Nothing mInit
                    | otherwise             = F.Declarator a ss (FA.genVar a ss lab') F.ScalarDecl Nothing mInit
                  labelDecls = map eachLabel . sort $ IM.toList essLabelMap
                  in [ F.BlStatement a'' ss' Nothing (F.StType stA stSS Nothing tyName)
                     , F.BlStatement a'' ss' Nothing (F.StDeclaration stA stSS nextTy attrs'' (F.AList alA alSS labelDecls))
                     , F.BlStatement a'' ss' Nothing (F.StEndType stA stSS Nothing) ]
            eachDim _ _ _ = []
            in (concat . reverse $ map (eachDim dimList maxDim) (List.tails $ dimTypes ++ [(0, ty)])) ++
               -- The declaration of the variable under the new derived type:
               [F.BlStatement a'' ss' lab (F.StDeclaration stA stSS ty' attrs'' (F.AList alA alSS [decl' dimDeclAList']))]
          | otherwise = []

    let aRem = onOrigAnnotation (\ orig -> orig { deleteNode = null declsRem }) a'
    return $ -- Reinsert any other variables, with the refactored ones removed from the list.
             [F.BlStatement aRem ss lab (F.StDeclaration stA stSS ty attrs (F.AList alA alSS (map snd declsRem)))] ++
             -- Followed by the new set of declarations and derived types.
             concatMap eachVar declsRef

  -- Eliminate comments that were processed.
  F.BlComment a ss _
    | Just spec <- ddtSpec (FA.prevAnnotation a)
    , ddtStStarred spec -> do
        let FU.SrcSpan lp _ = ss
            ss' = deleteLine ss
            a'  = flip onOrigAnnotation a $ \ orig -> orig { refactored = Just lp, deleteNode = True }
        return $ if spec `List.elem` ddtrSpecs
                 then [F.BlComment a' ss' $ F.Comment ""]
                 else [b]
  -- Rewrite references to the converted variable
  F.BlStatement _ _ _ _ -> do
    put False
    b'   <- transformBiM refactorExp b
    flag <- get
    let FU.SrcSpan lb _ = FU.getSpan b'
    if flag
      then return [F.modifyAnnotation (onOrigAnnotation $ \ a -> a { refactored = Just lb }) b']
      else return [b]
  -- FIXME: handle BlDo, etc
  _ -> (:[]) <$> descendBiM refactorBlocks b

isAttrDimension :: F.Attribute DA -> Bool
isAttrDimension F.AttrDimension{} = True; isAttrDimension _ = False

-- | Convert references such as @x(1,2,3)@ into @x % label1 % label2(3)@.
refactorExp :: F.Expression DA -> RefactorM (F.Expression DA)
refactorExp e = do
  smap <- asks ddtrSMap
  case e of
    F.ExpSubscript a s e1 ixAList
      | ixs   <- F.aStrip ixAList
      , list  <- zipWith ixLookup [1..] ixs
      , any SE.isRight list
      , list' <- groupBy ((==) `on` SE.isLeft) list
      , e'    <- foldl' rewrite e1 list'             -> put True >> return e'
        where
          ixLookup dim (F.IxSingle ixA ixS Nothing eIdx)
            | Just i <- FA.constExp (F.getAnnotation eIdx) >>= fromConstInt
            , Just (Essence{..}:_)  <- fmap S.toList $ M.lookup (FA.varName e1, dim) smap
            , Just label            <- IM.lookup (fromIntegral i) essLabelMap = SE.Right (ixA, ixS, label)
          ixLookup _ ix = SE.Left ix

          rewrite e' l@(SE.Left _:_)              = F.ExpSubscript a s e' (F.AList a s $ map SE.fromLeft l)
          rewrite e' (SE.Right (_, _, label):l)   = rewrite (F.ExpDataRef a s e' (F.ExpValue a s (F.ValVariable label))) l
          rewrite e' []                           = e'
    -- FIXME: either convert array slices, or regard that as a disqualifying effect
    _ -> return e

--------------------------------------------------
-- Synthesis helpers

-- Operate on [Block], handling insertion or deletion of blocks
synthBlocks :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> [F.Block DA] -> [F.Block DA]
synthBlocks marker report = concatMap (synthBlock marker report)

-- Synthesise comments where needed, strip existing comment annotations.
synthBlock :: (F.MetaInfo, Char) -> DerivedDataTypeReport -> F.Block DA -> [F.Block DA]
synthBlock (mi, marker) r@DerivedDataTypeReport { ddtrAMap = amap } b = case b of
  -- check if this declaration has variables of interest
  F.BlStatement a ss _ F.StDeclaration{} | vars <- ofInterest b -> genComment vars ++ [b]
    where
      ofInterest b' = filter (flip M.member amap . fst) $
        [ (FA.varName e, FA.srcName e) | F.Declarator _ _ e _ _ _ <- universeBi b' :: [F.Declarator DA] ]

      genComment = map $ \ var ->
        F.BlComment newA newSS . F.Comment . buildCommentText mi space $ marker:genCommentText r var

      -- Set the refactored flag for the reprinter.
      newA  = onOrigAnnotation (\ orig -> orig { refactored = Just lp } ) a

      newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
      FU.SrcSpan lp _ = ss
      space = FU.posColumn lp - 1
  -- strip existing comment annotations
  F.BlComment a ss _ | isJust . ddtSpec . FA.prevAnnotation $ a -> [F.BlComment a' ss' $ F.Comment ""]
    where
      FU.SrcSpan lp _ = ss
      ss' = deleteLine ss
      a'  = flip onOrigAnnotation a $ \ orig -> orig { refactored = Just lp, deleteNode = True }
  -- otherwise leave the Block untouched
  _ -> [descendBi (synthBlocks (mi, marker) r) b]

-- Generate the text that goes into the specification.
genCommentText :: DerivedDataTypeReport -> (F.Name, F.Name) -> String
genCommentText DerivedDataTypeReport{..} (varName, srcName)
  | Just pmap <- M.lookup varName ddtrAMap
  , (dim, set):_ <- IM.toList $ IM.filter (all isJust . S.toList) pmap =
    case M.lookup (varName, dim) ddtrSMap of
      -- generate new comment from scratch
      Nothing
        | nums <- map fromJust $ S.toList set
        , ty   <- varName ++ "_type"
        , labs <- List.intercalate ", " [ str ++ "=>" ++ "label" ++ str | num <- nums, let str = show num ] ->
            " ddt " ++ ty ++ "(" ++ labs ++ ") :: " ++ srcName ++ "(dim=" ++ show dim ++ ")"
      -- generate comment from pre-existing info
      Just essenceSet
        | Essence{..}:_ <- S.toList essenceSet
        , labs          <- List.intercalate ", " [ show i ++ "=>" ++ lab | (i, lab) <- IM.toList essLabelMap ]
        , starStr       <- if essStarred then "* " else " " ->
            " " ++ ddtShort ++ starStr ++ essTypeName ++ "(" ++ labs ++ ") :: " ++ srcName ++ "(dim=" ++ show dim ++ ")"
      _ -> error $ "genCommentText: unable to generate text for " ++ srcName
  | otherwise = error $ "genCommentText: empty pmap entry and/or unable to lookup varName = " ++ varName

--------------------------------------------------
-- Check helpers

-- | From the given piece of AST, returns a list of declared variables
-- (unique form) paired with location.
declaredVars :: Data (f DA) => String -> f DA -> [(F.Name, VInfo)]
declaredVars srcFile x = [ (FA.varName e, VInfo (FA.srcName e) srcFile (FU.getSpan e)) | e <- declaredExps x ]


-- | From the given piece of AST, returns a list of expressions
-- associated with the declarators.
declaredExps :: Data (f DA) => f DA -> [F.Expression DA]
declaredExps x = [ e | d <- universeBi x :: [F.Declarator DA]
                     , e@(F.ExpValue _ _ (F.ValVariable _)) <- [declExp d] ]


-- | Pattern matches the expression from the declarator
declExp :: F.Declarator a -> F.Expression a
declExp (F.Declarator _ _ e _ _ _) = e

-- | Pattern matches the initialiser from the declarator
declInitialiser :: F.Declarator a -> Maybe (F.Expression a)
declInitialiser (F.Declarator _ _ _ _ _ me) = me

-- | Given a parsed specification and variable information, attempts
-- to 'distil' the essence of the specification. If there is a problem
-- with the spec then it returns SE.Left, otherwise SE.Right.
distil :: DDTStatement -> VInfo -> SE.Either IndexError Essence
distil (DDTSt { ddtStStarred = star, ddtStTypeName = tyname, ddtStLabels = labels }) vinfo
  -- if no duplicated indices
  | noDups nums = SE.Right $ Essence { essTypeName = tyname
                                     , essLabelMap = IM.fromList [ (n, lname) | (n, lname) <- labels' ]
                                     , essVInfoSet = S.singleton vinfo
                                     , essStarred  = star }
  -- if there's a problem
  | otherwise   = SE.Left $ IndexDupError tyname vinfo (nums List.\\ List.nub nums)
  where
    labels'   = map (first read) labels
    nums      = map fst labels'
    noDups ns = length (List.nub ns) == length ns

-- | Create an essence from array access information derived in a
-- file, assuming some default names and labels.
distilArrayInfo :: F.Name -> S.Set VInfo -> S.Set Int -> Essence
distilArrayInfo var essVInfoSet dimSet = Essence{..}
  where
    essTypeName = var ++ "_type"
    essLabelMap = IM.fromList [ (n, lname) | n <- S.toList dimSet, let lname = "label" ++ show n ]
    essStarred  = False

-- | Like concatMap, over Sets
setConcatMap :: (Ord a, Ord b) => (a -> [b]) -> S.Set a -> S.Set b
setConcatMap f = S.fromList . concatMap f . S.toList

-- | Turn a list of indexed pairs into an IntMap combining values into sets.
makeIntMapSet :: Ord b => [(Int, b)] -> IM.IntMap (S.Set b)
makeIntMapSet = IM.fromListWith S.union . map (second S.singleton)

-- | Operates like 'sequence' but on an IntMap.
sequenceIntMap :: Monad m => IM.IntMap (m a) -> m (IM.IntMap a)
sequenceIntMap = fmap IM.fromList . sequence . map mstrength . IM.toList

-- | Lifts the monad from the second half of the pair.
mstrength :: Functor f => (a, f b) -> f (a, b)
mstrength (x, my) = fmap (x,) my

--------------------------------------------------
-- Compilation helpers

-- | Generate a new ModFile containing derived datatype information.
genDDTModFile :: Data a => F.ProgramFile (FA.Analysis a) -> DerivedDataTypeReport -> ModFile
genDDTModFile pf ddtr = alterModFileData f ddtCompiledDataLabel $ genModFile pf
  where
    f _ = Just $ encode ddtr

-- | Decode a DerivedDataTypeReport from a ModFile.
mfDerivedDataTypeReport :: ModFile -> DerivedDataTypeReport
mfDerivedDataTypeReport mf = case lookupModFileData ddtCompiledDataLabel mf of
  Nothing -> mempty
  Just bs -> case decodeOrFail bs of
    Left _ -> mempty
    Right (_, _, ddtr) -> ddtr

-- | Combine all reports from ModFiles using (<>)
combinedDerivedDataTypeReport :: ModFiles -> DerivedDataTypeReport
combinedDerivedDataTypeReport = foldMap mfDerivedDataTypeReport

-- | Section name for derived datatype info inside a ModFile.
ddtCompiledDataLabel :: String
ddtCompiledDataLabel = "derived-datatypes-compiled-data"
