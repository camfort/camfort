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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Camfort.Analysis.DerivedDataType
       ( infer, refactor, check, synth, compile
       , DerivedDataTypeReport(..)
       ) where
import Prelude hiding (unlines)
import Control.Monad
import GHC.Generics (Generic)
import Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (maybeToList, isJust)
import Data.List (foldl')
import Data.Text (Text, unlines, intercalate, pack)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Map.Strict as M
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
import Camfort.Analysis.Annotations  (A)
import Camfort.Helpers (Filename)

--------------------------------------------------

-- | map of information about constants used to index arrays
type AMap = M.Map F.Name (M.Map Int (S.Set (Maybe Integer)))

-- | map of info about vars
type VMap = M.Map F.Name (S.Set VInfo)

data VInfo = VInfo { vSrcName :: F.Name, vFileName :: String, vSrcSpan :: FU.SrcSpan }
  deriving (Generic, Eq, Ord)

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
      [ ("\n"<>pack fileName<>":\n"): [ describe srcSpan <> " derive-datatype " <> pack srcName <> " (" <> intercalate ", " pstrs <> ")"
                                      | (VInfo srcName _ srcSpan, pstrs) <- srcsPstrs ]
      | (fileName, srcsPstrs) <- M.toList byFile]

    where
      perParam Nothing = "*"; perParam (Just k) = describe k
      perName (n, consts) = "{" <> intercalate ";" (map perParam (S.toList consts)) <> "}"
      namePstrs = [ (name, pstrs) | (name, pmap) <- M.toList $ filterCondensedCategoryOne amap
                                  , let pstrs = map perName $ M.toList pmap ]
      srcsPstrs = [ (vFileName vinfo, [(vinfo, pstrs)]) | (name, pstrs) <- namePstrs
                                                        , vinfo <- join (maybeToList (S.toList <$> M.lookup name vmap)) ]
      byFile = M.fromListWith (++) srcsPstrs

--------------------------------------------------

-- | Generate report about derived datatypes in given program file
infer :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
infer pf = do
  mfs <- analysisModFiles
  return . fst $ genProgramFileReport mfs pf

-- | Check annotations relating to derived datatypes in given program file
check :: F.ProgramFile A -> PureAnalysis String () DerivedDataTypeReport
check pf = do
  mfs <- analysisModFiles
  -- FIXME: gather annotations and check consistency
  return mempty

-- | Generate and insert comments about derived datatypes
synth :: [F.ProgramFile A] -> PureAnalysis String () (DerivedDataTypeReport, [Either String (F.ProgramFile A)])
synth pfs = do
  mfs <- analysisModFiles
  forEachProgramFile pfs $ \ pf -> do
    let (report, pf') = genProgramFileReport mfs pf
    -- FIXME: generate datatype names, labels & insert comments
    return $ (report, Right pf)

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

genProgramFileReport :: ModFiles -> F.ProgramFile A -> (DerivedDataTypeReport, F.ProgramFile (FA.Analysis A))
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

-- filter only the interesting ones
filterCondensedCategoryOne :: AMap -> AMap
filterCondensedCategoryOne = M.filter (not . M.null . M.filter (all isJust . S.toList))

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
