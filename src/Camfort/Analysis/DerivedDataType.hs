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
       ( inferDerivedDataTypes, DerivedDataTypeReport(..), compileDerivedDataTypes
       ) where
import Prelude hiding (unlines)
import Control.Monad
import GHC.Generics (Generic)
import Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (isJust)
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

data DerivedDataTypeReport = DerivedDataTypeReport AMap
  deriving (Generic)

instance Binary DerivedDataTypeReport

instance SG.Semigroup DerivedDataTypeReport where
  DerivedDataTypeReport m1 <> DerivedDataTypeReport m2 = DerivedDataTypeReport $ M.unionWith (M.unionWith S.union) m1 m2

instance Monoid DerivedDataTypeReport where
  mempty = DerivedDataTypeReport M.empty
  mappend = (SG.<>)

instance ExitCodeOfReport DerivedDataTypeReport where
  exitCodeOf (DerivedDataTypeReport m) | M.null m  = 0
                                       | otherwise = 1

instance Describe DerivedDataTypeReport where
  describeBuilder (DerivedDataTypeReport amap)
    | M.null amap = "no cases detected"
    | otherwise = Builder.fromText $ unlines
      [ pack name <> " (" <> intercalate ", " pstrs <> ")" | (name, pmap) <- M.toList $ filterCondensedCategoryOne amap
                                                          , let pstrs = map perName $ M.toList pmap ]
    where
      perParam Nothing = "*"; perParam (Just k) = describe k
      perName (n, consts) = "{" <> intercalate ";" (map perParam (S.toList consts)) <> "}"

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

-- | Produce a report on potential derived data types
inferDerivedDataTypes :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () DerivedDataTypeReport
inferDerivedDataTypes pf = do
  mfs <- analysisModFiles
  let analysisOutput = analysis mfs pf
  let amap = condenseCategoryOne analysisOutput
  return $ DerivedDataTypeReport amap <> combinedDerivedDataTypeReport mfs

-- analyse bulk data info
analysis :: Data a => ModFiles -> F.ProgramFile a -> [(String, BulkDataArrayInfo)]
analysis mfs pf = accessInfo
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

ddtCompiledDataLabel :: String
ddtCompiledDataLabel = "derived-datatypes-compiled-data"

-- | Compile a program to a 'ModFile' containing derived datatype information.
compileDerivedDataTypes :: () -> ModFiles -> F.ProgramFile A -> IO ModFile
compileDerivedDataTypes _ mfs pf = do
  let (pf', _, _) = withCombinedEnvironment mfs pf
  let analysis = generalizePureAnalysis . inferDerivedDataTypes $ pf
  out <- runAnalysisT (F.pfGetFilename pf) (logOutputNone True) LogError mfs analysis

  case out ^? arResult . _ARSuccess of
    Just report -> return $ genDDTModFile pf' report
    Nothing -> fail "compileDerivedDataTypes: analysis failed"

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
