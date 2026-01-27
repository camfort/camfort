{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.Equivalence where

import Prelude hiding (unlines)

import Camfort.Analysis
import Camfort.Analysis.Annotations
import Camfort.Helpers.Syntax

import Data.Data
import Control.DeepSeq
import GHC.Generics
import qualified Data.Semigroup as SG

import qualified Data.Map as Map

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.AST.AList as AList
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.PrettyPrint as FAP

import Language.Fortran.Version

import qualified Data.Text.Lazy.Builder as Builder
import Data.Text (unlines, intercalate, pack)

import Data.Generics.Uniplate.Operations

import Debug.Trace

type PULoc = (F.ProgramUnitName, Origin)

data EquivalenceReport = EquivalenceReport [(F.Name, Origin)]
    deriving Generic

instance NFData EquivalenceReport

instance SG.Semigroup EquivalenceReport where
  EquivalenceReport r1 <> EquivalenceReport r2 = EquivalenceReport $ r1 ++ r2


instance Monoid EquivalenceReport where
    mempty = EquivalenceReport []
    mappend = (SG.<>)

instance Show EquivalenceReport where
    show (EquivalenceReport reports) = show reports

checkEquivalence :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () EquivalenceReport
checkEquivalence pf = do
    let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf
    let checkPU :: FAT.TypeEnv -> F.ProgramUnit (FA.Analysis a) -> EquivalenceReport
        checkPU env pu = 
            EquivalenceReport [ ("(" ++ s1 ++ ", " ++ s2 ++ ")", atSpannedInFile file pair)
                    | (F.StEquivalence _ span es) <- universeBi (F.programUnitBody pu) :: [F.Statement (FA.Analysis a)]
                    , pair@[e1@(F.ExpValue _ _ (F.ValVariable s1)), e2@(F.ExpValue _ _ (F.ValVariable s2)) ] <- map AList.aStrip $ AList.aStrip es
                    , not $ type_equiv (FA.varName e1) (FA.varName e2)
                    ]
            where
                type_equiv s1 s2 =
                    let t1 = Map.lookup s1 env in
                    let t2 = Map.lookup s2 env in
                    case (t1, t2) of
                        (Just t1, Just t2) -> t1 == t2
                        _ -> error "should not happen"
        -- make names unique by renaming
    let pf' = FAR.analyseRenames . FA.initAnalysis $ pf
    let (pf'', typeEnv) = FAT.analyseTypes pf'
    let reports = map (checkPU typeEnv) (universeBi pf'')
    return $!! mconcat reports

instance Describe EquivalenceReport where
    describeBuilder (EquivalenceReport results)
        | null results = "no equivalence problems detected"
        | otherwise = Builder.fromText . unlines $
            [ describe orig <> " possible endianness portability problem between variables: " <> pack name
            | (name, orig) <- results ]
            

instance ExitCodeOfReport EquivalenceReport where
    exitCodeOf (EquivalenceReport []) = 0
    exitCodeOf (EquivalenceReport _ ) = 1