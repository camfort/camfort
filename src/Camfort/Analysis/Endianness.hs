{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.Endianness 
  ( checkEndianSensitive
  , CheckEndianReport(..)
  ) where

import Control.DeepSeq
import Data.Data
import Data.Generics.Uniplate.Operations
import qualified Data.Semigroup as SG
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text (unlines)
import GHC.Generics
import Prelude hiding (unlines)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as F

import Camfort.Analysis 
  ( ExitCodeOfReport(..)
  , PureAnalysis
  , analysisModFiles
  , Describe(..)
  , describe
  , describeBuilder
  , atSpannedInFile
  , Origin
  )
import Camfort.Analysis.ModFile (withCombinedEnvironment)

--------------------------------------------------

type PULoc = (F.ProgramUnitName, Origin)

data CheckEndianReport
  = CheckEndianReport { endianSensitiveOps :: [PULoc] }
  deriving Generic
instance NFData CheckEndianReport
instance SG.Semigroup CheckEndianReport where
  CheckEndianReport a1 <> CheckEndianReport a2 = CheckEndianReport (a1 ++ a2)

instance Monoid CheckEndianReport where
  mempty = CheckEndianReport []
  mappend = (SG.<>)

-- | Check for endian-sensitive operations in Fortran code
-- This is a stub implementation that detects potential endianness issues
checkEndianSensitive :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () CheckEndianReport
checkEndianSensitive pf = do
  let F.ProgramFile F.MetaInfo { F.miFilename = file } _ = pf
  mfs <- analysisModFiles
  let (pf', _, _) = withCombinedEnvironment mfs pf

  let checkPU :: F.ProgramUnit (F.Analysis a) -> CheckEndianReport
      checkPU pu = CheckEndianReport {..}
        where
          -- Stub: detect nothing for now
          -- TODO: Implement actual detection of:
          -- - Binary I/O operations (READ/WRITE with FORM='UNFORMATTED')
          -- - Transfer intrinsics
          -- - EQUIVALENCE statements that may reinterpret data
          -- - COMMON statement that may reinterpret data
          endianSensitiveOps = []

  let reports = map checkPU (universeBi pf')

  return $!! mconcat reports

instance Describe CheckEndianReport where
  describeBuilder (CheckEndianReport {..})
    | null endianSensitiveOps = "no cases detected"
    | otherwise = Builder.fromText . unlines $
      [ describe orig <> " potentially endian-sensitive operation detected."
      | (_, orig) <- endianSensitiveOps ]

instance ExitCodeOfReport CheckEndianReport where
  exitCodeOf (CheckEndianReport {..})
    | null endianSensitiveOps = 0
    | otherwise = 1
