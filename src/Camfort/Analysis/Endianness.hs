{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Camfort.Analysis.Endianness where

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
import qualified Language.Fortran.Version as FV
import qualified Language.Fortran.Analysis.SemanticTypes as ST
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR

import Text.PrettyPrint ( Doc, render, parens, (<+>), comma, space )
import Language.Fortran.PrettyPrint ( Pretty(..), )

import qualified Data.Text.Lazy.Builder as Builder
import Data.Text (Text)
import qualified Data.Text as T

import Data.Generics.Uniplate.Operations

instance (Pretty a, Pretty b) => Pretty (a,b) where
    pprint' v (a, b) = parens (pprint' v a <> comma <> space <> pprint' v b)

docToText :: Doc -> Text
docToText = T.pack . render

newtype EndianReport
  = EndianReport [(F.Expression (), F.Expression (), Origin, FV.FortranVersion)]
  deriving Generic

instance NFData EndianReport

instance SG.Semigroup EndianReport where
  EndianReport r1 <> EndianReport r2 = EndianReport $ r1 ++ r2


instance Monoid EndianReport where
    mempty = EndianReport []
    mappend = (SG.<>)

instance Show EndianReport where
    show (EndianReport reports) = show reports

checkEquivalence :: forall a. Data a => F.ProgramFile a -> PureAnalysis String () EndianReport
checkEquivalence pf = do
    let F.ProgramFile (F.MetaInfo v file) _ = pf
    let checkPU :: FAT.TypeEnv -> F.ProgramUnit (FA.Analysis a) -> EndianReport
        checkPU env pu = 
            let equiv_list = [(e1, e2)
                    | F.StEquivalence _ span es <- universeBi (F.programUnitBody pu) :: [F.Statement (FA.Analysis a)]
                    , [e1, e2] <- map AList.aStrip $ AList.aStrip es]
            in mconcat $ map checkPair equiv_list
            where
                checkPair :: (F.Expression (FA.Analysis a), F.Expression (FA.Analysis a)) -> EndianReport
                checkPair (e1@(F.ExpValue _ span1 (F.ValVariable s1)), e2@(F.ExpValue _ span2 (F.ValVariable s2))) = 
                    let t1 = Map.lookup (FA.varName e1) env 
                        t2 = Map.lookup (FA.varName e2) env
                    in case (t1, t2) of
                        (Just (FA.IDType (Just ty1) _), Just (FA.IDType (Just ty2) _)) ->
                            if ST.getTypeSize ty1 == ST.getTypeSize ty2 then
                                EndianReport []
                            else
                                let e1' = F.ExpValue () span1 (F.ValVariable s1)
                                    e2' = F.ExpValue () span2 (F.ValVariable s2)
                                in EndianReport [(e1', e2', atSpannedInFile file [e1, e2], v)]
                        _ -> EndianReport []
                checkPair (_, _) = EndianReport []
        -- make names unique by renaming
    let pf' = FAR.analyseRenames . FA.initAnalysis $ pf
    let (pf'', typeEnv) = FAT.analyseTypes pf'
    let reports = map (checkPU typeEnv) (universeBi pf'')
    return $!! mconcat reports

instance Describe EndianReport where
    describeBuilder (EndianReport results)
        | null results = "no equivalence problems detected"
        | otherwise = Builder.fromText . T.unlines $
            [ describe orig <> " possible endianness portability problem: " <> docToText (pprint' v (e1, e2))
            | (e1, e2, orig, v) <- results ]
            

instance ExitCodeOfReport EndianReport where
    exitCodeOf (EndianReport []) = 0
    exitCodeOf (EndianReport _ ) = 1