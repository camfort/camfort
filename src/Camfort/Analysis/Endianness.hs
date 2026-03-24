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
import Control.Monad (void)
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
            mconcat [checkPair e1 e2 (atSpannedInFile file eq)
                    | eq@(F.StEquivalence _ span es) <- universeBi (F.programUnitBody pu) :: [F.Statement (FA.Analysis a)]
                    , [e1, e2] <- map AList.aStrip $ AList.aStrip es]
            where
                checkPair :: F.Expression (FA.Analysis a) -> F.Expression (FA.Analysis a) -> Origin -> EndianReport
                -- equivalence (a, b) 
                -- this is safe in case a and b have the same size
                checkPair e1@F.ExpValue{} e2@F.ExpValue{} s = 
                    if sameSize e1 e2 then EndianReport [] else EndianReport [(void e1, void e2, s, v)]

                -- equivalence (a, b(..))
                -- this is safe in case a and base-type of b have the same size
                checkPair e1@F.ExpValue{} e2@(F.ExpSubscript _ _ e2' _) s = 
                    if sameSize e1 e2' then EndianReport [] else EndianReport [(void e1, void e2, s, v)]
                checkPair e1@(F.ExpSubscript _ _ e1' _) e2@F.ExpValue{} s = 
                    if sameSize e1' e2 then EndianReport [] else EndianReport [(void e1, void e2, s, v)]

                -- equivalence (a(..), b(..))
                -- this is safe in case the basetypes have the same size
                checkPair e1@(F.ExpSubscript _ _ e1' _) e2@(F.ExpSubscript _ _ e2' _) s = 
                    if sameSize e1' e2' then EndianReport [] else EndianReport [(void e1, void e2, s, v)]

                checkPair e1 e2 _ = error $ "[error] checkPair: " ++ render (pprint' v e1) ++ ", " ++ render (pprint' v e2)

                sameSize :: F.Expression (FA.Analysis a) -> F.Expression (FA.Analysis a) -> Bool
                sameSize e1 e2 =
                    let t1 = Map.lookup (FA.varName e1) env
                        t2 = Map.lookup (FA.varName e2) env
                    in case (t1, t2) of
                        (Just (FA.IDType (Just ty1) _), Just (FA.IDType (Just ty2) _)) ->
                            ST.getTypeSize ty1 == ST.getTypeSize ty2
                        _ -> False
                                
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
