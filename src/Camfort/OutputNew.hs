{-# LANGUAGE FlexibleInstances, UndecidableInstances,
    DoAndIfThenElse, MultiParamTypeClasses, FlexibleContexts,
    ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Camfort.OutputNew where

import Prelude hiding (span)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.PrettyPrint as PP
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Version (FortranVersion(..))

import Camfort.Analysis.Annotations
import Camfort.Helpers
import Camfort.Helpers.Syntax

import System.Directory

import qualified Data.ByteString.Char8 as B
import Data.Generics
import Data.Functor.Identity
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Text.Reprinter as RP

import qualified Language.Fortran.Parser.Any as FP
--import qualified Camfort.Specification.DerivedDataType as DDT
import qualified Camfort.Specification.Stencils as CSS
import qualified Camfort.Analysis as CA
import qualified Camfort.Analysis.ModFile as CAM
import qualified Language.Fortran.Util.ModFile as FUM
import qualified Camfort.Output as CO
import qualified Camfort.Reprint as CR
import Control.Lens

--refactor :: SourceText -> F.ProgramUnit Annotation -> SourceText
refactor input ast = runIdentity $ RP.reprint (refactoring Fortran90) ast input

refactoring :: Typeable a => FortranVersion -> a -> Identity (Maybe (RP.RefactorType, SourceText, RP.Span))
refactoring v = catchAll
         `extQ` genReprinting pProgUnit
         `extQ` genReprinting pBlock
         `extQ` genReprinting pStatement
  where
    pprintAll :: PP.IndentablePretty a => a -> Identity SourceText
    pprintAll ast = Identity (B.pack (PP.pprintAndRender v ast Nothing))
    pProgUnit :: F.ProgramUnit A -> Identity SourceText
    pProgUnit = pprintAll
    pBlock :: F.Block A -> Identity SourceText
    pBlock = pprintAll
    pStatement  :: F.Statement A -> Identity SourceText
    pStatement = pprintAll

--mapStateT (\n -> Identity $ n `evalState` 0) (refactorProgramUnits v inp z)

{-
refactorProgramUnits :: FortranVersion -> Reprinting SourceText (State Int)

-- Output comments
refactorProgramUnits _ inp (F.PUComment ann span (F.Comment comment)) = do
    if   pRefactored ann
    then let (FU.SrcSpan lb ub) = span
             (p0, _)            = takeBounds (cursor, lb) inp
              nl                = if null comment then B.empty else B.pack "\n"
          in (put ub >> return (B.concat [p0, B.pack comment, nl], True))
    else return (B.empty, False)

refactorProgramUnits _ _ _ = catchAll
-}

--------------------------------------------------------------------------------

instance (F.Annotated ast, FU.Spanned (ast A)) => Refactorable (ast A) where
    getSpan = rewrapSpan

    -- For now, we ignore the other node annotations (new, delete, stored
    -- cursor), and say all refactors are replaces.
    isRefactored ast =
        if   (pRefactored . F.getAnnotation) ast
        then Just RP.Replace
        else Nothing

-- | Generic getSpan helper function.
rewrapSpan :: FU.Spanned a => a -> RP.Span
rewrapSpan a =
        let FU.SrcSpan p1 p2 = FU.getSpan a
         in (rewrapPos p1, rewrapPos p2)

-- | Convert fortran-src position type to the minimal one used by the reprinter.
rewrapPos :: FU.Position -> RP.Position
rewrapPos pos =
    let Right line = mkLine (FU.posLine   pos)
        Right col  = mkCol  (FU.posColumn pos)
     in (line, col)

--------------------------------------------------------------------------------

testReprint :: IO ()
testReprint = do
  let filename   = "test.f90"
  let fortranVer = Fortran90
  bs <- B.readFile filename
  case FP.fortranParserWithVersion fortranVer bs filename of
    Left  e    -> error "fuck"
    Right ast  -> do
      let astPretty = PP.pprintAndRender fortranVer ast Nothing
      putStrLn astPretty


testReprint' :: IO ()
testReprint' = do
  pf <- CAM.readParseSrcFile (Just fortranVer) [] filename
  case pf of
    Nothing -> error "fuck"
    Just (ast, bs) -> do
      let report = runIdentity . runAnal $ CSS.synth '=' [ast]
      case view CA.arResult report of
        CA.ARFailure _ _ -> error "fuck 2"
        CA.ARSuccess [pf] -> do
          printPf pf
          (B.putStrLn . refactorViaOld bs) pf
          (B.putStrLn . refactor bs) pf
        _ -> error "fuck 3"

  where
    filename   = "test.f90"
    fortranVer = Fortran90
    --runAnal :: AnalysisT e w m a -> m (AnalysisReport e w a)
    runAnal = CA.runAnalysisT filename (CA.logOutputNone True) CA.LogError FUM.emptyModFiles
    tryPrintResultPf :: Either e (F.ProgramFile anno) -> IO ()
    tryPrintResultPf = \case
      Left _ -> error "fuck"
      Right pf -> putStrLn $ PP.pprintAndRender fortranVer pf Nothing
    printPf :: F.ProgramFile a -> IO ()
    printPf = putStrLn . flip (PP.pprintAndRender fortranVer) Nothing
    refactorViaOld input ast = runIdentity $ CR.reprint (CO.refactoring fortranVer) ast input
