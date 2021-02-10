{-# LANGUAGE FlexibleInstances, UndecidableInstances,
    DoAndIfThenElse, MultiParamTypeClasses, FlexibleContexts,
    ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Camfort.OutputNew where

import Prelude hiding (span)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.PrettyPrint as PP
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Version (FortranVersion(..))

import Camfort.Analysis.Annotations
import Camfort.Reprint
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

--refactor :: SourceText -> F.ProgramUnit Annotation -> SourceText
refactor input ast = runIdentity $ RP.reprint (refactoring Fortran90) ast input

--refactoring :: FortranVersion -> F.ProgramUnit A -> Identity (Maybe (RP.RefactorType, SourceText, RP.Span))
refactoring :: FortranVersion -> Reprinting SourceText Identity
--refactoring v = catchAll `extQ` refactorProgramUnits v
refactoring v = catchAll `extQ` genReprinting f
  where
    f :: F.ProgramUnit A -> Identity SourceText
    f ast = Identity (B.pack (PP.pprintAndRender v ast Nothing))

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
