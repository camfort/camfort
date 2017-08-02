{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.CheckFrontend where

import           Control.Monad.Writer.Strict              hiding (Product)
import           Data.Generics.Uniplate.Operations

import qualified Language.Fortran.Analysis                as F
import qualified Language.Fortran.Analysis.BBlocks        as F
import qualified Language.Fortran.Analysis.DataFlow       as F
import qualified Language.Fortran.AST                     as F
import qualified Language.Fortran.Util.Position           as F

-- import           Camfort.Analysis.Annotations
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Specification.Parser             (SpecParseError)

import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckBackend
import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Types        (HoareParseError)

newtype CheckResult = CheckResult [HoareResult]

data HoareResult
  = HOkay
  | HFail HoareCheckError
  | HWarn

data HoareCheckError
  = ParseError F.SrcSpan (SpecParseError HoareParseError)
  | LogicError

invariantChecking :: F.ProgramFile (F.Analysis (A ())) -> CheckResult
invariantChecking pf = CheckResult . snd . runWriter $ do
  -- Attempt to parse comments to specifications
  pf' <- annotateComments hoareParser (\srcSpan err -> tell [parseError srcSpan err]) pf

  let results = map checkPU (childrenBi pf')

      -- TODO: More fine grained results 
      reportResult checker =
        case runChecker checker of
          Right True -> HOkay
          _ -> HFail LogicError

  tell (reportResult <$> results)

parseError :: F.SrcSpan -> SpecParseError HoareParseError -> HoareResult
parseError sp err = HFail (ParseError sp err)
