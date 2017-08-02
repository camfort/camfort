{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.CheckBackend where

import           Control.Monad.Reader               (MonadReader, ReaderT, ask,
                                                     runReaderT)
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict        hiding (Product)
import           Data.Function                      (on)
import           Data.Generics.Uniplate.Operations

import qualified Language.Fortran.Analysis          as F
import qualified Language.Fortran.Analysis.BBlocks  as F
import qualified Language.Fortran.Analysis.DataFlow as F
import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Util.Position     as F

-- import           Camfort.Analysis.Annotations
import           Camfort.Analysis.CommentAnnotator
import           Camfort.Specification.Parser       (SpecParseError)

import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.Types  (HoareParseError)


newtype Checker a = Checker ( a)


runChecker :: Checker a -> Either () a
runChecker = undefined


checkPU :: F.ProgramFile (F.Analysis (A ())) -> Checker Bool
checkPU pf = undefined
