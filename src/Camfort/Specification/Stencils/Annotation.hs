{- |
Module      :  Camfort.Specification.Stencils.Annotation
Description :  Annotation with stencil information.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental

Defines the 'StencilAnnotation' datatype, which is used for annotating a
'ProgramFile' with stencil information.
-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Camfort.Specification.Stencils.Annotation
  (
    StencilAnnotation
  , SA
  , mkStencilAnnotation
  -- ** Specification Annotation Helpers
  , getAstSpec
  , getParseSpec
  , getRegionSpec
  , getStencilBlock
  , giveAstSpec
  , giveParseSpec
  , giveRegionSpec
    -- ** Base Annotation
  , getBaseAnnotation
  , modifyBaseAnnotation
  ) where

import Data.Data (Data)

import qualified Language.Fortran.AST      as F
import qualified Language.Fortran.Analysis as FA

import qualified Camfort.Analysis.Annotations                as Ann
import           Camfort.Analysis.CommentAnnotator
import qualified Camfort.Specification.Stencils.Parser.Types as Gram
import qualified Camfort.Specification.Stencils.Syntax       as Syn

-- | Specification annotation.
data SpecAnnotation
  -- | Unprocessed syntax tree.
  = ParserSpec Gram.Specification
  -- | Region definition.
  | RegionDecl Syn.RegionDecl
  -- | Normalised AST specification.
  | ASTSpec Syn.SpecDecls
  deriving (Eq, Show, Data)

data StencilAnnotation a = StencilAnnotation  {
      prevAnnotation :: a
      -- | Assocatated specification.
    , stencilSpec    :: Maybe SpecAnnotation
      -- | Associated assignment.
    , stencilBlock   :: Maybe (F.Block (FA.Analysis (StencilAnnotation a)))
    } deriving (Show, Eq, Data)

-- | Create a new stencil annotation.
mkStencilAnnotation :: a -> StencilAnnotation a
mkStencilAnnotation a = StencilAnnotation
  { prevAnnotation = a
  , stencilSpec    = Nothing
  , stencilBlock   = Nothing
  }

-- | Convenience name for common annotation type.
type SA = FA.Analysis (StencilAnnotation Ann.A)

modifyBaseAnnotation :: (Ann.A -> Ann.A) -> SA -> SA
modifyBaseAnnotation f = Ann.onPrev (\ann -> ann { prevAnnotation = f (prevAnnotation ann) })

-- | Retrieve the underlying (base) annotation from a stencil annotation.
getBaseAnnotation :: SA -> Ann.A
getBaseAnnotation = prevAnnotation . FA.prevAnnotation

setSpec :: SpecAnnotation -> SA -> SA
setSpec s = Ann.onPrev (\ann -> ann { stencilSpec = Just s })

-- | Set the annotation's stencil specification to a parsed specification.
giveParseSpec :: Gram.Specification -> SA -> SA
giveParseSpec spec = setSpec (ParserSpec spec)

-- | Set the annotation's stencil specification to a region alias.
giveRegionSpec :: Syn.RegionDecl -> SA -> SA
giveRegionSpec spec = setSpec (RegionDecl spec)

-- | Set the annotation's stencil specification to a normalized specification.
giveAstSpec :: Syn.SpecDecls -> SA -> SA
giveAstSpec spec = setSpec (ASTSpec spec)

getSA :: SA -> StencilAnnotation Ann.A
getSA = FA.prevAnnotation

getSpec :: SA -> Maybe SpecAnnotation
getSpec = stencilSpec . getSA

-- | Retrieve a parsed specification from an annotation.
getParseSpec :: SA -> Maybe Gram.Specification
getParseSpec s = case getSpec s of
  (Just (ParserSpec spec)) -> Just spec
  _                        -> Nothing

-- | Retrieve a region environment from an annotation.
getRegionSpec :: SA -> Maybe Syn.RegionDecl
getRegionSpec s = case getSpec s of
  (Just (RegionDecl renv)) -> Just renv
  _                        -> Nothing

-- | Retrieve a normalized specification from an annotation.
getAstSpec :: SA -> Maybe Syn.SpecDecls
getAstSpec s = case getSpec s of
  (Just (ASTSpec ast)) -> Just ast
  _                    -> Nothing

getStencilBlock :: SA -> Maybe (F.Block SA)
getStencilBlock = stencilBlock . getSA

{- *** Routines for associating annotations to ASTs -}

-- Instances for embedding parsed specifications into the AST
instance ASTEmbeddable SA Gram.Specification where
  annotateWithAST ann ast =
    Ann.onPrev (\ann' -> ann' { stencilSpec = Just $ ParserSpec ast }) ann

instance Linkable SA where
  link ann b@F.BlDo{} =
      Ann.onPrev (\ann' -> ann' { stencilBlock = Just b }) ann
  link ann (b@(F.BlStatement _ _ _ F.StExpressionAssign{})) =
      Ann.onPrev (\ann' -> ann' { stencilBlock = Just b }) ann
  link   ann _ = ann
  linkPU ann _ = ann
