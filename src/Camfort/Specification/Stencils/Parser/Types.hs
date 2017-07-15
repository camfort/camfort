{- |
Module      :  Camfort.Specification.Stencils.Parser.Types
Description :  Defines the representation of stencil specifications resulting from parsing.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Specification.Stencils.Parser.Types
  ( Specification(..)
  , Region(..)
  , SpecInner(..)
  , reqRegions
  ) where

import Data.Data (Data, Typeable)
import Data.List (nub, sort)

import           Camfort.Specification.Stencils.Model
  (Approximation(..), Multiplicity(..))
import qualified Camfort.Specification.Stencils.Syntax as Syn

data Specification
  = RegionDec String Region
  | SpecDec SpecInner [String]
  deriving (Show, Eq, Typeable, Data)

-- | Regions that are referenced in a specification.
reqRegions :: Specification -> [Syn.Variable]
reqRegions spec = nub . sort $
  case spec of
    RegionDec _ r             -> reqRegions' r
    SpecDec (SpecInner x _) _ ->
      case x of
        Once a -> reqRegionsApprox a
        Mult a -> reqRegionsApprox a
  where
    reqRegionsApprox (Exact r) = reqRegions' r
    reqRegionsApprox (Bound l u) =
      let maybeReqRegions = maybe [] reqRegions'
      in maybeReqRegions l ++ maybeReqRegions u
    reqRegions' :: Region -> [Syn.Variable]
    reqRegions' RegionConst{} = []
    reqRegions' (Or r1 r2)    = reqRegions' r1 ++ reqRegions' r2
    reqRegions' (And r1 r2)   = reqRegions' r1 ++ reqRegions' r2
    reqRegions' (Var v)       = [v]

data Region
  = RegionConst Syn.Region
  | Or Region Region
  | And Region Region
  | Var String
  deriving (Show, Eq, Ord, Typeable, Data)

data SpecInner = SpecInner
    (Multiplicity (Approximation Region))  -- main specification content
    Syn.IsStencil                          -- a bool: stencil or access
  deriving (Show, Eq, Typeable, Data)
