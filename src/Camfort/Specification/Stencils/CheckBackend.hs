{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckBackend where

import Data.Data
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product,Sum)

import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Syntax
import Camfort.Specification.Stencils.Model
import qualified Camfort.Specification.Stencils.Grammar as SYN

import Camfort.Analysis.Annotations
import Camfort.Helpers.Vec
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers hiding (lineCol, spanLineCol)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD

import Language.Fortran.Util.Position
import qualified Data.Map as M
import Data.Set hiding (map)

type ErrorMsg = String

-- Class for functions converting from Grammar parse
-- syntax to the AST representation of the Syntax module
class SynToAst s t | s -> t where
  synToAst :: (?renv :: RegionEnv) => s -> Either ErrorMsg t

-- Top-level conversion of declarations
instance SynToAst SYN.Specification (Either RegionEnv SpecDecls) where
  synToAst (SYN.SpecDec spec vars) = do
     spec' <- synToAst spec
     return $ Right [(vars, spec')]

  synToAst (SYN.RegionDec rvar region) = do
     spec' <- synToAst region
     return $ Left [(rvar, spec')]

-- Convert temporal or spatial specifications
instance SynToAst SYN.Spec Specification where
  synToAst (SYN.Spatial mods r) = do
    (modLinear, approx) <- synToAst mods
    r' <- synToAst r
    let s' = Spatial modLinear r'
    return $ Specification $
       case approx of
        Just SYN.AtMost  -> Bound Nothing (Just s')
        Just SYN.AtLeast -> Bound (Just s') Nothing
        Nothing          -> Exact s'

-- Convert region definitions into the DNF-form used internally
instance SynToAst SYN.Region RegionSum where
  synToAst = dnf

-- Convert a grammar syntax to Disjunctive Normal Form AST
dnf :: (?renv :: RegionEnv) => SYN.Region -> Either ErrorMsg RegionSum

-- Distributive law
dnf (SYN.And r1 r2) = do
    r1' <- dnf r1
    r2' <- dnf r2
    return $ Sum $ unSum r1' >>= (\(Product ps1) ->
                    unSum r2' >>= (\(Product ps2) ->
                      return $ Product $ ps1 ++ ps2))
-- Coalesce sums
dnf (SYN.Or r1 r2) = do
    r1' <- dnf r1
    r2' <- dnf r2
    return $ Sum $ unSum r1' ++ unSum r2'
-- Region conversion
dnf (SYN.Forward dep dim reflx)  = return $ Sum [Product [Forward dep dim reflx]]
dnf (SYN.Backward dep dim reflx) = return $ Sum [Product [Backward dep dim reflx]]
dnf (SYN.Centered dep dim reflx) = return $ Sum [Product [Centered dep dim reflx]]
dnf (SYN.Var v)            =
    case lookup v ?renv of
      Nothing -> Left $ "Error: region " ++ v ++ " is not in scope."
      Just rs -> return rs

-- Convert modifier list to modifier info
instance SynToAst [SYN.Mod]
                  (Linearity, Maybe SYN.Mod) where
  synToAst mods = return (linearity, approx)
    where
      linearity = if SYN.ReadOnce `elem` mods then Linear else NonLinear

      approx = find' isApprox mods
      isApprox SYN.AtMost  = Just SYN.AtMost
      isApprox SYN.AtLeast = Just SYN.AtLeast
      isApprox _           = Nothing

find' :: Eq a => (a -> Maybe b) -> [a] -> Maybe b
find' p [] = Nothing
find' p (x : xs) =
  case p x of
    Nothing -> find' p xs
    Just b  -> Just b

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
