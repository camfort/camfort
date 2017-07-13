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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}

module Camfort.Specification.Stencils.CheckBackend
  (
    -- * Classes
    SynToAst(..)
    -- * Errors
  , SynToAstError
  , regionNotInScope
  ) where

import Data.Function (on)

import Camfort.Specification.Stencils.Syntax
import Camfort.Specification.Stencils.Model
import qualified Camfort.Specification.Stencils.Grammar as SYN

data SynToAstError = RegionNotInScope String
  deriving (Eq)

regionNotInScope :: String -> SynToAstError
regionNotInScope = RegionNotInScope

instance Show SynToAstError where
  show (RegionNotInScope r) = "Error: region " ++ r ++ " is not in scope."

-- Class for functions converting from Grammar parse
-- syntax to the AST representation of the Syntax module
class SynToAst s t | s -> t where
  synToAst :: (?renv :: RegionEnv) => s -> Either SynToAstError t

-- Top-level conversion of declarations
instance SynToAst SYN.Specification (Either RegionDecl SpecDecl) where
  synToAst (SYN.SpecDec spec vars) = do
     spec' <- synToAst spec
     return $ Right (vars, spec')

  synToAst (SYN.RegionDec rvar region) = do
     spec' <- synToAst region
     return $ Left (rvar, spec')

-- Convert temporal or spatial specifications
instance SynToAst SYN.SpecInner Specification where
  synToAst (SYN.SpecInner spec isStencil) = do
    spec' <- synToAst spec
    return $ Specification spec' isStencil

instance SynToAst (Multiplicity (Approximation SYN.Region)) (Multiplicity (Approximation Spatial)) where
  synToAst (Once a) = fmap Once . synToAst $ a
  synToAst (Mult a) = fmap Mult . synToAst $ a

instance SynToAst (Approximation SYN.Region) (Approximation Spatial) where
  synToAst (Exact s)     = fmap (Exact . Spatial) . synToAst $ s
  synToAst (Bound s1 s2) = (Bound `on` (fmap Spatial)) <$> synToAst s1 <*> synToAst s2

instance SynToAst (Maybe SYN.Region) (Maybe RegionSum) where
  synToAst Nothing  = pure Nothing
  synToAst (Just r) = fmap Just . synToAst $ r

-- Convert region definitions into the DNF-form used internally
instance SynToAst SYN.Region RegionSum where
  synToAst = dnf

-- Convert a grammar syntax to Disjunctive Normal Form AST
dnf :: (?renv :: RegionEnv) => SYN.Region -> Either SynToAstError RegionSum

dnf (SYN.RegionConst rconst) = pure . Sum $ [Product [rconst]]
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
dnf (SYN.Var v)              =
    case lookup v ?renv of
      Nothing -> Left (RegionNotInScope v)
      Just rs -> return rs

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
