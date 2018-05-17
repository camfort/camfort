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

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE TupleSections          #-}

module Camfort.Specification.Stencils.CheckBackend
  (
    -- * Classes
    SynToAst(..)
    -- * Errors
  , SynToAstError
  , regionNotInScope
    -- * Helpers
  , checkOffsetsAgainstSpec
  ) where

import           Algebra.Lattice (joins1)
import           Control.Arrow (second)
import           Data.Function (on)
import           Data.Int (Int64)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import qualified Camfort.Helpers.Vec as V
import qualified Camfort.Specification.Stencils.Consistency as C
import           Camfort.Specification.Stencils.Model
import qualified Camfort.Specification.Stencils.Parser.Types as SYN
import           Camfort.Specification.Stencils.Syntax

data SynToAstError = RegionNotInScope String
  deriving (Eq)

regionNotInScope :: String -> SynToAstError
regionNotInScope = RegionNotInScope

instance Show SynToAstError where
  show (RegionNotInScope r) = "Error: region " ++ r ++ " is not in scope."

-- Class for functions converting from Parser parse
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

-- *** Other Helpers

checkOffsetsAgainstSpec :: [(Variable, Multiplicity [[Int]])]
                        -> [(Variable, Specification)]
                        -> Bool
checkOffsetsAgainstSpec offsetMaps specMaps =
  variablesConsistent && all specConsistent specToVecList
  where
    variablesConsistent =
      let vs1 = sort . fmap fst $ offsetMaps
          vs2 = sort . fmap fst $ specMaps
      in vs1 == vs2
    specConsistent spec =
      case spec of
        (spec', Once (V.VL vs)) -> spec' `C.consistent` (Once . toUNF) vs == C.Consistent
        (spec', Mult (V.VL vs)) -> spec' `C.consistent` (Mult . toUNF) vs == C.Consistent
    toUNF :: [ V.Vec n Int64 ] -> UnionNF n Offsets
    toUNF = joins1 . NE.fromList . map (return . fmap intToSubscript)

    -- This function generates the special offsets subspace, subscript,
    -- that either had one element or is the whole set.
    intToSubscript :: Int64 -> Offsets
    intToSubscript i
      | fromIntegral i == absoluteRep = SetOfIntegers
      | otherwise = Offsets . S.singleton $ i

    -- Convert list of list of indices into vectors and wrap them around
    -- existential so that we don't have to prove they are all of the same
    -- size.
    specToVecList :: [ (Specification, Multiplicity (V.VecList Int64)) ]
    specToVecList = map (second (fmap V.fromLists)) specToIxs

    specToIxs :: [ (Specification, Multiplicity [ [ Int64 ] ]) ]
    specToIxs = pairWithFst specMaps (map (second toInt64) offsetMaps)

    toInt64 :: Multiplicity [ [ Int ] ] -> Multiplicity [ [ Int64 ] ]
    toInt64 = fmap (map (map fromIntegral))

    -- Given two maps for each key in the first map generate a set of
    -- tuples matching the (val,val') where val and val' are corresponding
    -- values from each set.
    pairWithFst :: Eq a => [ (a, b) ] -> [ (a, c) ] -> [ (b, c) ]
    pairWithFst [] _ = []
    pairWithFst ((key, val):xs) ys =
      map ((val,) . snd) (filter ((key ==) . fst) ys) ++ pairWithFst xs ys

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
