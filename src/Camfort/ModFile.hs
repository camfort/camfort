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

{-

  Format of Camfort precompiled files with information about Fortran modules.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.ModFile

where

import qualified Debug.Trace as D

import Data.Data
import Data.List
import Data.Char
import Data.Maybe
import Data.Generics.Uniplate.Operations
import qualified Data.Map.Strict as M
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR

import Camfort.Specification.Units.Environment

--------------------------------------------------

modFileSuffix = ".camfortmod"

-- | Function/subroutine name -> associated, parametric polymorphic constraints
type TemplateMap = M.Map F.Name Constraints

data ModFile = ModFile { mfModuleMap :: FAR.ModuleMap
                       , mfTemplateMap :: TemplateMap }
  deriving (Ord, Eq, Show, Data, Typeable, Generic)

instance Binary ModFile

emptyModFile = ModFile M.empty M.empty

genModFile :: forall a. Data a => F.ProgramFile (FA.Analysis a) -> TemplateMap -> ModFile
genModFile pf tmap = emptyModFile
  { mfModuleMap = M.fromList [ (n, env) | pu@(F.PUModule {}) <- universeBi pf :: [F.ProgramUnit (FA.Analysis a)]
                                        , let a = F.getAnnotation pu
                                        , let n = F.getName pu
                                        , env <- maybeToList (FA.moduleEnv a) ]
  , mfTemplateMap = tmap}

encodeModFile :: ModFile -> B.ByteString
encodeModFile = LB.toStrict . encode

combinedModuleMap :: [ModFile] -> FAR.ModuleMap
combinedModuleMap = M.unions . map mfModuleMap

combinedTemplateMap :: [ModFile] -> TemplateMap
combinedTemplateMap = M.unions . map mfTemplateMap

--------------------------------------------------

-- | Some options about how to handle literals.
data LiteralsOpt
  = LitPoly     -- ^ All literals are polymorphic.
  | LitUnitless -- ^ All literals are unitless.
  | LitMixed    -- ^ The literal "0" or "0.0" is fully parametric
                -- polymorphic. All other literals are monomorphic,
                -- possibly unitless.
  deriving (Show, Eq, Ord, Data)

instance Read LiteralsOpt where
  readsPrec _ s = case find ((`isPrefixOf` map toLower s) . fst) ms of
                    Just (str, con) -> [(con, drop (length str) s)]
                    Nothing         -> []
    where
      ms = [ ("poly", LitPoly), ("unitless", LitUnitless), ("mixed", LitMixed)
           , ("litpoly", LitPoly), ("litunitless", LitUnitless), ("litmixed", LitMixed) ]

-- | Options for the unit solver
data UnitOpts = UnitOpts
  { uoDebug          :: Bool                      -- ^ debugging mode?
  , uoLiterals       :: LiteralsOpt               -- ^ how to handle literals
  , uoNameMap        :: FAR.NameMap               -- ^ map of unique names to original names
  , uoModFiles       :: M.Map String ModFile      -- ^ map of included modules
  }
  deriving (Show, Data, Eq, Ord)

unitOpts0 :: UnitOpts
unitOpts0 = UnitOpts False LitMixed M.empty M.empty
