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

{-# LANGUAGE ScopedTypeVariables #-}

{- Simple syntactic analysis on Fortran programs -}

module Camfort.Analysis.Simple
 (countVariableDeclarations) where

import Data.Data
import Data.Generics.Uniplate.Operations

import qualified Language.Fortran.AST as F

import Camfort.Analysis (PureAnalysis)

{-| Counts the number of declarations (of variables) in a whole program -}

countVariableDeclarations :: forall a x. Data a => x -> F.ProgramFile a -> PureAnalysis () () Int
countVariableDeclarations _ pf = return $ length (universeBi pf :: [F.Declarator a])
