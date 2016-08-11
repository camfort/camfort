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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Camfort.Analysis.IntermediateReps where

import Data.Data

import Language.Fortran

data AccessP p = VarA String | ArrayA String [Expr p] deriving (Eq, Typeable, Data)

type Access = AccessP ()

accessToVarName :: AccessP a -> Variable
accessToVarName (VarA v) = v
accessToVarName (ArrayA v _) = v

instance Show (AccessP ()) where
    show (VarA s) = s
    show (ArrayA v es) = v ++ "(...)"