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
{-# LANGUAGE OverloadedStrings #-}

{- Simple syntactic analysis on Fortran programs -}

module Camfort.Analysis.Simple
 (countVariableDeclarations, checkImplicitNone) where
import Prelude hiding (unlines)
import Data.Data
import Data.Text (Text, unlines)
import Data.Monoid ((<>))
import Data.Generics.Uniplate.Operations
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder


import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as F

import Camfort.Analysis (describe, describeBuilder, PureAnalysis, Describe)

{-| Counts the number of declarations (of variables) in a whole program -}

countVariableDeclarations :: forall a. Data a => F.ProgramFile a -> PureAnalysis () () Int
countVariableDeclarations pf = return $ length (universeBi pf :: [F.Declarator a])

data ImplicitNoneReport = ImplicitNoneReport [(F.ProgramUnitName, F.SrcSpan)]

-- Follows host scoping unit rule: 'external' program units
-- (ie. appear at top level) must have implicit-none statements, which
-- are then inherited by internal subprograms. Also, subprograms of
-- interface blocks must have implicit-none statements. FIXME: when we
-- have Fortran 2008 support then submodules must also be checked.
checkImplicitNone :: forall a. Data a => F.ProgramFile a -> PureAnalysis () () ImplicitNoneReport
checkImplicitNone pf = do
  let checkedPUs = [ puHelper pu | pu <- childrenBi pf :: [F.ProgramUnit a] ]
  let checkedInterfaces = [ puHelper pu | int@(F.BlInterface {}) <- universeBi pf :: [F.Block a]
                                        , pu <- childrenBi int :: [F.ProgramUnit a] ]
  return . ImplicitNoneReport . concat $ checkedPUs ++ checkedInterfaces
  where
    isUseStmt (F.BlStatement _ _ _ (F.StUse {})) = True; isUseStmt _ = False
    isImplicitNone (F.BlStatement _ _ _ (F.StImplicit _ _ Nothing)) = True; isImplicitNone _ = False
    isImplicitSome (F.BlStatement _ _ _ (F.StImplicit _ _ (Just _))) = True; isImplicitSome _ = False

    checkPU :: F.ProgramUnit a -> Bool
    checkPU pu = case pu of
      F.PUMain _ _ _ bs _              -> checkBlocks bs
      F.PUModule _ _ _ bs _            -> checkBlocks bs
      F.PUSubroutine _ _ _ _ _ bs _    -> checkBlocks bs
      F.PUFunction _ _ _ _ _ _ _ bs _  -> checkBlocks bs
      _                                -> True

    checkBlocks bs = all (not . isImplicitSome) bs && all isUseStmt useStmts && not (null rest)
      where
        (useStmts, rest) = break isImplicitNone bs

    puHelper pu = if checkPU pu then [] else [(F.getName pu, F.getSpan pu)]


instance Describe ImplicitNoneReport where
  describeBuilder (ImplicitNoneReport results) = Builder.fromText . unlines . map descResult $ results
    where
      descResult (F.Named puName, srcSpan) = describe srcSpan <> " " <> describe puName
