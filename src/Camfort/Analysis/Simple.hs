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
 (countVariableDeclarations, checkImplicitNone, ImplicitNoneReport(..)) where
import Prelude hiding (unlines)
import Control.Monad
import Data.Data
import qualified Data.Semigroup as SG
import Data.Monoid ((<>))
import Data.Generics.Uniplate.Operations
import qualified Data.Text.Lazy.Builder as Builder


import qualified Language.Fortran.AST as F

import Camfort.Analysis ( ExitCodeOfReport(..), atSpanned, Origin
                        , logError, describe, describeBuilder
                        , PureAnalysis, Describe )

{-| Counts the number of declarations (of variables) in a whole program -}

newtype VarCountReport = VarCountReport Int
instance ExitCodeOfReport VarCountReport where
  exitCodeOf _ = 0
instance Describe VarCountReport where
  describeBuilder (VarCountReport c) = Builder.fromText $ describe c

countVariableDeclarations :: forall a. Data a => F.ProgramFile a -> PureAnalysis () () VarCountReport
countVariableDeclarations pf = return . VarCountReport $ length (universeBi pf :: [F.Declarator a])

type PULoc = (F.ProgramUnitName, Origin)
data ImplicitNoneReport
  = ImplicitNoneReport [PULoc] -- ^ list of program units identified as needing implicit none

instance SG.Semigroup ImplicitNoneReport where
  ImplicitNoneReport r1 <> ImplicitNoneReport r2 = ImplicitNoneReport $ r1 ++ r2

instance Monoid ImplicitNoneReport where
  mempty = ImplicitNoneReport []
  mappend = (SG.<>)

-- If allPU is False then function obeys host scoping unit rule:
-- 'external' program units (ie. appear at top level) must have
-- implicit-none statements, which are then inherited by internal
-- subprograms. Also, subprograms of interface blocks must have
-- implicit-none statements. If allPU is True then simply checks all
-- program units. FIXME: when we have Fortran 2008 support then
-- submodules must also be checked.
checkImplicitNone :: forall a. Data a => Bool -> F.ProgramFile a -> PureAnalysis String () ImplicitNoneReport
checkImplicitNone allPU pf = do
  checkedPUs <- if allPU
                then sequence [ puHelper pu | pu <- universeBi pf :: [F.ProgramUnit a] ]
                     -- host scoping unit rule + interface exception:
                else sequence ( [ puHelper pu | pu <- childrenBi pf :: [F.ProgramUnit a] ] ++
                                [ puHelper pu | int@(F.BlInterface {}) <- universeBi pf :: [F.Block a]
                                              , pu <- childrenBi int :: [F.ProgramUnit a] ] )
  forM_ checkedPUs $ \ r -> case r of
     ImplicitNoneReport [(F.Named name, orig)] -> logError orig name
     _ -> return ()

  return $ mconcat checkedPUs

  where
    isUseStmt (F.BlStatement _ _ _ (F.StUse {})) = True
    isUseStmt _ = False
    isComment (F.BlComment {}) = True
    isComment _ = False
    isUseOrComment b = isUseStmt b || isComment b

    isImplicitNone (F.BlStatement _ _ _ (F.StImplicit _ _ Nothing)) = True; isImplicitNone _ = False
    isImplicitSome (F.BlStatement _ _ _ (F.StImplicit _ _ (Just _))) = True; isImplicitSome _ = False

    checkPU :: F.ProgramUnit a -> Bool
    checkPU pu = case pu of
      F.PUMain _ _ _ bs _              -> checkBlocks bs
      F.PUModule _ _ _ bs _            -> checkBlocks bs
      F.PUSubroutine _ _ _ _ _ bs _    -> checkBlocks bs
      F.PUFunction _ _ _ _ _ _ _ bs _  -> checkBlocks bs
      _                                -> True

    checkBlocks bs = all (not . isImplicitSome) bs && all isUseOrComment useStmts && not (null rest) && all (not . isUseStmt) rest
      where
        (useStmts, rest) = break isImplicitNone bs

    puHelper pu
      | checkPU pu = return mempty
      | otherwise = fmap (\ o -> ImplicitNoneReport [(F.getName pu, o)]) $ atSpanned pu


instance Describe ImplicitNoneReport where
  describeBuilder (ImplicitNoneReport results)
    | null results = "no cases detected"
    | null (tail results) = "1 case detected"
    | otherwise = Builder.fromText $ describe (length results) <> " cases detected"

instance ExitCodeOfReport ImplicitNoneReport where
  exitCodeOf (ImplicitNoneReport []) = 0
  exitCodeOf (ImplicitNoneReport _)  = 1
