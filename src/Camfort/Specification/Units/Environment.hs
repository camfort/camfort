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
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}


{- Provides various data types and type class instances for the Units extension -}

module Camfort.Specification.Units.Environment where

import qualified Data.Label
import Data.Label.Mono (Lens)
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Util.Position as FU

import qualified Camfort.Specification.Units.Parser as P

import Data.Char
import Data.Data
import Data.List
import Data.Matrix
import Data.Ratio

import Text.Printf

-- | Description of the unit of an expression.
data UnitInfo
  = UnitParamPosAbs (String, Int)         -- an abstract parameter identified by PU name and argument position
  | UnitParamPosUse (String, Int, Int)    -- identify particular instantiation of parameters
  | UnitParamVarAbs (String, String)      -- an abstract parameter identified by PU name and variable name
  | UnitParamVarUse (String, String, Int) -- a particular instantiation of above
  | UnitParamLitAbs Int                   -- a literal with abstract, polymorphic units, uniquely identified
  | UnitParamLitUse (Int, Int)            -- a particular instantiation of a polymorphic literal
  | UnitLiteral Int                       -- literal with undetermined but uniquely identified units
  | UnitlessLit                           -- a unitless literal
  | UnitlessVar                           -- a unitless variable
  | UnitName String                       -- a basic unit
  | UnitAlias String                      -- the name of a unit alias
  | UnitVar String                        -- variable with undetermined units (assumed to have unique name)
  | UnitMul UnitInfo UnitInfo             -- two units multiplied
  | UnitPow UnitInfo Double               -- a unit raised to a constant power
  deriving (Eq, Ord, Data, Typeable)

instance Show UnitInfo where
  show u = case u of
    UnitParamPosAbs (f, i)    -> printf "#<ParamPosAbs %s[%d]>" f i
    UnitParamPosUse (f, i, j) -> printf "#<ParamPosUse %s[%d] callId=%d>" f i j
    UnitParamVarAbs (f, v)    -> printf "#<ParamVarAbs %s.%s>" f v
    UnitParamVarUse (f, v, j) -> printf "#<ParamVarUse %s.%s callId=%d>" f v j
    UnitParamLitAbs i         -> printf "#<ParamLitAbs litId=%d>" i
    UnitParamLitUse (i, j)    -> printf "#<ParamLitUse litId=%d callId=%d]>" i j
    UnitLiteral i             -> printf "#<Literal id=%d>" i
    UnitlessLit               -> "1"
    UnitlessVar               -> "1"
    UnitName name             -> name
    UnitAlias name            -> name
    UnitVar var               -> printf "#<Var %s>" var
    UnitMul u1 (UnitPow u2 k)
      | k < 0                 -> maybeParen u1 ++ " / " ++ show (UnitPow u2 (-k))
    UnitMul u1 u2             -> maybeParenS u1 ++ " " ++ maybeParenS u2
    UnitPow u 1               -> show u
    UnitPow u 0               -> "1"
    UnitPow u k               -> printf "%s**%s" (maybeParen u) kStr
      where kStr | k < 0     = printf "(%f)" k
                 | otherwise = show k
    where
      maybeParen x | all isAlphaNum s = s
                   | otherwise        = "(" ++ s ++ ")"
        where s = show x
      maybeParenS x | all isUnitMulOk s = s
                    | otherwise         = "(" ++ s ++ ")"
        where s = show x
      isUnitMulOk c = isSpace c || isAlphaNum c || c `elem` "*."

-- | A relation between UnitInfos
data Constraint
  = ConEq   UnitInfo UnitInfo        -- an equality constraint
  | ConConj [Constraint]             -- conjunction of constraints
  deriving (Eq, Ord, Data, Typeable)
type Constraints = [Constraint]

instance Show Constraint where
  show (ConEq u1 u2) = show u1 ++ " === " ++ show u2
  show (ConConj cs) = intercalate " && " (map show cs)

--------------------------------------------------

-- The annotation on the AST used for solving units.
data UnitAnnotation a = UnitAnnotation {
   prevAnnotation :: a,
   unitSpec       :: Maybe P.UnitStatement,
   unitConstraint :: Maybe Constraint,
   unitInfo       :: Maybe UnitInfo,
   unitBlock      :: Maybe (F.Block (FA.Analysis (UnitAnnotation a))) }
  deriving (Data, Typeable, Show)

dbgUnitAnnotation (UnitAnnotation _ s c i b) =
  "{ unitSpec = " ++ show s ++ ", unitConstraint = " ++ show c ++ ", unitInfo = " ++ show i ++ ", unitBlock = " ++
     (case b of
        Nothing -> "Nothing"
        Just (F.BlStatement _ span _ (F.StDeclaration {}))  -> "Just {decl}@" ++ show span
        Just (F.BlStatement _ span _ _) -> "Just {stmt}@" ++ show span
        Just _ -> "Just ...")
   ++ "}"

mkUnitAnnotation :: a -> UnitAnnotation a
mkUnitAnnotation a = UnitAnnotation a Nothing Nothing Nothing Nothing

--------------------------------------------------

-- | Convert parser units to UnitInfo
toUnitInfo :: P.UnitOfMeasure -> UnitInfo
toUnitInfo (P.UnitProduct u1 u2) =
    UnitMul (toUnitInfo u1) (toUnitInfo u2)
toUnitInfo (P.UnitQuotient u1 u2) =
    UnitMul (toUnitInfo u1) (UnitPow (toUnitInfo u2) (-1))
toUnitInfo (P.UnitExponentiation u1 p) =
    UnitPow (toUnitInfo u1) (toDouble p)
  where
    toDouble :: P.UnitPower -> Double
    toDouble (P.UnitPowerInteger i) = fromInteger i
    toDouble (P.UnitPowerRational x y) = fromRational (x % y)
toUnitInfo (P.UnitBasic str) =
    UnitName str
toUnitInfo (P.Unitless) =
    UnitlessLit
