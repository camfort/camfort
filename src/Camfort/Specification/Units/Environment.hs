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
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternGuards #-}


{- Provides various data types and type class instances for the Units extension -}

module Camfort.Specification.Units.Environment
  (
    -- * Datatypes and Aliases
    Constraint(..)
  , Constraints
  , UnitInfo(..)
  , isMonomorphic, isUnitless
  , VV, PP
    -- * Helpers
  , conParamEq
  , unitParamEq
  , doubleToRationalSubset
  , pprintConstr
  , pprintUnitInfo
  , toUnitInfo
  , foldUnits
  , flattenUnits
  , simplifyUnits
  , colSort, SortFn
    -- * Modules (instances)
  , module Data.Data
  ) where

import qualified Camfort.Specification.Units.Parser.Types as P
import           Control.Arrow (first, second)
import           Data.Binary
import           Data.Char
import           Data.Data
import           Data.Generics.Uniplate.Data (rewrite)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Ratio
import           GHC.Generics (Generic)
import qualified Language.Fortran.AST as F
import           Text.PrettyPrint (text)
import           Text.PrettyPrint.GenericPretty (Out, doc, docPrec)
import           Text.Printf

-- | A (unique name, source name) variable
type VV = (F.Name, F.Name)

-- | A (unique name, source name) program unit name
type PP = (F.Name, F.Name)

type UniqueId = Int

-- | Description of the unit of an expression.
data UnitInfo
  = UnitParamPosAbs (PP, VV, Int)         -- an abstract parameter identified by PU name, its name, and argument position
  | UnitParamPosUse (PP, Int, Int)        -- identify particular instantiation of parameters
  | UnitParamVarAbs (PP, VV)              -- an abstract parameter identified by PU name and variable name
  | UnitParamVarUse (PP, VV, Int)         -- a particular instantiation of above
  | UnitParamLitAbs UniqueId              -- a literal with abstract, polymorphic units, uniquely identified
  | UnitParamLitUse (UniqueId, Int)       -- a particular instantiation of a polymorphic literal
  | UnitParamEAPAbs VV                    -- an abstract Explicitly Annotated Polymorphic unit variable
  | UnitParamEAPUse (VV, Int)             -- a particular instantiation of an Explicitly Annotated Polymorphic unit variable
  | UnitParamImpAbs String                -- implicitly inferred polymorphic units, uniquely identified
  | UnitLiteral Int                       -- literal with undetermined but uniquely identified units
  | UnitlessLit                           -- a unitless literal
  | UnitlessVar                           -- a unitless variable
  | UnitName String                       -- a basic unit
  | UnitAlias String                      -- the name of a unit alias
  | UnitVar VV                            -- variable with undetermined units: (unique name, source name)
  | UnitMul UnitInfo UnitInfo             -- two units multiplied
  | UnitPow UnitInfo Double               -- a unit raised to a constant power
  | UnitRecord [(String, UnitInfo)]       -- 'record'-type of units
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | True iff u is monomorphic (has no parametric polymorphic pieces)
isMonomorphic :: UnitInfo -> Bool
isMonomorphic unitinfo = case unitinfo of
  UnitName _      -> True
  UnitAlias _     -> True
  UnitVar _       -> True
  UnitLiteral _   -> True
  UnitlessVar     -> True
  UnitlessLit     -> True
  UnitRecord recs -> all (isMonomorphic . snd) recs
  UnitMul u1 u2   -> isMonomorphic u1 && isMonomorphic u2
  UnitPow u _     -> isMonomorphic u
  _               -> False

-- | True iff argument matches one of the unitless constructors
isUnitless :: UnitInfo -> Bool
isUnitless UnitlessVar = True
isUnitless UnitlessLit = True
isUnitless _           = False

type SortFn = UnitInfo -> UnitInfo -> Ordering
colSort :: UnitInfo -> UnitInfo -> Ordering
colSort (UnitLiteral i) (UnitLiteral j)         = compare i j
colSort (UnitLiteral _) _                       = LT
colSort _ (UnitLiteral _)                       = GT
-- Don't compare on the names stored in `ParamPosAbs`
colSort (UnitParamPosAbs (x, _v, i)) (UnitParamPosAbs (y, _w, j)) = compare (x, i) (y, j)
colSort (UnitParamPosAbs _) _                   = GT
colSort _ (UnitParamPosAbs _)                   = LT
colSort x y                                     = compare x y

simplifyUnits :: UnitInfo -> UnitInfo
simplifyUnits = rewrite rw
  where
    rw (UnitMul (UnitMul u1 u2) u3)                          = Just $ UnitMul u1 (UnitMul u2 u3)
    rw (UnitMul u1 u2) | u1 == u2                            = Just $ UnitPow u1 2
    rw (UnitPow (UnitPow u1 p1) p2)                          = Just $ UnitPow u1 (p1 * p2)
    rw (UnitMul (UnitPow u1 p1) (UnitPow u2 p2)) | u1 == u2  = Just $ UnitPow u1 (p1 + p2)
    rw (UnitPow UnitlessLit _)                               = Just UnitlessLit
    rw (UnitPow UnitlessVar _)                               = Just UnitlessVar
    rw (UnitPow _ p) | p `approxEq` 0                        = Just UnitlessLit
    rw (UnitMul UnitlessLit u)                               = Just u
    rw (UnitMul u UnitlessLit)                               = Just u
    rw (UnitMul UnitlessVar u)                               = Just u
    rw (UnitMul u UnitlessVar)                               = Just u
    rw _                                                     = Nothing

flattenUnits :: UnitInfo -> [UnitInfo]
flattenUnits = map (uncurry UnitPow) . M.toList
             . M.filterWithKey (\ u _ -> u /= UnitlessLit && u /= UnitlessVar)
             . M.filter (not . approxEq 0)
             . M.fromListWith (+)
             . map (first simplifyUnits)
             . flatten
  where
    flatten (UnitMul u1 u2) = flatten u1 ++ flatten u2
    flatten (UnitPow u p)   = map (second (p*)) $ flatten u
    flatten u               = [(u, 1)]

foldUnits :: Foldable t => t UnitInfo -> UnitInfo
foldUnits units
  | null units = UnitlessVar
  | otherwise  = foldl1 UnitMul units

-- FIXME: we know better...
approxEq :: Double -> Double -> Bool
approxEq a b = abs (b - a) < epsilon
epsilon :: Double
epsilon = 0.001 -- arbitrary


instance Binary UnitInfo

instance Show UnitInfo where
  show unitinfo = case unitinfo of
    UnitParamPosAbs ((f, _), (v, _), i)         -> printf "#<ParamPosAbs %s.%s[%d]>" f v i
    UnitParamPosUse ((f, _), i, j)      -> printf "#<ParamPosUse %s[%d] callId=%d>" f i j
    UnitParamVarAbs ((f, _), (v, _))    -> printf "#<ParamVarAbs %s.%s>" f v
    UnitParamVarUse ((f, _), (v, _), j) -> printf "#<ParamVarUse %s.%s callId=%d>" f v j
    UnitParamLitAbs i                   -> printf "#<ParamLitAbs litId=%d>" i
    UnitParamLitUse (i, j)              -> printf "#<ParamLitUse litId=%d callId=%d]>" i j
    UnitParamEAPAbs (v, _)              -> v
    UnitParamEAPUse ((v, _), i)         -> printf "#<ParamEAPUse %s callId=%d]>" v i
    UnitParamImpAbs iden                -> printf "#<ParamImpAbs %s>" iden
    UnitLiteral i                       -> printf "#<Literal id=%d>" i
    UnitlessLit                         -> "1"
    UnitlessVar                         -> "1"
    UnitName name                       -> name
    UnitAlias name                      -> name -- FIXME: forbid polymorphism in aliases
    UnitVar (vName, _)                  -> printf "#<Var %s>" vName
    UnitRecord recs                     -> "record (" ++ intercalate ", " (map (\ (n, u) -> n ++ " :: " ++ show u) recs) ++ ")"
    UnitMul u1 (UnitPow u2 k)
      | k < 0                           -> maybeParen u1 ++ " / " ++ maybeParen (UnitPow u2 (-k))
    UnitMul u1 u2                       -> maybeParenS u1 ++ " " ++ maybeParenS u2
    UnitPow u 1                         -> show u
    UnitPow _ 0                         -> "1"
    UnitPow u k                         -> -- printf "%s**%f" (maybeParen u) k
      case doubleToRationalSubset k of
          Just r
            | e <- showRational r
            , e /= "1"  -> printf "%s**%s" (maybeParen u) e
            | otherwise -> show u
          Nothing -> error $
                      printf "Irrational unit exponent: %s**%f" (maybeParen u) k
       where showRational r
               | r < 0     = printf "(%s)" (showRational' r)
               | otherwise = showRational' r
             showRational' r
               | denominator r == 1 = show (numerator r)
               | otherwise = printf "(%d / %d)" (numerator r) (denominator r)
    where
      maybeParen x | all isAlphaNum s = s
                   | otherwise        = "(" ++ s ++ ")"
        where s = show x
      maybeParenS x | all isUnitMulOk s = s
                    | otherwise         = "(" ++ s ++ ")"
        where s = show x
      isUnitMulOk c = isSpace c || isAlphaNum c || c `elem` "*."

instance Out UnitInfo where
  doc = text . show
  docPrec _ = text . show

-- Converts doubles to a rational that can be expressed
-- as a rational with denominator at most 10
-- otherwise Noting
doubleToRationalSubset :: Double -> Maybe Rational
doubleToRationalSubset x | x < 0 =
    negate <$> doubleToRationalSubset (abs x)
doubleToRationalSubset x =
    doubleToRational' 0 1 (ceiling x) 1
  where
    -- The maximum common denominator, controls granularity
    n = 16
    doubleToRational' a b c d
         | b <= n && d <= n =
           let mediant = (fromIntegral (a+c))/(fromIntegral (b+d))
           in if x == mediant
              then if b + d <= n
                   then Just $ (a + c) % (b + d)
                   else Nothing
              else if x > mediant
                   then doubleToRational' (a+c) (b+d) c d
                   else doubleToRational' a b (a+c) (b+d)
         | b > n     = Just $ c % d
         | otherwise = Just $ a % b

-- | A relation between UnitInfos
data Constraint
  = ConEq   UnitInfo UnitInfo        -- an equality constraint
  | ConConj [Constraint]             -- conjunction of constraints
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Binary Constraint

type Constraints = [Constraint]

instance Show Constraint where
  show (ConEq u1 u2) = show u1 ++ " === " ++ show u2
  show (ConConj cs) = intercalate " && " (map show cs)

isUnresolvedUnit :: UnitInfo -> Bool
isUnresolvedUnit (UnitVar _)         = True
isUnresolvedUnit (UnitLiteral _)     = True
isUnresolvedUnit (UnitParamVarUse _) = True
isUnresolvedUnit (UnitParamVarAbs _) = True
isUnresolvedUnit (UnitParamPosUse _) = True
isUnresolvedUnit (UnitParamPosAbs _) = True
isUnresolvedUnit (UnitParamLitUse _) = True
isUnresolvedUnit (UnitParamLitAbs _) = True
isUnresolvedUnit (UnitParamEAPAbs _) = True
isUnresolvedUnit (UnitParamImpAbs _) = True
isUnresolvedUnit (UnitParamEAPUse _) = True
isUnresolvedUnit (UnitPow u _)       = isUnresolvedUnit u
isUnresolvedUnit (UnitMul u1 u2)     = isUnresolvedUnit u1 || isUnresolvedUnit u2
isUnresolvedUnit _                   = False

isResolvedUnit :: UnitInfo -> Bool
isResolvedUnit = not . isUnresolvedUnit

isConcreteUnit :: UnitInfo -> Bool
isConcreteUnit (UnitPow u _) = isConcreteUnit u
isConcreteUnit (UnitMul u v) = isConcreteUnit u && isConcreteUnit v
isConcreteUnit (UnitAlias _) = True
isConcreteUnit UnitlessLit = True
isConcreteUnit (UnitName _) = True
isConcreteUnit _ = False

pprintConstr :: Constraint -> String
pprintConstr (ConEq u1 u2)
  | isResolvedUnit u1 && isConcreteUnit u1 &&
    isResolvedUnit u2 && isConcreteUnit u2 =
      "Units '" ++ pprintUnitInfo u1 ++ "' and '" ++ pprintUnitInfo u2 ++
      "' should be equal"
  | isResolvedUnit u1 = "'" ++ pprintUnitInfo u2 ++ "' should have unit '" ++ pprintUnitInfo u1 ++ "'"
  | isResolvedUnit u2 = "'" ++ pprintUnitInfo u1 ++ "' should have unit '" ++ pprintUnitInfo u2 ++ "'"
  | otherwise = "'" ++ pprintUnitInfo u1 ++ "' should have the same units as '" ++ pprintUnitInfo u2 ++ "'"
pprintConstr (ConConj cs)  = intercalate "\n\t and " (fmap pprintConstr cs)

pprintUnitInfo :: UnitInfo -> String
pprintUnitInfo (UnitVar (_, sName))                 = printf "%s" sName
pprintUnitInfo (UnitParamVarUse (_, (_, sName), _)) = printf "%s" sName
pprintUnitInfo (UnitParamPosUse ((_, fname), 0, _)) = printf "result of %s" fname
pprintUnitInfo (UnitParamPosUse ((_, fname), i, _)) = printf "parameter %d to %s" i fname
pprintUnitInfo (UnitParamEAPUse ((v, _), _))        = printf "explicitly annotated polymorphic unit %s" v
pprintUnitInfo (UnitLiteral _)                      = "literal number"
pprintUnitInfo (UnitMul u1 u2)                      = pprintUnitInfo u1 ++ " * " ++ pprintUnitInfo u2
pprintUnitInfo (UnitPow u k) | k `approxEq` 0       = "1"
                             | otherwise            =
    case doubleToRationalSubset k of
          Just r
            | e <- showRational r
            , e /= "1"  -> printf "%s**%s" (maybeParen u) e
            | otherwise -> pprintUnitInfo u
          Nothing -> error $
                      printf "Irrational unit exponent: %s**%f" (maybeParen u) k
  where
    showRational r
      | r < 0     = printf "(%s)" (showRational' r)
      | otherwise = showRational' r
    showRational' r
      | denominator r == 1 = show (numerator r)
      | otherwise = printf "(%d / %d)" (numerator r) (denominator r)

    maybeParen x
      | all isAlphaNum s = s
      | otherwise        = "(" ++ s ++ ")"
      where s = pprintUnitInfo x
    -- maybeParenS x
    --   | all isUnitMulOk s = s
    --   | otherwise         = "(" ++ s ++ ")"
    --   where s = pprintUnitInfo x
    -- isUnitMulOk c = isSpace c || isAlphaNum c || c `elem` "*."

pprintUnitInfo ui                                   = show ui

--------------------------------------------------

-- | Constraint 'parametric' equality: treat all uses of a parametric
-- abstractions as equivalent to the abstraction.
conParamEq :: Constraint -> Constraint -> Bool
conParamEq (ConEq lhs1 rhs1) (ConEq lhs2 rhs2) = (unitParamEq lhs1 lhs2 || unitParamEq rhs1 rhs2) ||
                                                 (unitParamEq rhs1 lhs2 || unitParamEq lhs1 rhs2)
conParamEq (ConConj cs1) (ConConj cs2) = and $ zipWith conParamEq cs1 cs2
conParamEq _ _ = False

-- | Unit 'parametric' equality: treat all uses of a parametric
-- abstractions as equivalent to the abstraction.
unitParamEq :: UnitInfo -> UnitInfo -> Bool
unitParamEq (UnitParamLitAbs i)           (UnitParamLitUse (i', _))     = i == i'
unitParamEq (UnitParamLitUse (i', _))     (UnitParamLitAbs i)           = i == i'
unitParamEq (UnitParamVarAbs (f, i))      (UnitParamVarUse (f', i', _)) = (f, i) == (f', i')
unitParamEq (UnitParamVarUse (f', i', _)) (UnitParamVarAbs (f, i))      = (f, i) == (f', i')
unitParamEq (UnitParamPosAbs (f, _, i))   (UnitParamPosUse (f', i', _)) = (f, i) == (f', i')
unitParamEq (UnitParamPosUse (f', i', _)) (UnitParamPosAbs (f, _, i))   = (f, i) == (f', i')
unitParamEq (UnitParamImpAbs v)           (UnitParamImpAbs v')          = v == v'
unitParamEq (UnitParamEAPAbs v)           (UnitParamEAPUse (v', _))     = v == v'
unitParamEq (UnitParamEAPUse (v', _))     (UnitParamEAPAbs v)           = v == v'
unitParamEq (UnitMul u1 u2)               (UnitMul u1' u2')             = unitParamEq u1 u1' && unitParamEq u2 u2' ||
                                                                          unitParamEq u1 u2' && unitParamEq u2 u1'
unitParamEq (UnitPow u p)                 (UnitPow u' p')               = unitParamEq u u' && p == p'
unitParamEq u1 u2 = u1 == u2

-- | Convert parser units to UnitInfo
toUnitInfo   :: P.UnitOfMeasure -> UnitInfo
toUnitInfo (P.UnitProduct u1 u2)       = UnitMul (toUnitInfo u1) (toUnitInfo u2)
toUnitInfo (P.UnitQuotient u1 u2)      = UnitMul (toUnitInfo u1) (UnitPow (toUnitInfo u2) (-1))
toUnitInfo (P.UnitExponentiation u1 p) = UnitPow (toUnitInfo u1) (toDouble p)
  where
    toDouble :: P.UnitPower   -> Double
    toDouble (P.UnitPowerInteger i)    = fromInteger i
    toDouble (P.UnitPowerRational x y) = fromRational (x % y)
toUnitInfo (P.UnitBasic str)           = UnitName str
toUnitInfo (P.Unitless)                = UnitlessLit
toUnitInfo (P.UnitRecord us)           = UnitRecord (map (fmap toUnitInfo) us)
