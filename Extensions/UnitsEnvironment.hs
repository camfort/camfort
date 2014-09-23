{-# LANGUAGE TemplateHaskell #-}

{- Provides various data types and type class instances for the Units extension -}

module Extensions.UnitsEnvironment where


import qualified Data.Label
import Data.Label.Mono (Lens)
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Language.Fortran
import Data.Matrix

data UnitConstant = Unitful [(MeasureUnit, Rational)] | Unitless Rational deriving (Eq, Show)

newtype UnitVariable = UnitVariable Int deriving (Eq, Show)
data UnitVarCategory = Literal | Temporary | Variable | Argument | Magic deriving (Eq, Show)

type UnitVarEnv = [(Variable, (UnitVariable, [UnitVariable]))]

type DerivedUnitEnv = [(MeasureUnit, UnitConstant)]

type ProcedureNames = (String, Maybe Variable, [Variable])
type Procedure = (Maybe UnitVariable, [UnitVariable])
type ProcedureEnv = [(String, Procedure)]

type LinearSystem = (Matrix Rational, [UnitConstant])

type Row = Int
type Col = Int

type DebugInfo = [(Int, String)]

data UnitEnv = UnitEnv {
  _report              :: [String],
  _unitVarEnv          :: UnitVarEnv,
  _derivedUnitEnv      :: DerivedUnitEnv,
  _procedureEnv        :: ProcedureEnv,
  _calls               :: ProcedureEnv,
  _unitVarCats         :: [UnitVarCategory],
  _reorderedCols       :: [Int],
  _underdeterminedCols :: [Int],
  _linearSystem        :: LinearSystem, 
  _debugInfo           :: DebugInfo,
  _tmpRowsAdded        :: [Int],
  _tmpColsAdded        :: [Int]
}

Data.Label.mkLabels [''UnitEnv]

resetTemps :: State UnitEnv ()
resetTemps = do tmpRowsAdded =: []
                tmpColsAdded =: []

trim = filter $ \(unit, r) -> r /= 0

{- Treat 'UnitConstant's as numbers -}
instance Num UnitConstant where
  (Unitful u1) + (Unitful u2) = Unitful $ trim $ merge u1 u2
    where merge [] u2 = u2
          merge u1 [] = u1
          merge ((unit1, r1) : u1) ((unit2, r2) : u2)
            | unit1 == unit2 = (unit1, r1 + r2) : merge u1 u2
            | unit1 <  unit2 = (unit1, r1) : merge u1 ((unit2, r2) : u2)
            | otherwise      = (unit2, r2) : merge ((unit1, r1) : u1) u2
  (Unitless n1) + (Unitless n2) = Unitless (n1 + n2)
  (Unitful units) * (Unitless n) = Unitful $ trim [(unit, r * n) | (unit, r) <- units]
  (Unitless n) * (Unitful units) = Unitful $ trim [(unit, n * r) | (unit, r) <- units]
  (Unitless n1) * (Unitless n2) = Unitless (n1 * n2)
  negate (Unitful units) = Unitful [(unit, -r) | (unit, r) <- units]
  negate (Unitless n) = Unitless (-n)
  abs (Unitful units) = Unitful [(unit, abs r) | (unit, r) <- units]
  abs (Unitless n) = Unitless $ abs n
  signum (Unitful units) = Unitful [(unit, signum r) | (unit, r) <- units]
  signum (Unitless n) = Unitless $ signum n
  fromInteger = Unitless . fromInteger

{- Treat 'UnitConstant's as fractionals -}
instance Fractional UnitConstant where
  (Unitful units) / (Unitless n) = Unitful [(unit, r / n) | (unit, r) <- units]
  (Unitless n1) / (Unitless n2) = Unitless (n1 / n2)
  fromRational = Unitless . fromRational