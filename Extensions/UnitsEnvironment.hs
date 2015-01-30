{-# LANGUAGE TemplateHaskell, ImplicitParams #-}

{- Provides various data types and type class instances for the Units extension -}

module Extensions.UnitsEnvironment where


import qualified Data.Label
import Data.Label.Mono (Lens)
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Language.Fortran
import Data.Matrix

type EqualityConstrained = Bool

data Solver = LAPACK | Custom deriving (Show, Read, Eq)
data AssumeLiterals = Poly | Unitless | Mixed deriving (Show, Read, Eq)

data UnitConstant = Unitful [(MeasureUnit, Rational)] | UnitlessC Rational deriving (Eq, Show)

newtype UnitVariable = UnitVariable Col deriving (Eq, Show)
data UnitVarCategory = Literal EqualityConstrained | Temporary | Variable | Argument | Magic deriving (Eq, Show)

type UnitVarEnv = [(Variable, (UnitVariable, [UnitVariable]))]

type DerivedUnitEnv = [(MeasureUnit, UnitConstant)]

type ProcedureNames = (String, Maybe Variable, [Variable])
type Procedure = (Maybe UnitVariable, [UnitVariable])
type ProcedureEnv = [(String, Procedure)]

type LinearSystem = (Matrix Rational, [UnitConstant])

type Row = Int
type Col = Int

type DebugInfo = [(Int, (SrcSpan, String))]

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
} deriving Show

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
  (UnitlessC n1) + (UnitlessC n2) = UnitlessC (n1 + n2)
  (Unitful units) * (UnitlessC n) = Unitful $ trim [(unit, r * n) | (unit, r) <- units]
  (UnitlessC n) * (Unitful units) = Unitful $ trim [(unit, n * r) | (unit, r) <- units]
  (UnitlessC n1) * (UnitlessC n2) = UnitlessC (n1 * n2)
  negate (Unitful units) = Unitful [(unit, -r) | (unit, r) <- units]
  negate (UnitlessC n) = UnitlessC (-n)
  abs (Unitful units) = Unitful [(unit, abs r) | (unit, r) <- units]
  abs (UnitlessC n) = UnitlessC $ abs n
  signum (Unitful units) = Unitful [(unit, signum r) | (unit, r) <- units]
  signum (UnitlessC n) = UnitlessC $ signum n
  fromInteger = UnitlessC . fromInteger

{- Treat 'UnitConstant's as fractionals -}
instance Fractional UnitConstant where
  (Unitful units) / (UnitlessC n) = Unitful [(unit, r / n) | (unit, r) <- units]
  (UnitlessC n1) / (UnitlessC n2) = UnitlessC (n1 / n2)
  fromRational = UnitlessC . fromRational

data Consistency a = Ok a | Bad a (UnitConstant, [Rational]) | BadL a deriving Show

efmap :: (a -> a) -> Consistency a -> Consistency a
efmap f (Ok x)      = Ok (f x)
efmap f (Bad x msg) = Bad x msg

ifDebug :: (?debug :: Bool, Monad m) => m a -> m ()
ifDebug e = if ?debug then e >> return () else return ()