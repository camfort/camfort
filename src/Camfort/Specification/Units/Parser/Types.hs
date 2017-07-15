{- |
Module      :  Camfort.Specification.Units.Parser.Types
Description :  Defines the representation of unit specifications resulting from parsing.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Specification.Units.Parser.Types
  ( UnitStatement(..)
  , UnitOfMeasure(..)
  , UnitPower(..)
  ) where

import Data.Data (Data)
import Data.List (intercalate)

data UnitStatement =
   UnitAssignment (Maybe [String]) UnitOfMeasure
 | UnitAlias String UnitOfMeasure
  deriving (Eq, Data)

instance Show UnitStatement where
  show (UnitAssignment (Just ss) uom)
    = "= unit (" ++ show uom ++ ") :: " ++ intercalate "," ss
  show (UnitAssignment Nothing uom)
    = "= unit (" ++ show uom ++ ")"
  show (UnitAlias s uom)
    = "= unit :: " ++ s ++ " = " ++ show uom

data UnitOfMeasure =
   Unitless
 | UnitBasic String
 | UnitProduct UnitOfMeasure UnitOfMeasure
 | UnitQuotient UnitOfMeasure UnitOfMeasure
 | UnitExponentiation UnitOfMeasure UnitPower
 | UnitRecord [(String, UnitOfMeasure)]
  deriving (Data, Eq)

instance Show UnitOfMeasure where
  show Unitless      = "1"
  show (UnitBasic s) = s
  show (UnitProduct uom1 uom2)
    = show uom1 ++ " " ++ show uom2
  show (UnitQuotient uom1 uom2)
    = show uom1 ++ " / " ++ show uom2
  show (UnitExponentiation uom expt)
    = show uom  ++ "** (" ++ show expt ++ ")"
  show (UnitRecord recs)
    = "record (" ++ intercalate ", "
      (map (\ (n, u) -> n ++ " :: " ++ show u) recs) ++ ")"

data UnitPower =
   UnitPowerInteger Integer
 | UnitPowerRational Integer Integer
 deriving (Data, Eq)

instance Show UnitPower where
  show (UnitPowerInteger i)      = show i
  show (UnitPowerRational i1 i2) = show i1 ++ "/" ++ show i2
