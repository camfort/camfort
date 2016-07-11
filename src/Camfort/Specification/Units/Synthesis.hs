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

{-# LANGUAGE ImplicitParams, DoAndIfThenElse #-}

module Camfort.Specification.Units.Synthesis where

import Data.Data
import Data.Char
import Data.Function
import Data.Matrix
import Data.List
import Data.Matrix
import Data.Ratio
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad

import Language.Fortran
import Language.Fortran.Pretty

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.SyntaxConversion
import Camfort.Transformation.Syntax

-- *************************************
--   Insert unit declarations into code
--
-- *************************************

insertUnit :: (?num :: Int) => [UnitVarCategory] -> [Int] -> LinearSystem -> Type Annotation -> Int -> State UnitEnv (Type Annotation)
insertUnit ucats badCols system (BaseType aa tt attrs kind len) uv = undefined

makeUnitSpec = undefined

insertUnitsInBlock :: Block Annotation -> State UnitEnv (Block Annotation)
insertUnitsInBlock x = undefined -- transformBiM insertUnits x

{-
  do let unit = lookupUnit ucats badCols system uv
     u <- (insertUnitAttribute unit attrs)
     return $ BaseType aa tt u kind len

insertUnit ucats badCols system (ArrayT dims aa tt attrs kind len) uv =
  do let unit = lookupUnit ucats badCols system uv
     u <- insertUnitAttribute unit attrs
     return $ ArrayT dims aa tt u kind len




lookupUnit :: [UnitVarCategory] -> [Int] -> LinearSystem -> Col -> Maybe UnitConstant
lookupUnit ucats badCols system@(matrix, vector) m =
  let -- m is the column corresopnding to the variable for which we are looking up the unit
      n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
      defaultUnit = if ucats !! (m - 1) == Argument then Nothing else Just (Unitful [])
  in maybe defaultUnit (lookupUnit' ucats badCols system m) n


lookupUnit' :: [UnitVarCategory] -> [Int] -> LinearSystem -> Int -> Int -> Maybe UnitConstant
lookupUnit' ucats badCols (matrix, vector) m n
  | not $ null ms = Nothing
  | ucats !! (m - 1) /= Argument && m `notElem` badCols = Just $ vector !! (n - 1)
  | ms' /= [m] = Nothing
  | otherwise = Just $ vector !! (n - 1)
  where ms = filter significant [1 .. ncols matrix]
        significant m' = m' /= m && matrix ! (n, m') /= 0 && ucats !! (m' - 1) == Argument
        ms' = filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

insertUnits :: Decl Annotation -> State UnitEnv (Decl Annotation)
insertUnits decl@(Decl a sp@(s1, s2) d t) | not (pRefactored a || hasUnits t) =
  do system  <- gets linearSystem
     ucats   <- gets unitVarCats
     badCols <- gets underdeterminedCols
     vColEnv <- gets varColEnv
     let varCol (Var _ s ((VarName _ v, _):_), _, _) =  case (lookupWithSrcSpan v s (vColEnv)) of
                                                           (Just (VarCol m,_)) ->  m
                                                           Nothing -> error $ "No variable " ++ (show v)
     let sameUnits = (==) `on` (lookupUnit ucats badCols system . varCol)
     let groups = groupBy sameUnits d
     types <- mapM (\g -> let ?num = length g in insertUnit ucats badCols system t . varCol . head $ g) groups
     let   a' = a { refactored = Just s1 }
     let   sp' = dropLine $ refactorSpan sp
     let   sp'' = (toCol0 s1, snd $ dropLine sp)
     let   decls = [Decl a' sp' group t' | (group, t') <- zip groups types]
     if (not (types == [t])) then
         return $ DSeq a (NullDecl a' sp'') (foldr1 (DSeq a) decls)
     else
         return $ decl

insertUnits decl = return decl


insertUnitAttribute :: (?num :: Int) => Maybe UnitConstant -> [Attr Annotation] -> State UnitEnv [Attr Annotation]
insertUnitAttribute (Just unit) attrs = do spec <- makeUnitSpec unit
                                           return $ attrs ++ [MeasureUnit unitAnnotation $ spec]
insertUnitAttribute Nothing attrs = return attrs



hasUnits :: Type a -> Bool
hasUnits (BaseType _ _ attrs _ _) = any isUnit attrs
hasUnits (ArrayT _ _ _ attrs _ _) = any isUnit attrs

isUnit :: Attr a -> Bool
isUnit (MeasureUnit _ _) = True
isUnit _ = False

-- Used for evaluation
updateAdded k s = do (n, xs) <- gets evUnitsAdded
                     let k' = if k == 0 then 1 else k
                     evUnitsAdded =: (n + k, xs ++ [s])


makeUnitSpec :: (?num :: Int) => UnitConstant -> State UnitEnv (MeasureUnitSpec Annotation)
makeUnitSpec (UnitlessC r) =
    do let u = UnitProduct unitAnnotation [("1", (FractionConst unitAnnotation (show $ numerator r) (show $ denominator r)))] --hm!
       updateAdded ?num (pprint u)
       return $ u

makeUnitSpec (Unitful []) = return $ UnitNone unitAnnotation
makeUnitSpec (Unitful units)
  | null neg = let u = UnitProduct unitAnnotation $ formatUnits pos
               in do updateAdded ?num (pprint u)
                     return u
  | otherwise = let u = UnitQuotient unitAnnotation (formatUnits pos) (formatUnits neg)
                in do updateAdded ?num (pprint u)
                      return u
  where pos = filter (\(unit, r) -> r > 0) units
        neg = [(unit, -r) | (unit, r) <- units, r < 0]

formatUnits :: [(MeasureUnit, Rational)] -> [(MeasureUnit, Fraction Annotation)]
formatUnits units = [(unit, toFraction r) | (unit, r) <- units]

toFraction :: Rational -> Fraction Annotation
toFraction 1 = NullFraction unitAnnotation
toFraction r
  | q == 1 = IntegerConst unitAnnotation $ show p
  | otherwise = FractionConst unitAnnotation (show p) (show q)
  where p = numerator r
        q = denominator r
-}