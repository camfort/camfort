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

{-# LANGUAGE ImplicitParams, DoAndIfThenElse, ConstraintKinds #-}

module Camfort.Specification.Units.Synthesis
                (synthesiseUnits, pprintUnitConstant) where

import Data.Function
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Ratio (numerator, denominator)
import qualified Data.Map as M
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Util.Position as FU

import qualified Camfort.Output as O (srcSpanToSrcLocs)
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.SyntaxConversion

-- *************************************
--   Insert unit declarations into code
--
-- *************************************

type A1 = FA.Analysis (UnitAnnotation A)
type Params = ?nameMap :: FAR.NameMap

-- Run this after checking/inference
synthesiseUnits :: Params => Bool -> F.ProgramFile A1 -> State UnitEnv (F.ProgramFile A1)
synthesiseUnits inferReport pf = transformBiM (perBlock inferReport) pf

perBlock :: Params => Bool -> F.Block A1 -> State UnitEnv (F.Block A1)
-- Found a declaration to which we might want to insert a comment
perBlock inferReport s@(F.BlStatement a span@(FU.SrcSpan lp up) _
                               d@(F.StDeclaration _ _ _ _ decls)) = do
    vColEnv <- gets varColEnv
    let declNames = getNames (F.aStrip decls)
    if inferReport
      -- If we are just producing an inference report
      -- Then add to report and return the original statement
      then do
        -- Find all units associated to this declaration
        units <- mapM (\d -> findUnit d vColEnv) declNames
        mapM (\u -> fromMaybe (return ()) (fmap (\u -> report <<++ mkReport u) u)) units
        return s

      else do
        -- Otherwise, replace this node with a comment node
        -- which will get output by the reprint algorithm (along
        -- with the original statement node) *IFF* we haven't
        -- already got a declaration here
        hasDec <- gets hasDeclaration
        let findUnitIfUndec d | d `elem` hasDec = Nothing
                              | otherwise       = Just $ findUnit d vColEnv
        units <- sequence $ mapMaybe findUnitIfUndec declNames
        -- Create comments for each
        let unitDecls = mapMaybe (fmap mkComment) units
        return $ (F.BlComment a' span0 (intercalate "\n" unitDecls))
    where
     -- Helper for making a report
     mkReport (var, unit) = show (spanLineCol span) ++ "\t" ++ mkInfo (var, unit)

     -- Helper for building unit specification
     mkInfo   (var, unit) = "unit (" ++ pprintUnitConstant unit  ++ ")"
                                    ++ " :: " ++ realName var
     -- Helper for building unit spec annotation comment
     mkComment (var, unit) = tabs ++ "!= " ++ mkInfo (var, unit)

     -- Calculate tab space sbased on start of declaration line
     tabs =  take (FU.posColumn lp  - 1) (repeat ' ')
     -- Create a zero-length span for the new comment node
     span0 = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})

     -- Create new annotation which labesl this as a refactored node
     ap = (prevAnnotation (FA.prevAnnotation a)) { refactored = Just loc }
     a' = a {FA.prevAnnotation = (FA.prevAnnotation a) { prevAnnotation = ap }}

     -- Start source loc
     loc  = fst $ O.srcSpanToSrcLocs span

     -- Helper for calculating the real names (not gensymed ones)
     realName v = v `fromMaybe` (v `M.lookup` ?nameMap)

     -- Lookup the unit for a variable
     findUnit v colEnv =
        case lookupWithoutSrcSpan v colEnv of
          Just (VarCol m, _) -> do u <- lookupUnit m
                                   case u of
                                     Nothing -> return Nothing
                                     Just u  -> return $ Just (v, u)
          Nothing            -> return $ Nothing

     -- All names being declared by this declaration statement
     getNames ds =
          [FA.varName e | (F.DeclVariable _ _ e@(F.ExpValue {}) _ _)
             <- universeBi ds :: [F.Declarator A1]]
       ++ [FA.varName e | (F.DeclArray _ _ e@(F.ExpValue {}) _ _ _)
             <- universeBi ds :: [F.Declarator A1]]

perBlock _ b = return b

-- Turn the internal representation into a user-readable spec
pprintUnitConstant :: UnitConstant -> String
pprintUnitConstant (UnitlessC 1) = "1"
pprintUnitConstant (UnitlessC r) = "1**(" ++ show r ++")"
pprintUnitConstant (Unitful ucs)  =
     intercalate " " (map (uncurry pprintPow) ucsPos)
  ++ (if not (null ucsNeg') then " / " else "")
  ++ intercalate " " (map (uncurry pprintPow) ucsNeg')
  where ucsNeg' = map (\(n, r) -> (n, abs r)) ucsNeg
        (ucsNeg, ucsPos) = break ((>0) . snd) ucs'
        ucs' = sortBy (compare `on` snd) ucs
        pprintPow n 1 = n
        pprintPow n r = n ++ "**" ++ show' r
        show' r =
          if denominator r == 1
          then show $ numerator r
          else '(' : (show $ numerator r) ++ '/' : (show $ denominator r) ++ ")"

lookupUnit :: Col -> State UnitEnv (Maybe UnitConstant)
lookupUnit m = do
    -- m is the column corresopnding to the variable for which
    -- we are looking up the unit
    system@(matrix, vector)  <- gets linearSystem
    ucats   <- gets unitVarCats
    badCols <- gets underdeterminedCols
    vColEnv <- gets varColEnv
    let n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
    let defaultUnit = if ucats !! (m - 1) == Argument then Nothing else Just (Unitful [])
    return $ maybe defaultUnit (lookupUnit' ucats badCols system m) n

lookupUnit' :: [UnitVarCategory] -> [Int] -> LinearSystem -> Int -> Int -> Maybe UnitConstant
lookupUnit' ucats badCols (matrix, vector) m n
  | not $ null ms = Nothing
  | ucats !! (m - 1) /= Argument && m `notElem` badCols = Just $ vector !! (n - 1)
  | ms' /= [m] = Nothing
  | otherwise = Just $ vector !! (n - 1)
  where ms = filter significant [1 .. ncols matrix]
        significant m' = m' /= m && matrix ! (n, m') /= 0 && ucats !! (m' - 1) == Argument
        ms' = filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)


{- OLD CODE
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
