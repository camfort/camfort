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
{-

  Units of measure extension to Fortran

TODO:
 * Deal with variable shadowing in "contained" functions.
 * Better errors with line number info

-}


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse #-}

module Camfort.Specification.Units.InferenceBackend where


import qualified Data.Vector as V
import Data.Data
import Data.Char
import Data.Maybe
import Data.Function
import Data.Matrix
import Data.List
import Data.Matrix
import Data.Ratio
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad

import qualified Language.Fortran.Util.Position as FU

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Debug
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Solve
import Camfort.Specification.Units.SyntaxConversion
import Camfort.Specification.Units.Synthesis
import Camfort.Transformation.Syntax

-- *************************************
--   Gaussian Elimination (Main)
--
-- *************************************

-- TOOD : fortran_src implement pretty print
pprint _ = ""

{-| Print debug information for non-zero coefficients from the Gaussian matrix -}
debugInfoForNonZeros :: [Rational] -> State UnitEnv String
debugInfoForNonZeros row = do debugs <- gets debugInfo
                              let cSpots = concatMap (getInfo debugs) (zip [1..] row)
                              return $ if (cSpots == []) then "" else (" arising from \n" ++ cSpots)
                                  where
                                    getInfo debugs (n, 0) = ""
                                    getInfo debugs (n, r) =
                                         case lookup n debugs of
                                                        (Just (span, s)) -> "\t" ++ (showSrcSpan span) ++ " - " ++ s ++ "\n"
                                                        _                -> ""

{- | An attempt at getting some useful user information. Needs position information -}
errorMessage :: (?debug :: Bool) => Row -> UnitConstant -> [Rational] -> State UnitEnv String
errorMessage row unit rowCoeffs =
 let ?num = 0 in
    do uvarEnv <- gets varColEnv
       debugs <- gets debugInfo
       u <- makeUnitSpec unit
       let unitStr = pprint u
       let varCols = map (+1) (findIndices (\n -> n /= 0) rowCoeffs)
       if varCols == [] then
           case unit of
             Unitful xs | length xs > 1 ->
                     do let xs' = map (\(v, r) -> (v, r * (-1))) (tail xs)
                        uR <- makeUnitSpec (Unitful $ xs')
                        uL <- makeUnitSpec (Unitful [head xs])
                        success =: False
                        conflictInfo <- debugInfoForNonZeros rowCoeffs
                        return $
                           let unitStrL = pprint uL
                               unitStrR = pprint uR
                               msg = "Conflict since " ++ unitStrL ++ " != " ++ unitStrR
                           in msg ++ conflictInfo
             {- A single unit with no variable column suggests an attempt to unify an unit
                with unitless -}
             Unitful xs | length xs == 1 ->
                          do let xs' = map (\(v, r) -> (v, r * (-1))) xs
                             uL <- makeUnitSpec (Unitful xs')
                             let unitStrL = pprint uL
                             ifDebug debugGaussian
                             conflictInfo <- debugInfoForNonZeros rowCoeffs
                             return $ "Conflict since " ++ unitStrL ++ " != 1" ++ conflictInfo
             _ -> do debugGaussian
                     return "Sorry, I can't give a better error."
       else
           let varColsAndNames = zip varCols (lookupVarsByCols uvarEnv varCols)
               exprStr' = map (\(k,v) -> if (rowCoeffs !! (k - 1)) == 1
                                         then v
                                         else (showRational (rowCoeffs !! (k - 1))) ++ "*" ++ v) varColsAndNames
               exprStr = concat $ intersperse "*" exprStr'
               msg     = "Conflict arising from " ++ exprStr ++ " of unit " ++ unitStr
           in do conflictInfo <- debugInfoForNonZeros rowCoeffs
                 return $ msg ++ conflictInfo

reportInconsistency :: (?debug :: Bool) => LinearSystem -> [Int] -> State UnitEnv ()
reportInconsistency (m, v) ns = do
  uvarEnv <- gets varColEnv
  debugs <- gets debugInfo

  -- helper functions
  let srcLineCompare = compare `on` (fst . lineCol . (\(FU.SrcSpan l _) -> l) . fst)
  let nonZeroVectorIndices = V.toList . V.map (+1) . V.findIndices (/= 0)

  -- examine all row numbers given to us as the parameter
  vs <- fmap (sortBy srcLineCompare . concat) . forM ns $ \ n -> do
      -- find out column indices of interest in the row
      let colsOfInterest = nonZeroVectorIndices (getRow n m)

      -- for each index of interest in the row, see what other rows also use it
      vs <- forM colsOfInterest $ \ i -> do
        let rowsOfInterest = nub . (i:) . nonZeroVectorIndices $ getCol i m
        -- lookup debug info for those row indices of interest
        let colDebugs = mapMaybe (flip lookup debugs) $ rowsOfInterest
        -- also lookup VarBinder info for i and convert it to same format
        let vs = map (\ (VarBinder (v, s)) -> (s, v)) $ lookupVarBindersByCols uvarEnv [i]
        return $ vs ++ colDebugs

      -- flatten it out
      return (concat vs)

  report <<++ "Caused by at least one of the following terms:"
  forM_ (nub vs) $ \ (s1@(FU.SrcSpan l _), str) -> do
    unless (all (\ x -> isNumber x || x == '.' || x == '-') str) $
      report <<++ "line " ++ show (lineCol l) ++ ": " ++ str


extendConstraints :: [UnitConstant] -> State UnitEnv ()
extendConstraints units =
        do (matrix, vector) <- gets linearSystem
           let n = nrows matrix + 1
               m = ncols matrix + 1
           linearSystem =: case units of
                             [] -> do (extendTo 0 0 m matrix, vector)
                             _ -> (setElem 1 (n, m) $ extendTo 0 n m matrix, vector ++ [last units])
           tmpColsAdded << m
           tmpRowsAdded << n
           return ()

swapUnitVarCats x n xs = swapUnitVarCats' x n xs xs 1
swapUnitVarCats' x n [] ys c = []
swapUnitVarCats' x n (z:zs) ys c | c == x = (ys !! (n - 1)) : (swapUnitVarCats' x n zs ys (c + 1))
                                 | c == n = (ys !! (x - 1)) : (swapUnitVarCats' x n zs ys (c + 1))
                                 | otherwise = z : (swapUnitVarCats' x n zs ys (c + 1))

swapCols :: Int -> Int -> State UnitEnv ()
swapCols x n = do --report <<++ ("Pre swap - " ++ (show x) ++ " <-> " ++ (show n))
                  --debugGaussian
                  varColEnv   =. updateColInfo x n
                  procedureEnv =. updateColInfo x n
                  calls        =. updateColInfo x n
                  unitVarCats  =. swapUnitVarCats x n
                  linearSystem =. (\(m, v) -> (switchCols x n m, v))
                  debugInfo    =. map (updateColInfo x n)
                  tmpColsAdded =. map (updateColInfo x n)
                  --report <<++ "Post swap"
                  --debugGaussian
                  return ()

class UpdateColInfo t where
    updateColInfo :: Col -> Col -> t -> t

instance UpdateColInfo VarCol where
    updateColInfo x n (VarCol y) | y == x = VarCol n
                                       | y == n = VarCol x
                                       | otherwise = VarCol y

instance UpdateColInfo VarColEnv where
    updateColInfo _ _ [] = []
    updateColInfo x n ((v, (uv, uvs)):ys) = (v, (updateColInfo x n uv, map (updateColInfo x n) uvs)) : (updateColInfo x n ys)

instance UpdateColInfo Procedure where
    updateColInfo x n (Nothing, ps) = (Nothing, map (updateColInfo x n) ps)
    updateColInfo x n (Just p, ps) = (Just $ updateColInfo x n p, map (updateColInfo x n) ps)

instance UpdateColInfo ProcedureEnv where
    updateColInfo x n = map (\(s, p) -> (s, updateColInfo x n p))

instance UpdateColInfo (Int, a) where
    updateColInfo x n (y, s) | y == x = (n, s)
                             | y == n = (x, s)
                             | otherwise = (y, s)

instance UpdateColInfo Int where
    updateColInfo x n y | y == x = x
                        | y == n = n
                        | otherwise = y

{-| reorderVarCols puts any variable columns to the end of the Gaussian matrix (along with the associated information) -}
reorderVarCols :: State UnitEnv ()
reorderVarCols = do ucats <- gets unitVarCats
                    (matrix, _) <- gets linearSystem
                    reorderVarCols' (ncols matrix) 1
                   where   correctEnd :: Int -> State UnitEnv Int
                           correctEnd 0   = return 0
                           correctEnd end = do ucats <- gets unitVarCats
                                               case (ucats !! (end - 1)) of
                                                  Variable -> correctEnd (end - 1)
                                                  _        -> return $ end

                           reorderVarCols' :: Int -> Int -> State UnitEnv ()
                           reorderVarCols' end c | c >= end = return ()
                           reorderVarCols' end c = do ucats <- gets unitVarCats
                                                      case (ucats !! (c - 1)) of
                                                        Variable -> do end' <- correctEnd end
                                                                       swapCols end' c
                                                                       reorderVarCols' (end' - 1) (c+1)
                                                        _        -> reorderVarCols' end (c+1)

{-| reduceRows is a core part of the polymorphic unit checking for procedures.

   It is essentially an "optimisation" of the Gaussian matrix (not in the sense
   of performance), that elimiantes rows in the system such that there are as
   few variables as possible. Within a function, assuming everything is
   consistent, then this should generate a linear constraint between the
   parameters and the return as a single row in the matrix. This is then used by
   the interprocedural constraints to hookup call-sites with definitions (in a
   parametrically polymorphic way- i.e. lambda abstraction is polymorphic in its
   units, different to say ML).
-}


reduceRows :: Col -> LinearSystem -> LinearSystem
reduceRows m (matrix, vector)
  | m > ncols matrix = (matrix, vector)
  | otherwise =
    case (find (\n -> matrix ! (n, m) /= 0) [1..nrows matrix]) of
      Just r1 ->
        case (find (\n -> matrix ! (n, m) /= 0) [(r1 + 1)..nrows matrix]) of
          Just r2 -> -- Found two rows with non-zero coeffecicients
                     -- in this column
            case (elimRow (matrix, vector) (Just r1) m r2) of
              -- Eliminate the row and cut the system down
              Ok (matrix', vector') -> reduceRows m (cutSystem r2 (matrix', vector'))
              Bad _ _ _             -> reduceRows (m+1) (matrix, vector)

          Nothing -> -- If there are no two rows with non-zero coeffecieints
                     -- in column m then move onto the next column
                     reduceRows (m+1) (matrix, vector)
      Nothing -> reduceRows (m+1) (matrix, vector)

solveSystemM :: (?solver :: Solver, ?debug :: Bool) => String -> State UnitEnv Bool
solveSystemM adjective = do
  system <- gets linearSystem
  ifDebug debugGaussian
  case (solveSystemH_Either system) of
    Right system' -> do
      linearSystem =: system'
      ifDebug (report <<++ "After solve")
      ifDebug (debugGaussian)
      return True
    Left ns       -> do
      report <<++ (adjective ++ " units of measure")
      reportInconsistency system ns
      return False
      -- linearSystem =: system'
      -- if (adjective `elem` ["inconsistent", "underdetermined"]) then
      --     do msg <- errorMessage row unit vars
      --        report <<++ msg
      --        return False
      -- else
      --     return False

checkUnderdeterminedM :: State UnitEnv ()
checkUnderdeterminedM = do ucats <- gets unitVarCats
                           system <- gets linearSystem
                           varenv  <- gets varColEnv
                           debugs  <- gets debugInfo
                           procenv <- gets procedureEnv

                           let badCols = checkUnderdetermined ucats system
                           uenv <- gets varColEnv
                           if not (null badCols) then
                               do let exprs = map (showExprLines ucats varenv procenv debugs) badCols
                                  let exprsL = concat $ intersperse "\n\t" exprs
                                  debugGaussian
                                  report <<++ "Underdetermined units of measure. Try adding units to: \n\t" ++ exprsL
                                  return ()
                           else return ()
                           underdeterminedCols =: badCols


checkUnderdetermined :: [UnitVarCategory] -> LinearSystem -> [Int]
checkUnderdetermined ucats system@(matrix, vector) =
  fixValue (propagateUnderdetermined matrix) $ checkUnderdetermined' ucats system 1

lookupVarsByColsFilterByArg :: Matrix Rational -> VarColEnv -> [UnitVarCategory] -> [Int] -> DebugInfo -> [String]
lookupVarsByColsFilterByArg matrix uenv ucats cols dbgs =
      mapMaybe (\j -> lookupEnv j uenv) cols
         where lookupEnv j [] = --Nothing
                                if (ucats !! (j - 1) == Temporary && (not (all (==0) (V.toList (getCol j matrix))))) then
                                     case (lookup j dbgs) of
                                       Just (srcSpan, info) -> Just ("[expr: " ++ (showSrcSpan srcSpan) ++ "@" ++ info ++ "]")
                                       Nothing              -> Nothing

                                else Nothing
               lookupEnv j ((VarBinder (v, _), (VarCol i, _)):uenv)
                                              | i == j    = if (j <= length ucats) then
                                                             case (ucats !! (j - 1)) of
                                                                Argument -> Nothing
                                                                _        -> if (all (==0) (V.toList (getCol j matrix)))
                                                                            then Nothing
                                                                            else Just v
                                                            else Nothing
                                              | otherwise = lookupEnv j uenv

firstNonZeroCoeff :: Matrix Rational -> [UnitVarCategory] -> Row -> Col
firstNonZeroCoeff matrix ucats row =
      case (V.findIndex (/= 0) (getRow row matrix)) of
                                  Nothing -> ncols matrix
                                  Just i  -> i + 1
{-    firstNonZeroCoeff' (V.toList $ getRow row matrix) 0
       where
                {-   -}
         firstNonZeroCoeff' []     n = n + 1
         firstNonZeroCoeff' (0:rs) n = firstNonZeroCoeff' rs (n+1)
         firstNonZeroCoeff' (r:rs) n = case (ucats !! n) of
                                         Literal -> firstNonZeroCoeff' rs (n + 1)
                                         _       -> n + 1-}



-- debug string ("n = " ++ show n ++ " vc = " ++ (show (vector !! (n - 1))) ++ " ms = " ++ show ms ++ " rest = " ++ show rest) `D.trace`
checkUnderdetermined' :: [UnitVarCategory] -> LinearSystem -> Int -> [Int]
checkUnderdetermined' ucats system@(matrix, vector) n
  | n > nrows matrix = []
  | not ((drop 1 ms) == []) && vector !! (n - 1) /= Unitful [] = ms ++ rest
  | otherwise = rest
  where ms = filter significant [2 .. ncols matrix]
        significant m = matrix ! (n, m) /= 0 && ucats !! (m - 1) `notElem` [Literal False, Literal True, Argument, Temporary]
        rest = checkUnderdetermined' ucats system (n + 1)

propagateUnderdetermined :: Matrix Rational -> [Int] -> [Int]
propagateUnderdetermined matrix list =
    nub $ do m <- list
             n <- filter (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
             filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

-- *************************************
--   Intrinsic functions: information &
--      setup functions for them.
--
-- *************************************

intrinsicsDict :: (?assumeLiterals :: AssumeLiterals) => [(String, String -> State UnitEnv ())]
intrinsicsDict =
    map (\x -> (x, addPlain1ArgIntrinsic)) ["ABS", "ACHAR", "ADJUSTL", "ADJUSTR", "AIMAG", "AINT", "ANINT", "CEILING", "CONJG", "DBLE", "EPSILON", "FLOOR","FLOAT", "FRACTION", "HUGE", "IACHAR", "ICHAR", "INT", "IPARITY", "LOGICAL", "MAXEXPONENT", "MINEXPONENT",  "NEW_LINE", "NINT", "NORM2", "NOT", "NULL", "PARITY", "REAL", "RRSPACING", "SPACING", "SUM", "TINY", "TRANSPOSE", "TRIM"]

 ++ map (\x -> (x, addPlain2ArgIntrinsic)) ["ALL", "ANY", "IALL", "IANY", "CHAR", "CMPLX", "DCOMPLX", "DIM", "HYPOT", "IAND", "IEOR", "IOR", "MAX", "MIN", "MAXVAL", "MINVAL","MODULO", "MOD"]

 ++ map (\x -> (x, addPlain1Arg1ExtraIntrinsic)) ["CSHIFT", "EOSHIFT", "IBCLR", "IBSET", "NEAREST", "PACK", "REPEAT", "RESHAPE", "SHIFTA", "SHIFTL", "SHIFTR", "SIGN"]

 ++ map (\x -> (x, addPlain2Arg1ExtraIntrinsic)) ["DSHIFTL", "DSHIFTR", "ISHFT", "ISHFTC", "MERGE", "MERGE_BITS"]

 ++ map (\x -> (x, addProductIntrinsic)) ["DOT_PRODUCT", "DPROD", "MATMUL"]

 ++ map (\x -> (x, addPowerIntrinsic)) ["SCALE", "SET_EXPONENT"]

 ++ map (\x -> (x, addUnitlessIntrinsic)) ["ACOS", "ACOSH", "ASIN", "ASINH", "ATAN", "ATANH", "BESSEL_J0", "BESSEL_J1", "BESSEL_Y0", "BESSEL_Y1", "COS", "COSH", "ERF", "ERFC", "ERFC_SCALED", "EXP", "EXPONENT", "GAMMA", "LOG", "ALOG", "LOG10", "LOG_GAMMA", "PRODUCT", "SIN", "SINH", "TAN", "TANH"]

 ++ map (\x -> (x, addUnitlessSubIntrinsic)) ["CPU_TIME", "RANDOM_NUMBER"]

 ++ map (\x -> (x, addUnitlessResult0ArgIntrinsic)) ["COMMAND_ARGUMENT_COUNT", "COMPILER_OPTIONS", "COMPILER_VERSION"]

 ++ map (\x -> (x, addUnitlessResult1ArgIntrinsic)) ["ALLOCATED", "ASSOCIATED", "BIT_SIZE", "COUNT", "DIGITS",  "IS_IOSTAT_END", "IS_IOSTAT_EOR", "KIND", "LBOUND", "LCOBOUND", "LEADZ", "LEN", "LEN_TRIM", "MASKL", "MASKR", "MAXLOC", "MINLOC", "POPCOUNT", "POPPAR", "PRECISION", "PRESENT", "RADIX", "RANGE", "SELECTED_CHAR_KIND", "SELECTED_INT_KIND", "SELECTED_REAL_KIND", "SHAPE", "SIZE", "STORAGE_SIZE", "TRAILZ", "UBOUND", "UCOBOUND"]

 ++ map (\x -> (x, addUnitlessResult2SameArgIntrinsic)) ["ATAN2", "BGE", "BGT", "BLE", "BLT", "INDEX", "LGE", "LGT", "LLE", "LLT", "SCAN", "VERIFY"]

 ++ map (\x -> (x, addUnitlessResult2AnyArgIntrinsic)) ["BTEST", "EXTENDS_TYPE_OF", "SAME_TYPE_AS"]

     -- missing: ATOMIC_DEFINE, ATOMIC_REF, BESSEL_JN, BESSEL_YN, C_*, DATE_AND_TIME, EXECUTE_COMMAND_LINE, GET_COMMAND, GET_COMMAND_ARGUMENT, GET_ENVIRONMENT_VARIABLE, IBITS, any of the image stuff, MOVE_ALLOC, MVBITS, RANDOM_SEED, SPREAD, SYSTEM_CLOCK, TRANSFER, UNPACK


{- [A] Various helpers for adding information about procedures to the type system -}

addPlain1ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPlain1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual False result arg
     procedureEnv << (name, (Just result, [arg]))

addPlain2ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPlain2ArgIntrinsic name =
  do result <- anyUnits Variable
     arg1  <- anyUnits Argument
     arg2  <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False result arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain1Arg1ExtraIntrinsic :: (?assumeLiterals :: AssumeLiterals) =>  String -> State UnitEnv ()
addPlain1Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result arg1
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain2Arg1ExtraIntrinsic :: (?assumeLiterals :: AssumeLiterals) =>  String -> State UnitEnv ()
addPlain2Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     arg3   <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False result arg2
     procedureEnv << (name, (Just result, [arg1, arg2, arg3]))

addProductIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addProductIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     temp   <- mustAddUp arg1 arg2 1 1
     mustEqual False result temp
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPowerIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPowerIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False arg2 (VarCol 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual False result (VarCol 1)
     mustEqual False arg (VarCol 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessSubIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessSubIntrinsic name =
  do arg <- anyUnits Variable
     mustEqual False arg (VarCol 1)
     procedureEnv << (name, (Nothing, [arg]))

addUnitlessResult0ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult0ArgIntrinsic name =
  do result <- anyUnits Variable
     mustEqual False result (VarCol 1)
     procedureEnv << (name, (Just result, []))

addUnitlessResult1ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg <- anyUnits Argument
     mustEqual False result (VarCol 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessResult2AnyArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult2AnyArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result (VarCol 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessResult2SameArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult2SameArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result (VarCol 1)
     mustEqual False arg1 arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))

-- *************************************
--   Unit inferences (Helpers)
--
-- *************************************

-- mustEqual - used for saying that two units must be the same- returns one of the variables
--             (choice doesn't matter, but left is chosen).
--             Returns the unit variables equaled upon
mustEqual :: (?assumeLiterals :: AssumeLiterals)
          => Bool -> VarCol -> VarCol -> State UnitEnv VarCol
mustEqual flagAsUnitlessIfLit (VarCol uv1) (VarCol uv2) =
  do n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, uv1) . incrElem 1 (n, uv2)
     ucats <- gets unitVarCats
     if flagAsUnitlessIfLit then
       case ?assumeLiterals of
         Mixed -> unitVarCats =: (map (\(n, cat) -> if ((n == uv1 || n == uv2) && ((cat == Literal True) || (cat == Literal False)))
                                                    then Literal True
                                                    else cat)  (zip [1..] ucats))
         _     -> return ()
      else return ()
     return $ VarCol uv1

-- mustAddUp - used for multipling and dividing. Creates a new 'temporary' column and returns
--             the variable associated with it
mustAddUp :: VarCol -> VarCol -> Rational -> Rational -> State UnitEnv VarCol
mustAddUp (VarCol uv1) (VarCol uv2) k1 k2 =
  do m <- addCol Temporary
     n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem k1 (n, uv1) . incrElem k2 (n, uv2)
     return $ VarCol m


sqrtUnits :: VarCol -> State UnitEnv VarCol
sqrtUnits (VarCol uv) =
  do m <- addCol Temporary
     n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem 0.5 (n, uv)
     return $ VarCol m

anyUnits :: UnitVarCategory -> State UnitEnv VarCol
anyUnits category =
  do m <- addCol category
     return $ VarCol m

-- *************************************
--    Matrix operations
--
-- *************************************


inverse :: [Int] -> [Int]
inverse perm = [j + 1 | Just j <- map (flip elemIndex perm) [1 .. length perm]]

fixValue :: Eq a => (a -> a) -> a -> a
fixValue f x = snd $ until (uncurry (==)) (\(x, y) -> (y, f y)) (x, f x)

-- The indexing for switchScaleElems and moveElem is 1-based, in line with Data.Matrix.


moveElem :: Int -> Int -> [a] -> [a]
moveElem i j []             = []
moveElem i j xs | i > j     = moveElem j i xs
                 | otherwise = moveElemA i j xs Nothing
                                where moveElemA i    j []     (Just z) = [z]
                                      moveElemA i    j []     Nothing  = []
                                      moveElemA 1    j (x:xs) (Just z) = x : moveElemA 1 (j - 1) xs (Just z)
                                      moveElemA 1    j (x:xs) Nothing  = moveElemA 1 j xs (Just x)
                                      moveElemA i    j (x:xs) Nothing  = x : moveElemA (i - 1) j xs Nothing


incrElem :: Num a => a -> (Int, Int) -> Matrix a -> Matrix a
incrElem value pos matrix = setElem (matrix ! pos + value) pos matrix

moveCol :: Int -> Int -> Matrix a -> Matrix a
moveCol i j m
    | i > j = moveCol j i m
    | otherwise = matrix (nrows m) (ncols m)
                     $ \(r, c) -> if (c < i || c > j)       then m ! (r, c)
                                  else if (c >= i && c < j) then m ! (r, c+1)
                                       else                      m ! (r, i)