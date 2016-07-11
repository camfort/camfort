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

module Camfort.Specification.Units.Debug where


import qualified Data.Vector as V
import qualified Debug.Trace as D
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

import Language.Fortran
import Language.Fortran.Pretty

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.SyntaxConversion
import Camfort.Transformation.Syntax

-- *************************************
--   Debugging and testing functions
--
-- *************************************


-- QuickCheck instance for matrices, used for testing matrix operations
{-
instance (Arbitrary a) => Arbitrary (Matrix a) where
    arbitrary = sized (\n -> do xs <- vectorOf (n*n) arbitrary
                                return $ matrix n n (\(i, j) -> xs !! ((i-1)*n + (j-1))))
-}

-- Matrix for development
fooMatrix :: Matrix Rational
fooMatrix = matrix 4 4 $ (\(i,j) -> if (i==j) then (toInteger i) % 1 else 0)


{-| debugGaussian - a debugging routine which shose the Gaussian matrix with various peieces of info
                    mainly used for development purposes -}
debugGaussian :: State UnitEnv String
debugGaussian = do grid' <- debugGaussian'
                   report <<++ ("Dump of units-of-measure system matrix\n" ++ grid')
                   return grid'

debugGaussian' = do ucats   <- gets unitVarCats
                    (matrix,rowv)  <- gets linearSystem
                    varenv  <- gets varColEnv
                    debugs  <- gets debugInfo
                    procenv <- gets procedureEnv

                    let -- Column headings and then a space
                        grid = ["" : map show [1..(ncols matrix)], []]
                        -- Gaussian matrix
                            ++ map (\r -> (show r) : (map showRational $ V.toList $ getRow r matrix) ++ [show $ rowv !! (r - 1)]) [1..(nrows matrix)]
                        -- Column categories
                            ++ [[], "" : map showCat ucats]
                        -- Debug info, e.g., expression or variable
                            ++ ["" : map (showExpr ucats varenv procenv debugs) [1.. (ncols matrix)]]
                        -- Additional debug info for args that are also variables
                            ++ ["" : map (showArgVars ucats varenv) [1..(ncols matrix)]]
                    let colSize = maximum' (map maximum' (map (notLast . (map length)) grid))
                    let expand r = r ++ (replicate (colSize - length r) ' ')
                    let showLine x = (concatMap expand x) ++ "\n"

                    let grid' = concatMap showLine grid
                    return grid'

   where maximum' [] = 0
         maximum' xs = maximum xs

         notLast xs = take (length xs - 1) xs

showExpr cats vars procs debugInfo c =
             case (cats !! (c - 1)) of
               Variable  -> case (lookupVarsByCols vars [c]) of
                              []    -> case (lookupProcByCols procs [c]) of
                                         []    -> "?"
                                         (x:_) -> "=" ++ x
                              (x:_) -> x
               Temporary -> snd $ case (lookup c debugInfo) of
                                    Just x -> x
                                    Nothing -> (undefined, "") -- error $ "Temporary fail " ++ (show c) " not in " ++ (show cats)
               Argument  -> case (lookupProcByArgCol procs [c]) of
                              []    -> "?"
                              (x:_) -> x
               Literal _  -> snd $ case (lookup c debugInfo) of
                                    Just x -> x
                                    Nothing -> show c `D.trace` error "Literal fail"
               Magic     -> ""

showSrcLoc loc = show (srcLine loc) ++ ":" ++ show (srcColumn loc)
showSrcSpan (start, end) = "(" ++ showSrcLoc start ++ " - " ++ showSrcLoc end ++ ")"

showSrcFile (start, _) = srcFilename start

showExprLines cats vars procs debugInfo c =
             case (cats !! (c - 1)) of
               Variable  -> case (lookup c debugInfo) of
                              Just (sp, expr) -> (showSrcSpan sp) ++ "\t" ++ expr
                              Nothing ->
                                case (lookupVarsByCols vars [c]) of
                                  []    -> case (lookupProcByCols procs [c]) of
                                             []    -> "?"
                                             (x:_) -> "=" ++ x
                                  (x:_) -> x
               Temporary -> let (sp, expr) = fromJust $ lookup c debugInfo
                            in (showSrcSpan sp) ++ "\t" ++ expr
               Argument  -> case (lookupProcByArgCol procs [c]) of
                              []    -> "?"
                              (x:_) -> x
               Literal _ -> let (sp, expr) = fromJust $ lookup c debugInfo
                            in (showSrcSpan sp) ++ "\t" ++ expr
               Magic     -> ""

showArgVars cats vars c =
             case (cats !! (c - 1)) of
               Argument -> case (lookupVarsByCols vars [c]) of
                             []    -> ""
                             (x:_) -> x
               _        -> ""


showCat Variable  = "Var"
showCat Magic     = "Magic"
showCat Temporary = "Temp"
showCat Argument  = "Arg"
showCat (Literal False) = "Lit"
showCat (Literal True)  = "Lit="

lookupProcByArgCol :: ProcedureEnv -> [Int] -> [String]
lookupProcByArgCol penv cols =
             mapMaybe (\j -> lookupEnv j penv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((p, (_, args)):penv)
                           | elem (VarCol j) args  = Just (p ++ "#" ++ (show $ fromJust $ elemIndex (VarCol j) args))
                           | otherwise    = lookupEnv j penv


lookupProcByCols :: ProcedureEnv -> [Int] -> [String]
lookupProcByCols penv cols =
             mapMaybe (\j -> lookupEnv j penv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((p, (Just (VarCol i), _)):penv)
                                    | i == j    = Just p
                                    | otherwise = lookupEnv j penv
                       lookupEnv j ((p, (Nothing, _)):penv) = lookupEnv j penv

lookupVarsByCols :: VarColEnv -> [Int] -> [Variable]
lookupVarsByCols uenv cols = mapMaybe (\j -> lookupEnv j uenv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((VarBinder (v, _), (VarCol i, _)):uenv)
                                    | i == j    = Just v
                                    | otherwise = lookupEnv j uenv

lookupVarBindersByCols :: VarColEnv -> [Int] -> [VarBinder]
lookupVarBindersByCols uenv cols = mapMaybe (\j -> lookupEnv j uenv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((vb@(VarBinder (v, _)), (VarCol i, _)):uenv)
                                    | i == j    = Just vb
                                    | otherwise = lookupEnv j uenv

showRational r = show (numerator r) ++ if ((denominator r) == 1) then "" else "%" ++ (show $ denominator r)
