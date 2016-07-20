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

{-# LANGUAGE PatternGuards, ScopedTypeVariables, ImplicitParams, DoAndIfThenElse, ConstraintKinds #-}

module Camfort.Specification.Units.Synthesis
  (runSynthesis, synthesiseUnits, pprintUnitConstant)
where

import Data.Function
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Ratio (numerator, denominator)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Util.Position as FU

import qualified Camfort.Specification.Units.Parser as P
import Camfort.Analysis.CommentAnnotator
import qualified Camfort.Output as O (srcSpanToSrcLocs)
import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.Monad
import qualified Debug.Trace as D

-- | Insert unit declarations into the ProgramFile as comments.
runSynthesis :: [(String, UnitInfo)] -> UnitSolver [(String, UnitInfo)]
runSynthesis vars = do
  modifyProgramFileM $ descendBiM (synthBlocks vars)   -- descendBiM finds the head of lists
  return vars

-- Should be invoked on the beginning of a list of blocks
synthBlocks :: [(String, UnitInfo)] -> [F.Block UA] -> UnitSolver [F.Block UA]
synthBlocks vars = fmap reverse . foldM (synthBlock vars) []

-- Process an individual block while building up a list of blocks (in
-- reverse order) to ultimately replace the original list of
-- blocks. We're looking for blocks containing declarations, in
-- particular, in order to possibly insert a unit annotation before
-- them.
synthBlock :: [(String, UnitInfo)] -> [F.Block UA] -> F.Block UA -> UnitSolver [F.Block UA]
synthBlock vars bs b@(F.BlStatement a ss@(FU.SrcSpan lp up) _ (F.StDeclaration _ _ _ _ decls)) = do
  pf    <- usProgramFile `fmap` get
  nMap  <- uoNameMap `fmap` ask
  gvSet <- usGivenVarSet `fmap` get
  newBs <- fmap catMaybes . forM (universeBi decls) $ \ e -> case e of
    e@(F.ExpValue _ _ (F.ValVariable _))
      | name `S.notMember` gvSet            -- not a member of the already-given variables
      , Just u <- lookup name vars -> do    -- and a unit has been inferred
        -- Pick the start source loc from the existing decl.
        let loc   = fst $ O.srcSpanToSrcLocs ss
        -- Create new annotation which labels this as a refactored node.
        let newA  = a { FA.prevAnnotation = (FA.prevAnnotation a) {
                           prevAnnotation = (prevAnnotation (FA.prevAnnotation a)) {
                               refactored = Just loc } } }
        -- Create a zero-length span for the new comment node.
        let newSS = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
        -- Build the text of the comment with the unit annotation.
        let txt   = "= " ++ showUnitDecl nMap (e, u)
        let space = FU.posColumn lp - 1
        let newB  = F.BlComment newA newSS . insertSpacing space $ commentText pf txt
        return $ Just newB
      where
        name = FA.varName e
    (e :: F.Expression UA) -> return Nothing
  return (b:reverse newBs ++ bs)
synthBlock _ bs b = return (b:bs)

-- Insert the correct comment markers around the given text string, depending on Fortran version.
-- FIXME: use Fortran meta information when I have finished adding it to ProgramFile.
commentText :: F.ProgramFile UA -> String -> String
commentText _ text = "!" ++ text

-- Insert a given amount of spacing before the string.
insertSpacing :: Int -> String -> String
insertSpacing n = (replicate n ' ' ++)

-- Pretty print a unit declaration.
showUnitDecl nameMap (e, u) = "unit " ++ show u ++ " :: " ++ (v `fromMaybe` M.lookup v nameMap)
  where v = FA.varName e

--------------------------------------------------
--------------------------------------------------

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
        -- count
        (n, ad) <- gets evUnitsAdded
        evUnitsAdded =: (n + (length units), ad)
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

     -- Create new annotation which labels this as a refactored node
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
     numeratorU
  ++ (if not (null ucsNeg') then " / " else "")
  ++ denominatorU
  where numeratorU = if (null ucsPos) then "1" else numeratorA
        numeratorA = intercalate " " (map (uncurry pprintPow) ucsPos)
        denominatorU = intercalate " " (map (uncurry pprintPow) ucsNeg')
        ucsNeg' = map (\(n, r) -> (n, abs r)) ucsNeg
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
