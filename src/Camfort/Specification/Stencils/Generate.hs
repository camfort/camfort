{- |
Module      :  Camfort.Specification.Stencils.Generate
Description :  Generate stencils for inference and synthesis
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Camfort.Specification.Stencils.Generate
  (
    EvalLog
  , Neighbour(..)
  , assocsSequence
  , genSpecifications
  , genSubscripts
  , isArraySubscript
  , neighbourIndex
  , isVariableExpr
  , ixToNeighbour'
  , indicesToRelativisedOffsets
  , indicesToSpec
  ) where

import Control.Monad (void, when, zipWithM)
import Control.Monad.State.Strict (get, put, runState, State)
import Control.Monad.Writer.Strict (tell, Writer)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Operations (transformBi, universeBi)
import Data.Graph.Inductive.Graph (lab, pre)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as S

import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Util.Position as FU

import Camfort.Analysis.Annotations (A, Annotation)
import Camfort.Helpers (collect)
import qualified Camfort.Helpers.Vec as V
import Camfort.Specification.Stencils.Model
  (Approximation(..), Multiplicity(..))
import Camfort.Specification.Stencils.Annotation ()
import Camfort.Specification.Stencils.Syntax
  ( absoluteRep
  , fromBool
  , groupKeyBy
  , hasDuplicates
  , isEmpty
  , isUnit
  , setLinearity
  , Specification(..)
  , Variable)

import Camfort.Specification.Stencils.CheckBackend
import Camfort.Specification.Stencils.InferenceBackend
type EvalLog = [(String, Variable)]

-- Representation for indices as either:
--   * neighbour indices
--   * constant
--   * non neighbour index
data Neighbour = Neighbour Variable Int
               | Constant (F.Value ())
               | NonNeighbour deriving (Eq, Show)

-- Match expressions which are array subscripts, returning Just of their
-- index expressions, else Nothing
isArraySubscript :: F.Expression (FA.Analysis A)
                 -> Maybe [F.Index (FA.Analysis A)]
isArraySubscript (F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable _)) subs) =
   Just $ F.aStrip subs
isArraySubscript (F.ExpDataRef _ _ e e') =
   isArraySubscript e <> isArraySubscript e'
isArraySubscript _ = Nothing

-- Given a list of induction variables and a list of indices
-- map them to a list of constant or neighbourhood indices
-- if any are non neighbourhood then return Nothi ng
neighbourIndex :: FAD.InductionVarMapByASTBlock
               -> [F.Index (FA.Analysis Annotation)] -> Maybe [Neighbour]
neighbourIndex ivs ixs =
  if NonNeighbour `notElem` neighbours
  then Just neighbours
  else Nothing
    where neighbours = map (ixToNeighbour ivs) ixs

genSpecifications ::
     FAD.FlowsGraph A
  -> FAD.InductionVarMapByASTBlock
  -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Writer EvalLog ([([Variable], Specification)], [Int])
genSpecifications flowsGraph ivs lhs blocks = do
    let (subscripts, visitedNodes) = genSubscripts flowsGraph blocks
    varToSpecs <- assocsSequence $ mkSpecs subscripts
    case varToSpecs of
      [] -> do
         tell [("EVALMODE: Empty specification (tag: emptySpec)", "")]
         return ([], visitedNodes)
      _ -> do
         let varsToSpecs = groupKeyBy varToSpecs
         return (splitUpperAndLower varsToSpecs, visitedNodes)
    where
      mkSpecs = M.mapWithKey (\v -> indicesToSpec ivs v lhs)

      splitUpperAndLower = concatMap splitUpperAndLower'
      splitUpperAndLower' (vs, Specification (Mult (Bound (Just l) (Just u))) isStencil)
        | isUnit l =
         [(vs, Specification (Mult (Bound Nothing (Just u))) isStencil)]
        | otherwise =
         [(vs, Specification (Mult (Bound (Just l) Nothing)) isStencil),
          (vs, Specification (Mult (Bound Nothing (Just u))) isStencil)]
      splitUpperAndLower' (vs, Specification (Once (Bound (Just l) (Just u))) isStencil)
        | isUnit l =
         [(vs, Specification (Mult (Bound Nothing (Just u))) isStencil)]
        | otherwise =
         [(vs, Specification (Once (Bound (Just l) Nothing)) isStencil),
          (vs, Specification (Once (Bound Nothing (Just u))) isStencil)]
      splitUpperAndLower' x = [x]

{-| genSubscripts
   Takes * a name map
         * a list of blocks representing an RHS
   Returns a map from array variables to indices, and a list of
   nodes that were visited when computing this information -}
genSubscripts ::
     FAD.FlowsGraph A
  -> [F.Block (FA.Analysis A)]
  -> (M.Map Variable [Indices], [Int])
genSubscripts flowsGraph blocks =
    (subscripts, visitedNodes)
  where
    (maps, visitedNodes) = runState (mapM (genSubscripts' True flowsGraph) blocks) []
    subscripts = M.unionsWith (++) maps

    -- Generate all subscripting expressions (that are translations on
    -- induction variables) that flow to this block
    -- The State monad provides a list of the visited nodes so far
    genSubscripts' ::
        Bool
     -> FAD.FlowsGraph A
     -> F.Block (FA.Analysis A)
     -> State [Int] (M.Map Variable [Indices])

    genSubscripts' False _ (F.BlStatement _ _ _ (F.StExpressionAssign _ _ e _))
       | isJust $ isArraySubscript e
       -- Don't pull dependencies through arrays
       = return M.empty

    genSubscripts' _ flowsGraph block = do
       visited <- get
       case FA.insLabel $ F.getAnnotation block of

         Just node
           | node `elem` visited ->
            -- This dependency has already been visited during this traversal
              pure M.empty
           | otherwise -> do
            -- Fresh dependency
            put $ node : visited
            let blocksFlowingIn = mapMaybe (lab flowsGraph) $ pre flowsGraph node
            dependencies <- mapM (genSubscripts' False flowsGraph) blocksFlowingIn
            return $ M.unionsWith (++) (genRHSsubscripts block : dependencies)

         Nothing -> error $ "Missing a label for: " ++ show block

type Indices = [F.Index (FA.Analysis A)]

-- Given a list of induction variables and an index, compute
-- its Neighbour representation
-- e.g., for the expression a(i+1,j-1) then this function gets
-- passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1)
ixToNeighbour :: FAD.InductionVarMapByASTBlock
              -> F.Index (FA.Analysis Annotation) -> Neighbour
-- Range with stride = 1 and no explicit bounds count as reflexive indexing
ixToNeighbour ivmap f = ixToNeighbour' ivsList f
  where
    insl = FA.insLabel . F.getAnnotation $ f
    errorMsg = show (ixsspan f)
            ++ " get IVs associated to labelled index "
            ++ show insl
    insl' = fromJustMsg errorMsg insl
    ivsList = S.toList $ fromMaybe S.empty $ IM.lookup insl'  ivmap
    -- For debugging purposes
    ixsspan :: F.Index (FA.Analysis A)  -> FU.SrcSpan
    ixsspan  (F.IxRange _ sp _ _ _) = sp
    ixsspan (F.IxSingle _ sp _ _ ) = sp

fromJustMsg _ (Just x) = x
fromJustMsg msg Nothing = error msg

ixToNeighbour' _ (F.IxRange _ _ Nothing Nothing Nothing)     = Neighbour "" 0
ixToNeighbour' _ (F.IxRange _ _ Nothing Nothing
                  (Just (F.ExpValue _ _ (F.ValInteger "1")))) = Neighbour "" 0

ixToNeighbour' ivs (F.IxSingle _ _ _ exp)  = expToNeighbour ivs exp
ixToNeighbour' _ _ = NonNeighbour -- indexing expression is a range

-- Combiantor for reducing a map with effects and partiality inside
-- into an effectful list of key-value pairs
assocsSequence :: Monad m => M.Map k (m (Maybe a)) -> m [(k, a)]
assocsSequence maps = do
    assocs <- mapM strength . M.toList $ maps
    return . mapMaybe strength $ assocs
  where
    strength :: Monad m => (a, m b) -> m (a, b)
    strength (a, mb) = mb >>= (\b -> return (a, b))

-- Convert list of indexing expressions to a spec
indicesToSpec :: FAD.InductionVarMapByASTBlock
              -> Variable
              -> [Neighbour]
              -> [[F.Index (FA.Analysis Annotation)]]
              -> Writer EvalLog (Maybe Specification)
indicesToSpec ivs a lhs ixs = do
    mMultOffsets <- indicesToRelativisedOffsets ivs a lhs ixs
    return $ do
      (mult, offsets) <- mMultOffsets
      spec <- relativeIxsToSpec offsets
      let spec' = setLinearity (fromBool mult) spec
      return $ setType lhs spec'

-- Get all RHS subscript which are translated induction variables
-- return as a map from (source name) variables to a list of relative indices
genRHSsubscripts ::
     F.Block (FA.Analysis A)
  -> M.Map Variable [Indices]
genRHSsubscripts b = genRHSsubscripts' (transformBi replaceModulo b)
  where
    -- Any occurence of an subscript "modulo(e, e')" is replaced with "e"
    replaceModulo :: F.Expression (FA.Analysis A) -> F.Expression (FA.Analysis A)
    replaceModulo (F.ExpFunctionCall _ _
                      (F.ExpValue _ _ (F.ValIntrinsic iname)) subs)
        | iname `elem` ["modulo", "mod", "amod", "dmod"]
        -- We expect that the first parameter to modulo is being treated
        -- as an IxSingle element
        , Just (F.Argument _ _ _ e':_) <- fmap F.aStrip subs = e'
    replaceModulo e = e

    genRHSsubscripts' b =
       collect [ (FA.srcName exp, e)
         | F.ExpSubscript _ _ exp subs <- FA.rhsExprs b
         , isVariableExpr exp
         , let e = F.aStrip subs
         , not (null e)]

-- Given a list of induction variables and an expression, compute its
-- Neighbour representation
expToNeighbour :: forall a. Data a
            => [Variable] -> F.Expression (FA.Analysis a) -> Neighbour

expToNeighbour ivs e@(F.ExpValue _ _ v@(F.ValVariable _))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) 0
    | otherwise               = Constant (void v)

expToNeighbour _ (F.ExpValue _ _ val) = Constant (void val)

expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                 e@(F.ExpValue _ _ (F.ValVariable _))
                   (F.ExpValue _ _ (F.ValInteger offs)))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) (read offs)

expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                  (F.ExpValue _ _ (F.ValInteger offs))
                e@(F.ExpValue _ _ (F.ValVariable _)))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) (read offs)

expToNeighbour ivs (F.ExpBinary _ _ F.Subtraction
                 e@(F.ExpValue _ _ (F.ValVariable _))
                   (F.ExpValue _ _ (F.ValInteger offs)))
   | FA.varName e `elem` ivs =
         Neighbour (FA.varName e) (if x < 0 then abs x else (- x))
             where x = read offs

expToNeighbour ivs e =
  -- Record when there is some kind of relative index on an inducion variable
  -- but that is not a neighbourhood index by our definitions
  if null ivs' then Constant (F.ValInteger "0") else NonNeighbour
  where
    -- set of all induction variables involved in this expression
    ivs' = [i | e@(F.ExpValue _ _ F.ValVariable{})
                 <- universeBi e :: [F.Expression (FA.Analysis a)]
                , let i = FA.varName e
                , i `elem` ivs]

indicesToRelativisedOffsets :: FAD.InductionVarMapByASTBlock
                            -> Variable
                            -> [Neighbour]
                            -> [[F.Index (FA.Analysis Annotation)]]
                            -> Writer EvalLog (Maybe (Bool, [[Int]]))
indicesToRelativisedOffsets ivs a lhs ixs = do
   -- Convert indices to neighbourhood representation
  let rhses = map (map (ixToNeighbour ivs)) ixs

  -- As an optimisation, do duplicate check in front-end first
  -- so that duplicate indices don't get passed into the main engine
  let (rhses', mult) = hasDuplicates rhses

  -- Check that induction variables are used consistently on lhs and rhses
  if not (consistentIVSuse lhs rhses')
    then do tell [("EVALMODE: Inconsistent IV use (tag: inconsistentIV)", "")]
            return Nothing
    else
      -- For the EvalMode, if there are any non-neighbourhood relative
      -- subscripts detected then add this to the eval log
      if hasNonNeighbourhoodRelatives rhses'
      then do tell [("EVALMODE: Non-neighbour relative subscripts\
                    \ (tag: nonNeighbour)","")]
              return Nothing
      else do
        -- Relativize the offsets based on the lhs
        let rhses'' = relativise lhs rhses'
        when (rhses' /= rhses'') $
          tell [("EVALMODE: Relativized spec (tag: relativized)", "")]

        let offsets  = padZeros $ map (fromJust . mapM neighbourToOffset) rhses''
        tell [("EVALMODE: dimensionality=" ++
                 show (if null offsets then 0 else length . head $ offsets), a)]
        return (Just (mult, offsets))
  where hasNonNeighbourhoodRelatives = any (elem NonNeighbour)

-- Convert list of relative offsets to a spec
relativeIxsToSpec :: [[Int]] -> Maybe Specification
relativeIxsToSpec ixs =
    if isEmpty exactSpec then Nothing else Just exactSpec
    where exactSpec = inferFromIndicesWithoutLinearity . V.fromLists $ ixs

{-| Set the type of Specification (stencil or access) based on the lhs
    set of neighbourhood indices; empty implies this is an access
    specification -}
setType :: [Neighbour] -> Specification -> Specification
setType [] (Specification spec _) = Specification spec False
setType _  (Specification spec _)  = Specification spec True

-- Given a list of the neighbourhood representation for the LHS, of size n
-- and a list of size-n lists of offsets, relativise the offsets
relativise :: [Neighbour] -> [[Neighbour]] -> [[Neighbour]]
relativise lhs rhses = foldr relativiseRHS rhses lhs
    where
      relativiseRHS (Neighbour lhsIV i) rhses =
          map (map (relativiseBy lhsIV i)) rhses
      relativiseRHS _ rhses = rhses

      relativiseBy v i (Neighbour u j) | v == u = Neighbour u (j - i)
      relativiseBy _ _ x = x

-- Helper predicates
isVariableExpr :: F.Expression a -> Bool
isVariableExpr (F.ExpValue _ _ (F.ValVariable _)) = True
isVariableExpr _                                  = False

-- Check that induction variables are used consistently
consistentIVSuse :: [Neighbour] -> [[Neighbour]] -> Bool
consistentIVSuse [] _ = True
consistentIVSuse _ [] = True
consistentIVSuse lhs rhses =
     isJust rhsBasis -- There is a consitent RHS
  && (all (`consistentWith` lhs) (fromJust rhsBasis)
   || all (`consistentWith` fromJust rhsBasis) lhs)
    where
      cmp (Neighbour v i) (Neighbour v' _) | v == v'   = Just $ Neighbour v i
                                           | otherwise = Nothing
      -- Cases for constants or non neighbour indices
      cmp n@Neighbour{}  (Constant _) = Just n
      cmp (Constant _) n@Neighbour{}  = Just n
      cmp NonNeighbour{} Neighbour{}  = Nothing
      cmp Neighbour{} NonNeighbour{}  = Nothing
      cmp _ _                         = Just $ Constant (F.ValInteger "")
      rhsBasis = foldrM (zipWithM cmp) (head rhses) (tail rhses)
      -- If there is an induction variable on the RHS, then it also occurs on
      -- the LHS
      consistentWith :: Neighbour -> [Neighbour] -> Bool
      consistentWith (Neighbour rv _) ns = any (matchesIV rv) ns
      consistentWith _                _  = True

      matchesIV :: Variable -> Neighbour -> Bool
      matchesIV v (Neighbour v' _) | v == v' = True
      -- All RHS to contain index ranges
      matchesIV v Neighbour{}      | v  == "" = True
      matchesIV _ (Neighbour v' _) | v' == "" = True
      matchesIV _ _                          = False

-- padZeros makes this rectilinear
padZeros :: [[Int]] -> [[Int]]
padZeros ixss = let m = maximum (map length ixss)
                in map (\ixs -> ixs ++ replicate (m - length ixs) 0) ixss

neighbourToOffset :: Neighbour -> Maybe Int
neighbourToOffset (Neighbour _ o) = Just o
neighbourToOffset (Constant _)    = Just absoluteRep
neighbourToOffset _               = Nothing
