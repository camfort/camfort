{- |
Module      :  Camfort.Specification.Stencils.Generate
Description :  Generate stencils for inference and synthesis
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Camfort.Specification.Stencils.Generate
  (
    EvalLog
  , Neighbour(..)
  , extractRelevantIVS
  , genOffsets
  , genSpecifications
  , isArraySubscript
  , neighbourIndex
  , runStencilInferer
   -- Various helpers that get used by other tools, e.g., array-analysis
  , isVariableExpr
  , convIxToNeighbour
  , indicesToRelativisedOffsets
  , indicesToSpec
  , neighbourToOffset
  , relativise
  , consistentIVSuse
  ) where

import           Control.Monad (void, when, zipWithM)
import           Control.Monad.State.Strict (State, get, put, runState)
import           Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Data.Data (Data)
import           Data.Foldable (foldrM)
import           Data.Generics.Uniplate.Operations (transformBi, universeBi)
import           Data.Graph.Inductive.Graph (lab, pre)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as S

import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Analysis          as FA
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position     as FU

import           Camfort.Helpers (collect)
import qualified Camfort.Helpers.Vec as V
import           Camfort.Specification.Stencils.Annotation ()
import           Camfort.Specification.Stencils.Analysis
import           Camfort.Specification.Stencils.InferenceBackend
import           Camfort.Specification.Stencils.Model
  (Approximation(..), Multiplicity(..))
import           Camfort.Specification.Stencils.Syntax
  ( absoluteRep
  , fromBool
  , groupKeyBy
  , hasDuplicates
  , isEmpty
  , isUnit
  , setLinearity
  , Specification(..)
  , Variable)
import           Language.Fortran.Repr

type Indices a = [[F.Index (FA.Analysis a)]]

type EvalLog = [(String, Variable)]

data SIEnv ann = SIEnv
  {
    -- | In-scope induction variables.
    sieIvs :: [Variable]
  , sieFlowsGraph :: FAD.FlowsGraph ann
  }

-- | Analysis for working with low-level stencil inference.
type StencilInferer ann = ReaderT (SIEnv ann) (WriterT EvalLog StencilsAnalysis)

-- | Get the list of in-scope induction variables.
getIvs :: StencilInferer ann [Variable]
getIvs = asks sieIvs

-- | Get the FlowsGraph for the current analysis.
getFlowsGraph :: StencilInferer ann (FAD.FlowsGraph ann)
getFlowsGraph = asks sieFlowsGraph

runStencilInferer :: StencilInferer ann a -> [Variable] -> FAD.FlowsGraph ann -> StencilsAnalysis (a, EvalLog)
runStencilInferer si ivs flowsGraph = do
  let senv = SIEnv { sieIvs = ivs, sieFlowsGraph = flowsGraph }
  runWriterT $ runReaderT si senv

{-| Representation for indices as either:
     * neighbour indices
     * constant
     * non neighbour index -}
data Neighbour = Neighbour Variable Int
               | Constant (F.Value ())
               | NonNeighbour deriving (Eq, Show)


{-| Match expressions which are array subscripts, returning Just of their
    index expressions, else Nothing -}
isArraySubscript :: F.Expression (FA.Analysis a) -> Maybe [F.Index (FA.Analysis a)]
isArraySubscript (F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable _)) subs) =
   Just $ F.aStrip subs
isArraySubscript (F.ExpDataRef _ _ e e') =
   isArraySubscript e <> isArraySubscript e'
isArraySubscript _ = Nothing

{-| Given an induction-variable-map, convert a list of indices to
    Maybe a list of constant or neighbourhood indices.
    If any are non neighbourhood then return Nothing -}
neighbourIndex :: (Data a)
               => FAD.InductionVarMapByASTBlock
               -> [F.Index (FA.Analysis a)]
               -> Maybe [Neighbour]
neighbourIndex ivs ixs =
  if NonNeighbour `notElem` neighbours
  then Just neighbours
  else Nothing
    where
      neighbours = map (\ix -> convIxToNeighbour (extractRelevantIVS ivs ix) ix) ixs

genSpecifications
  :: (Data a, Show a, Eq a)
  => [Neighbour]
  -> F.Block (FA.Analysis a)
  -> StencilInferer a ([([Variable], Specification)], [Int])
genSpecifications lhs block = do
  (subscripts, visitedNodes) <- genSubscripts [block]
  varToSpecs <- assocsSequence . mkSpecs $ subscripts
  case varToSpecs of
    [] -> do
       tell [("EVALMODE: Empty specification (tag: emptySpec)", "")]
       return ([], visitedNodes)
    _ -> do
       let varsToSpecs = groupKeyBy varToSpecs
       return (splitUpperAndLower varsToSpecs, visitedNodes)
  where
    mkSpecs = M.mapWithKey (`indicesToSpec` lhs)

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

genOffsets
  :: (Data a, Show a, Eq a)
  => [Neighbour]
  -> [F.Block (FA.Analysis a)]
  -> StencilInferer a [(Variable, (Bool, [[Int]]))]
genOffsets lhs blocks = do
  (subscripts, _) <- genSubscripts blocks
  assocsSequence . mkOffsets $ subscripts
  where
    mkOffsets = M.mapWithKey (`indicesToRelativisedOffsets` lhs)

{-| genSubscripts
   Takes * a list of blocks representing an RHS
   Returns a map from array variables to indices, and a list of
   nodes that were visited when computing this information -}
genSubscripts
  :: (Data a, Show a, Eq a)
  => [F.Block (FA.Analysis a)]
  -> StencilInferer a (M.Map Variable (Indices a), [Int])
genSubscripts blocks = do
  flowsGraph <- getFlowsGraph
  let (maps, visitedNodes) = runState (mapM (genSubscripts' True flowsGraph) blocks) []
      subscripts = M.unionsWith (++) maps
  pure (subscripts, visitedNodes)
  where
    -- Generate all subscripting expressions (that are translations on
    -- induction variables) that flow to this block
    -- The State monad provides a list of the visited nodes so far
    genSubscripts'
      :: (Data a, Show a, Eq a)
      => Bool
      -> FAD.FlowsGraph a
      -> F.Block (FA.Analysis a)
      -> State [Int] (M.Map Variable (Indices a))

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
            -- Try to get the block from the flowsGraph before analysis its rhses
            let blockG = fromMaybe block (lab flowsGraph node)
            dependencies <- mapM (genSubscripts' False flowsGraph) blocksFlowingIn
            return $ M.unionsWith (++) (genRHSsubscripts blockG : dependencies)

         Nothing -> error $ "Missing a label for: " ++ show block

-- | Given an induction variable map, and a piece of syntax
-- return a list of induction variables in scope for this index
extractRelevantIVS :: (FU.Spanned (ast (FA.Analysis a)), F.Annotated ast) =>
     FAD.InductionVarMapByASTBlock
  -> ast (FA.Analysis a)
  -> [Variable]
extractRelevantIVS ivmap f = ivsList
  where
    ivsList = S.toList $ fromMaybe S.empty $ IM.lookup label ivmap

    label   = fromMaybe (error errorMsg) (FA.insLabel . F.getAnnotation $ f)
    -- For debugging purposes
    errorMsg = show (FU.getSpan f)
            ++ " get IVs associated to labelled index "

{-| Given a list of induction variables and an index, compute
   its Neighbour representation
   e.g., for the expression a(i+1,j-1) then this function gets
   passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1) -}
convIxToNeighbour :: (Data a) => [Variable] -> F.Index (FA.Analysis a) -> Neighbour
convIxToNeighbour _ (F.IxRange _ _ Nothing Nothing Nothing)     = Neighbour "" 0
convIxToNeighbour _ (F.IxRange _ _ Nothing Nothing
                  (Just (F.ExpValue _ _ (F.ValInteger "1" _)))) = Neighbour "" 0

convIxToNeighbour ivs (F.IxSingle _ _ _ expr)  = expToNeighbour ivs expr
convIxToNeighbour _ _ = NonNeighbour -- indexing expression is a range

-- Combinator for reducing a map with effects and partiality inside
-- into an effectful list of key-value pairs
assocsSequence :: Monad m => M.Map k (m (Maybe a)) -> m [(k, a)]
assocsSequence maps = do
  assocs <- mapM strength . M.toList $ maps
  return . mapMaybe strength $ assocs
  where
    strength :: Monad m => (a, m b) -> m (a, b)
    strength (a, mb) = mb >>= (\b -> return (a, b))

-- Convert list of indexing expressions to a spec
indicesToSpec :: (Data a)
              => Variable
              -> [Neighbour]
              -> Indices a
              -> StencilInferer a (Maybe Specification)
indicesToSpec a lhs ixs = do
  mMultOffsets <- indicesToRelativisedOffsets a lhs ixs
  return $ do
    (mult, offsets) <- mMultOffsets
    spec <- relativeIxsToSpec offsets
    let spec' = setLinearity (fromBool mult) spec
    return $ setType lhs spec'

-- Get all RHS subscript which are translated induction variables
-- return as a map from (source name) variables to a list of relative indices
genRHSsubscripts :: forall a. (Data a, Eq a)
                 => F.Block (FA.Analysis a) -> M.Map Variable (Indices a)
genRHSsubscripts block = genRHSsubscripts' (transformBi replaceModulo block)
  where
    -- Any occurence of an subscript "modulo(e, e')" is replaced with "e"
    replaceModulo :: F.Expression (FA.Analysis a) -> F.Expression (FA.Analysis a)
    replaceModulo (F.ExpFunctionCall _ _
                      (F.ExpValue _ _ (F.ValIntrinsic iname)) subs)
        | iname `elem` ["modulo", "mod", "amod", "dmod"]
        -- We expect that the first parameter to modulo is being treated
        -- as an IxSingle element
        , arg@F.Argument{} : _ <- F.aStrip subs = F.argExprNormalize (F.argumentExpr arg)
    replaceModulo e = e

    genRHSsubscripts' b =
       collect [ (FA.srcName expr, e)
         | F.ExpSubscript _ _ expr subs <- FA.rhsExprs b
         , isVariableExpr expr
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

-- use constant-expression analysis if available
expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                    e1@(F.ExpValue _ _ (F.ValVariable _))
                    e2)
    | FA.varName e1 `elem` ivs
    , Just offs <- FA.constExp (F.getAnnotation e2) >>= fromConstInt
    = Neighbour (FA.varName e1) (fromIntegral offs)

expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                 e@(F.ExpValue _ _ (F.ValVariable _))
                   (F.ExpValue _ _ (F.ValInteger offs _)))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) (read offs)

-- use constant-expression analysis if available
expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                    e1@(F.ExpValue _ _ (F.ValVariable _))
                    e2)
    | FA.varName e1 `elem` ivs
    , Just offs <- FA.constExp (F.getAnnotation e2) >>= fromConstInt
    = Neighbour (FA.varName e1) (fromIntegral offs)

expToNeighbour ivs (F.ExpBinary _ _ F.Addition
                  (F.ExpValue _ _ (F.ValInteger offs _))
                e@(F.ExpValue _ _ (F.ValVariable _)))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) (read offs)

-- use constant-expression analysis if available
expToNeighbour ivs (F.ExpBinary _ _ F.Subtraction
                    e1@(F.ExpValue _ _ (F.ValVariable _))
                    e2)
   | FA.varName e1 `elem` ivs
   , Just offs <- FA.constExp (F.getAnnotation e2) >>= fromConstInt
   , offs' <- if offs < 0 then abs offs else (- offs) = Neighbour (FA.varName e1) (fromIntegral offs')

expToNeighbour ivs (F.ExpBinary _ _ F.Subtraction
                 e@(F.ExpValue _ _ (F.ValVariable _))
                   (F.ExpValue _ _ (F.ValInteger offs _)))
   | FA.varName e `elem` ivs =
         Neighbour (FA.varName e) (if x < 0 then abs x else (- x))
             where x = read offs

expToNeighbour ivs expr =
  -- Record when there is some kind of relative index on an inducion variable
  -- but that is not a neighbourhood index by our definitions
  if null ivs' then Constant (F.ValInteger "0" Nothing) else NonNeighbour
  where
    -- set of all induction variables involved in this expression
    ivs' = [i | e@(F.ExpValue _ _ F.ValVariable{})
                 <- universeBi expr :: [F.Expression (FA.Analysis a)]
                , let i = FA.varName e
                , i `elem` ivs]

indicesToRelativisedOffsets :: (Data a)
                            => Variable
                            -> [Neighbour]
                            -> Indices a
                            -> StencilInferer a (Maybe (Bool, [[Int]]))
indicesToRelativisedOffsets a lhs ixs = do
  ivs <- getIvs
   -- Convert indices to neighbourhood representation
  let rhses = fmap (fmap (convIxToNeighbour ivs)) ixs

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
setType xs (Specification spec _) | all isConstant xs = Specification spec False
  where
    isConstant (Constant _) = True
    isConstant _            = False
setType _  (Specification spec _)  = Specification spec True

-- Given a list of the neighbourhood representation for the LHS, of size n
-- and a list of size-n lists of offsets, relativise the offsets
relativise :: [Neighbour] -> [[Neighbour]] -> [[Neighbour]]
relativise lhs rhses = foldr relativiseRHS rhses lhs
  where
    relativiseRHS (Neighbour lhsIV i) rs =
        map (map (relativiseBy lhsIV i)) rs
    relativiseRHS _ rs = rs

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
      cmp _ _                         = Just $ Constant (F.ValInteger "" Nothing)
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