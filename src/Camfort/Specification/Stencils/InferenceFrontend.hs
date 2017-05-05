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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Camfort.Specification.Stencils.InferenceFrontend where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding (Product)

import Camfort.Analysis.CommentAnnotator

import Camfort.Specification.Stencils.InferenceBackend
import Camfort.Specification.Stencils.Model
import Camfort.Specification.Stencils.Syntax
import Camfort.Specification.Stencils.Annotation ()
import qualified Camfort.Specification.Stencils.Grammar as Gram
import qualified Camfort.Specification.Stencils.Synthesis as Synth
import Camfort.Analysis.Annotations
import Camfort.Helpers (collect, descendReverseM, descendBiReverseM)
import qualified Camfort.Helpers.Vec as V
import Camfort.Input

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Operations
import Data.Graph.Inductive.Graph hiding (isEmpty)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

-- Define modes of interaction with the inference
data InferMode =
  DoMode | AssignMode | CombinedMode | EvalMode | Synth
  deriving (Eq, Show, Data, Read)

instance Default InferMode where
    defaultValue = AssignMode

data InferState = IS {
     ivMap        :: FAD.InductionVarMapByASTBlock
   , hasSpec      :: [(FU.SrcSpan, Variable)]
   , visitedNodes :: [Int]}


-- The inferer returns information as a LogLine
type EvalLog = [(String, Variable)]
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
-- The core of the inferer works within this monad
type Inferer = WriterT [LogLine]
                 (ReaderT (FAD.FlowsGraph A)
                    (State InferState))

type Params = (?flowsGraph :: FAD.FlowsGraph A, ?nameMap :: FAR.NameMap)

runInferer :: FAD.InductionVarMapByASTBlock
           -> FAD.FlowsGraph A
           -> Inferer a
           -> (a, [LogLine])
runInferer ivmap flTo =
    flip evalState (IS ivmap [] [])
  . flip runReaderT flTo
  . runWriterT

stencilInference :: FAR.NameMap
                 -> InferMode
                 -> Char
                 -> F.ProgramFile (FA.Analysis A)
                 -> (F.ProgramFile (FA.Analysis A), [LogLine])
stencilInference nameMap mode marker pf =
    (F.ProgramFile mi cm_pus' blocks', log1 ++ log2)
  where
    -- Parse specification annotations and include them into the syntax tree
    -- that way if generate specifications at the same place we can
    -- decide whether to synthesise or not

    -- TODO: might want to output log0 somehow (though it doesn't fit LogLine)
    (pf'@(F.ProgramFile mi cm_pus blocks), log0) =
         if mode == Synth
          then runWriter (annotateComments Gram.specParser pf)
          else (pf, [])

    (cm_pus', log1) = runWriter (transformBiM perPU cm_pus)
    (blocks', log2) = runInferer ivMap flTo blocksInf
    blocksInf       = let ?flowsGraph = flTo
                          ?nameMap    = nameMap
                      in descendBiM (perBlockInfer mode marker) blocks

    -- Run inference per program unit, placing the flowsmap in scope
    perPU :: F.ProgramUnit (FA.Analysis A)
          -> Writer [LogLine] (F.ProgramUnit (FA.Analysis A))

    perPU pu | Just _ <- FA.bBlocks $ F.getAnnotation pu =
         let ?flowsGraph = flTo
             ?nameMap    = nameMap
         in do
              let pum = descendBiM (perBlockInfer mode marker) pu
              let (pu', log) = runInferer ivMap flTo pum
              tell log
              return pu'
    perPU pu = return pu

    -- induction variable map
    ivMap = FAD.genInductionVarMapByASTBlock beMap gr
    -- perform reaching definitions analysis
    rd    = FAD.reachingDefinitions dm gr
    -- create graph of definition "flows"
    flTo =  FAD.genFlowsToGraph bm dm gr rd

    -- identify every loop by its back-edge
    beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr

    -- get map of AST-Block-ID ==> corresponding AST-Block
    bm    = FAD.genBlockMap pf'
    -- get map of program unit ==> basic block graph
    bbm   = FAB.genBBlockMap pf'
    -- build the supergraph of global dependency
    sgr   = FAB.genSuperBBGr bbm
    -- extract the supergraph itself
    gr    = FAB.superBBGrGraph sgr

    -- get map of variable name ==> { defining AST-Block-IDs }
    dm    = FAD.genDefMap bm

{- *** 1 . Core inference over blocks -}

genSpecsAndReport :: Params
  => InferMode -> FU.SrcSpan -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Inferer [([Variable], Specification)]
genSpecsAndReport mode span lhs blocks = do
    (IS ivmap _ _) <- get
    let ((specs, visited), evalInfos) = runWriter $ genSpecifications ivmap lhs blocks
    modify (\state -> state { visitedNodes = (visitedNodes state) ++ visited })
    tell [ (span, Left specs) ]
    if mode == EvalMode
      then do
         tell [ (span, Right ("EVALMODE: assign to relative array subscript\
                              \ (tag: tickAssign)","")) ]
         mapM_ (\evalInfo -> tell [ (span, Right evalInfo) ]) evalInfos
         mapM_ (\spec -> if show spec == ""
                          then tell [ (span, Right ("EVALMODE: Cannot make spec\
                                                   \ (tag: emptySpec)","")) ]
                          else return ()) specs
         return specs
      else return specs



-- Match expressions which are array subscripts, returning Just of their
-- index expressions, else Nothing
isArraySubscript :: F.Expression (FA.Analysis A)
                 -> Maybe [F.Index (FA.Analysis A)]
isArraySubscript (F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable _)) subs) =
   Just $ F.aStrip subs
isArraySubscript (F.ExpDataRef _ _ e e') = do
   isArraySubscript e <++> isArraySubscript e'
 where
   Nothing <++> Nothing = Nothing
   Nothing <++> Just xs = Just xs
   Just xs <++> Nothing  = Just xs
   Just xs <++> Just ys  = Just (xs ++ ys)
isArraySubscript _ = Nothing

fromJustMsg _ (Just x) = x
fromJustMsg msg Nothing = error msg

-- Traverse Blocks in the AST and infer stencil specifications
perBlockInfer :: Params
               => InferMode -> Char -> F.Block (FA.Analysis A)
               -> Inferer (F.Block (FA.Analysis A))

perBlockInfer Synth _ b@(F.BlComment ann _ _) = do
  -- If we have a comment that is actually a specification then record that
  -- this has been assigned so that we don't generate extra specifications
  -- that overlap with user-given oones
  ann' <- return $ FA.prevAnnotation ann
  -- Check if we have a spec
  case (stencilSpec ann', stencilBlock ann') of
    -- Comment contains an (uncoverted) specification and an associated block
    (Just (Left (Gram.SpecDec _  vars)), Just block) ->
     -- Is the block an assignment
     case block of
      F.BlStatement _ span _ F.StExpressionAssign{} -> do
         -- Then update the list of spans+vars that have specifications
         state <- get
         put (state { hasSpec = hasSpec state ++ zip (repeat span) vars })
    _ -> return ()
  return b

perBlockInfer mode marker b@(F.BlStatement ann span@(FU.SrcSpan lp _) _ stmnt)
  | mode == AssignMode || mode == CombinedMode || mode == EvalMode || mode == Synth = do

    (IS ivmap hasSpec visitedStmts) <- get
    let label = fromMaybe (-1) (FA.insLabel ann)
    if (label `elem` visitedStmts)
    then -- This statement has been part of a visited dataflow path
      return b
    else do
      -- On all StExpressionAssigns that occur in stmt....
      let lhses = [lhs | (F.StExpressionAssign _ _ lhs _)
                           <- universe stmnt :: [F.Statement (FA.Analysis A)]]
      specs <- forM lhses $ \lhs -> do
         -- ... apply the following:
         case lhs of
          -- Assignment to a variable
          (F.ExpValue _ _ (F.ValVariable v)) -> genSpecsAndReport mode span [] [b]
          _ -> case isArraySubscript lhs of
             Just subs ->
               -- Left-hand side is a subscript-by relative index or by a range
               case neighbourIndex ivmap subs of
                 Just lhs -> genSpecsAndReport mode span lhs [b]
                 Nothing  -> if mode == EvalMode
                             then do
                               tell [(span , Right ("EVALMODE: LHS is an array\
                                                   \ subscript we can't handle \
                                                   \(tag: LHSnotHandled)",""))]
                               return []
                             else return []
             -- Not an assign we are interested in
             _ -> return []
      if mode == Synth && not (null specs) && specs /= [[]]
      then
        let specComment = Synth.formatSpec (Just (tabs ++ '!':marker:" ")) ?nameMap (span, Left (concat specs'))
            specs' = map (mapMaybe noSpecAlready) specs
            noSpecAlready (vars, spec) =
               if null vars'
               then Nothing
               else Just (vars', spec)
               where vars' = filter (\v -> not ((span, realName v) `elem` hasSpec)) vars
            realName v = v `fromMaybe` (v `M.lookup` ?nameMap)
            tabs  = take (FU.posColumn lp  - 1) (repeat ' ')
            (FU.SrcSpan loc _) = span
            span' = FU.SrcSpan (lp {FU.posColumn = 0}) (lp {FU.posColumn = 0})
            ann'  = ann { FA.prevAnnotation = (FA.prevAnnotation ann) { refactored = Just loc } }
        in return $ F.BlComment ann' span' (F.Comment specComment)
      else return b

perBlockInfer mode marker b@(F.BlDo ann span lab cname lab' mDoSpec body tlab) = do
    if (mode == DoMode || mode == CombinedMode) && isStencilDo b
      then genSpecsAndReport mode span [] body
      else return []

    -- descend into the body of the do-statement (in reverse order)
    body' <- mapM (descendBiReverseM (perBlockInfer mode marker)) (reverse body)
    return $ F.BlDo ann span lab cname lab' mDoSpec body' tlab

perBlockInfer mode marker b = do
    -- Go inside child blocks
    b' <- descendReverseM (descendBiReverseM (perBlockInfer mode marker)) $ b
    return b'

-- Combiantor for reducing a map with effects and partiality inside
-- into an effectful list of key-value pairs
assocsSequence :: Monad m => M.Map k (m (Maybe a)) -> m [(k, a)]
assocsSequence maps = do
    assocs <- sequence . map strength . M.toList $ maps
    return . catMaybes . map strength $ assocs
  where
    strength :: Monad m => (a, m b) -> m (a, b)
    strength (a, mb) = mb >>= (\b -> return (a, b))

genSpecifications :: Params
  => FAD.InductionVarMapByASTBlock
  -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Writer EvalLog ([([Variable], Specification)], [Int])
genSpecifications ivs lhs blocks = do
    let (subscripts, visitedNodes) = subscriptsOnRhs ?nameMap blocks
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
      splitUpperAndLower' (vs, Specification (Mult (Bound (Just l) (Just u))))
        | isUnit l =
         [(vs, Specification (Mult (Bound Nothing (Just u))))]
        | otherwise =
         [(vs, Specification (Mult (Bound (Just l) Nothing))),
          (vs, Specification (Mult (Bound Nothing (Just u))))]
      splitUpperAndLower' (vs, Specification (Once (Bound (Just l) (Just u))))
        | isUnit l =
         [(vs, Specification (Mult (Bound Nothing (Just u))))]
        | otherwise =
         [(vs, Specification (Once (Bound (Just l) Nothing))),
          (vs, Specification (Once (Bound Nothing (Just u))))]
      splitUpperAndLower' x = [x]

{-| subscriptsOnRhs
   Takes * a name map
         * a list of blocks representing an RHS
   Returns a map from array variables to indices, and a list of
   nodes that were visited when computing this information -}
subscriptsOnRhs :: Params
  => FAR.NameMap
  -> [F.Block (FA.Analysis A)]
  -> (M.Map Variable [[F.Index (FA.Analysis A)]], [Int])
subscriptsOnRhs nameMap blocks =
    (subscripts', visitedNodes)
  where
    (maps, visitedNodes) = runState (mapM (genSubscripts True) blocks) []
    subscripts = M.unionsWith (++) maps
    subscripts' = filterOutFuns ?nameMap subscripts

genOffsets :: Params
  => FAD.InductionVarMapByASTBlock
  -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Writer EvalLog [(Variable, (Bool, [[Int]]))]
genOffsets ivs lhs blocks = do
    let (subscripts, _) = subscriptsOnRhs ?nameMap blocks
    assocsSequence $ mkOffsets subscripts
  where
    mkOffsets = M.mapWithKey (\v -> indicesToRelativisedOffsets ivs v lhs)


-- Filter out any variable names which do not appear in the name map or
-- which in appear in the name map with the same name, indicating they
-- are an instric function, e.g., abs
filterOutFuns nameMap m =
  M.filterWithKey (\k _ ->
     case k `M.lookup` nameMap of
        Nothing           -> False
        Just k' | k == k' -> False
        _                 -> True) m

-- Generate all subscripting expressions (that are translations on
-- induction variables) that flow to this block
-- The State monad provides a list of the visited nodes so far
genSubscripts :: Params
  => Bool
  -> F.Block (FA.Analysis A)
  -> State [Int] (M.Map Variable [[F.Index (FA.Analysis A)]])
genSubscripts False (F.BlStatement _ _ _ (F.StExpressionAssign _ _ e _))
    | isArraySubscript e /= Nothing
    -- Don't pull dependencies through arrays
    = return M.empty

genSubscripts _ block = do
    visited <- get
    case (FA.insLabel $ F.getAnnotation block) of

      Just node
        | node `elem` visited ->
          -- This dependency has already been visited during this traversal
          return $ M.empty
        | otherwise -> do
          -- Fresh dependency
          put $ node : visited
          let blocksFlowingIn = mapMaybe (lab ?flowsGraph) $ pre ?flowsGraph node
          dependencies <- mapM (genSubscripts False) blocksFlowingIn
          return $ M.unionsWith (++) (genRHSsubscripts block : dependencies)

      Nothing -> error $ "Missing a label for: " ++ show block

-- Get all RHS subscript which are translated induction variables
-- return as a map from (program) variables to a list of relative indices and
-- a flag marking whether there are any duplicate indices
genRHSsubscripts ::
     F.Block (FA.Analysis A)
  -> M.Map Variable [[F.Index (FA.Analysis A)]]
genRHSsubscripts b = genRHSsubscripts' (transformBi replaceModulo b)
  where
    -- Any occurence of an subscript "modulo(e, e')" is replaced with "e"
    replaceModulo :: F.Expression (FA.Analysis A) -> F.Expression (FA.Analysis A)
    replaceModulo e@(F.ExpSubscript _ _
                    (F.ExpValue _ _ (F.ValVariable "modulo")) subs) =
        -- We expect that the first parameter to modulo is being treated
        -- as an IxSingle element
        case (head $ F.aStrip subs) of
           (F.IxSingle _ _ _ e') -> e'
           _                     -> e
    replaceModulo e = e

    genRHSsubscripts' b =
       collect [ (FA.varName exp, e)
         | F.ExpSubscript _ _ exp subs <- FA.rhsExprs b
         , isVariableExpr exp
         , let e = F.aStrip subs
         , not (null e)]

getInductionVar :: Maybe (F.DoSpecification (FA.Analysis A)) -> [Variable]
getInductionVar (Just (F.DoSpecification _ _ (F.StExpressionAssign _ _ e _) _ _))
  | isVariableExpr e = [FA.varName e]
getInductionVar _ = []

isStencilDo :: F.Block (FA.Analysis A) -> Bool
isStencilDo (F.BlDo _ _ _ _ _ mDoSpec body _) =
 -- Check to see if the body contains any affine use of the induction variable
 -- as a subscript
 case getInductionVar mDoSpec of
    [] -> False
    [ivar] -> length exprs > 0 &&
               and [ all (\sub -> sub `isNeighbour` [ivar]) subs' |
               F.ExpSubscript _ _ _ subs <- exprs
               , let subs' = F.aStrip subs
               , not (null subs') ]
      where exprs = universeBi upToNextDo :: [F.Expression (FA.Analysis A)]
            upToNextDo = takeWhile (not . isDo) body
            isDo (F.BlDo {}) = True
            isDo _            = False
isStencilDo _  = False

{- *** 2 .Conversion from indexing expressions -}

-- padZeros makes this rectilinear
padZeros :: [[Int]] -> [[Int]]
padZeros ixss = let m = maximum (map length ixss)
                in map (\ixs -> ixs ++ replicate (m - length ixs) 0) ixss

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
    let spec = relativeIxsToSpec offsets
    fmap (setLinearity (fromBool mult)) spec

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
        if rhses' /= rhses''
          then  tell [("EVALMODE: Relativized spec (tag: relativized)", "")]
          else return ()

        let offsets  = padZeros $ map (fromJust . mapM neighbourToOffset) rhses''
        tell [("EVALMODE: dimensionality=" ++
                 show (if null offsets then 0 else length . head $ offsets), a)]
        return (Just $ (mult, offsets))
  where hasNonNeighbourhoodRelatives xs = or (map (any ((==) NonNeighbour)) xs)


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

-- Check that induction variables are used consistently
consistentIVSuse :: [Neighbour] -> [[Neighbour]] -> Bool
consistentIVSuse [] _ = True
consistentIVSuse _ [] = True
consistentIVSuse lhs rhses =
     rhsBasis /= Nothing  -- There is a consitent RHS
  && (all (`consistentWith` lhs) (fromJust rhsBasis)
   || all (`consistentWith` (fromJust rhsBasis)) lhs)
    where
      cmp (Neighbour v i) (Neighbour v' _) | v == v'   = Just $ Neighbour v i
                                           | otherwise = Nothing
      -- Cases for constants or non neighbour indices
      cmp n@(Neighbour {})  (Constant _)   = Just n
      cmp (Constant _) n@(Neighbour {})    = Just n
      cmp (NonNeighbour {}) (Neighbour {}) = Nothing
      cmp (Neighbour {}) (NonNeighbour{})  = Nothing
      cmp _ _                              = Just $ Constant (F.ValInteger "")
      rhsBasis = foldrM (\a b -> mapM (uncurry cmp) $ zip a b) (head rhses) (tail rhses)
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

-- Convert list of relative offsets to a spec
relativeIxsToSpec :: [[Int]] -> Maybe Specification
relativeIxsToSpec ixs =
    if isEmpty exactSpec then Nothing else Just exactSpec
    where exactSpec = inferFromIndicesWithoutLinearity . V.fromLists $ ixs

isNeighbour :: Data a => F.Index (FA.Analysis a) -> [Variable] -> Bool
isNeighbour exp vs =
    case (ixToNeighbour' vs exp) of
        Neighbour _ _ -> True
        _             -> False

-- Given a list of induction variables and a list of indices
-- map them to a list of constant or neighbourhood indices
-- if any are non neighbourhood then return Nothi ng
neighbourIndex :: FAD.InductionVarMapByASTBlock
               -> [F.Index (FA.Analysis Annotation)] -> Maybe [Neighbour]
neighbourIndex ivs ixs =
  if all ((/=) NonNeighbour) neighbours
  then Just neighbours
  else Nothing
    where neighbours = map (ixToNeighbour ivs) ixs

-- Representation for indices as either:
--   * neighbour indices
--   * constant
--   * non neighbour index
data Neighbour = Neighbour Variable Int
               | Constant (F.Value ())
               | NonNeighbour deriving (Eq, Show)

neighbourToOffset :: Neighbour -> Maybe Int
neighbourToOffset (Neighbour _ o) = Just o
neighbourToOffset (Constant _)    = Just absoluteRep
neighbourToOffset _               = Nothing

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

ixToNeighbour' _ (F.IxRange _ _ Nothing Nothing Nothing)     = Neighbour "" 0
ixToNeighbour' _ (F.IxRange _ _ Nothing Nothing
                  (Just (F.ExpValue _ _ (F.ValInteger "1")))) = Neighbour "" 0

ixToNeighbour' ivs (F.IxSingle _ _ _ exp)  = expToNeighbour ivs exp
ixToNeighbour' _ _ = NonNeighbour -- indexing expression is a range

-- Given a list of induction variables and an expression, compute its
-- Neighbour representation
expToNeighbour :: forall a. Data a
            => [Variable] -> F.Expression (FA.Analysis a) -> Neighbour

expToNeighbour ivs e@(F.ExpValue _ _ v@(F.ValVariable _))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) 0
    | otherwise               = Constant (fmap (const ()) v)

expToNeighbour _ (F.ExpValue _ _ val) = Constant (fmap (const ()) val)

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
    ivs' = [i | e@(F.ExpValue _ _ (F.ValVariable {}))
                 <- universeBi e :: [F.Expression (FA.Analysis a)]
                , let i = FA.varName e
                , i `elem` ivs]

--------------------------------------------------

-- Helper predicates
isUnaryOrBinaryExpr :: F.Expression a -> Bool
isUnaryOrBinaryExpr (F.ExpUnary {})  = True
isUnaryOrBinaryExpr (F.ExpBinary {}) = True
isUnaryOrBinaryExpr _                = False

isVariableExpr :: F.Expression a -> Bool
isVariableExpr (F.ExpValue _ _ (F.ValVariable _)) = True
isVariableExpr _                                  = False

-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
