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

module Camfort.Analysis.StencilSpecification.InferenceFrontend where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding (Product)

import Camfort.Analysis.StencilSpecification.InferenceBackend
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Helpers.Vec
import Camfort.Input

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
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
import Data.List
import Debug.Trace

type Variable = String

-- Define modes of interaction with the inference
data InferMode =
  DoMode | AssignMode | CombinedMode | EvalMode
  deriving (Eq, Show, Data, Read)

instance Default InferMode where
    defaultValue = AssignMode

-- The inferer returns information as a LogLine
type EvalLog = [(String, Variable)]
type LogLine = (FU.SrcSpan, Either [([Variable], Specification)] (String,Variable))
-- The core of the inferer works within this monad
type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State FAD.InductionVarMapByASTBlock))
type Cycles = [(F.Name, F.Name)]

runInferer :: FAD.InductionVarMapByASTBlock -> Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer ivmap cycles puName tenv =
  flip evalState ivmap . flip runReaderT (cycles, puName, tenv) . execWriterT

stencilInference :: FAR.NameMap -> InferMode -> F.ProgramFile (FA.Analysis A) -> [LogLine]
stencilInference nameMap mode pf@(F.ProgramFile _ _) = concatMap perPU (universeBi cm_pus')
  where
    pf'@(F.ProgramFile cm_pus' _) = fillInductionVarsOfIndices pf

    -- Run inference per program unit, placing the flowsmap in scope
    perPU :: F.ProgramUnit (FA.Analysis A) -> [LogLine]

    perPU pu | Just _ <- FA.bBlocks $ F.getAnnotation pu =
         let ?flowsGraph = flTo
         in runInferer ivMap [] (FA.puName pu) tenv (descendBiM (perBlockInfer mode) pu)
    perPU _ = []

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
    tenv  = FAT.inferTypes pf'

ixsspan :: F.Index (FA.Analysis A)  -> FU.SrcSpan
ixsspan  (F.IxRange _ sp _ _ _) = sp
ixsspan (F.IxSingle _ sp _ _ ) = sp

-- | Fills out the insLabel and the indices values of F.Index
-- AST-nodes based upon the F.Block AST-node that it is contained
-- within most directly.
fillInductionVarsOfIndices :: F.ProgramFile (FA.Analysis A) -> F.ProgramFile (FA.Analysis A)
fillInductionVarsOfIndices = transformBi perBlock
  where
    perBlock :: F.Block (FA.Analysis A) -> F.Block (FA.Analysis A)
    perBlock (F.BlStatement a s e st)    = F.BlStatement a s (mfill i e) (fill i st)               where i = FA.insLabel a
    perBlock (F.BlIf a s e1 e2 bss)      = F.BlIf a s (mfill i e1) (mmfill i e2) bss               where i = FA.insLabel a
    perBlock (F.BlCase a s e1 e2 is bss) = F.BlCase a s (mfill i e1) (fill i e2) (mmfill i is) bss where i = FA.insLabel a
    perBlock (F.BlDo a s e1 e2 bs)       = F.BlDo a s (mfill i e1) (mfill i e2) bs                 where i = FA.insLabel a
    perBlock (F.BlDoWhile a s e1 e2 bs)  = F.BlDoWhile a s (mfill i e1) (fill i e2) bs             where i = FA.insLabel a
    perBlock b                           = b

    mfill i  = fmap (fill i)
    mmfill i = fmap (fmap (fill i))

    fill :: forall f. (Data (f (FA.Analysis A))) => Maybe Int -> f (FA.Analysis A) -> f (FA.Analysis A)
    fill Nothing  = id
    fill (Just i) = transform perIndex
      where
        transform :: (F.Index (FA.Analysis A) -> F.Index (FA.Analysis A)) -> f (FA.Analysis A) -> f (FA.Analysis A)
        transform = transformBi

        perIndex :: (F.Index (FA.Analysis A) -> F.Index (FA.Analysis A))
        perIndex x = (show (ixsspan x) ++ " l= " ++ show (FA.insLabel (F.getAnnotation y))) `trace` y
          where
            y   = F.setAnnotation (a { FA.insLabel = Just i, FA.prevAnnotation = pa' }) x
            a   = F.getAnnotation x
            pa  = FA.prevAnnotation a
            pa' = pa { indices = ["HI!"] }
            --ivs = S.toList (S.empty `fromMaybe` IM.lookup i ivMap)
            --pa' = pa { indices = ivs }

-- | Return list of variable names that flow into themselves via a 2-cycle
findVarFlowCycles :: Data a => F.ProgramFile a -> [(F.Name, F.Name)]
findVarFlowCycles = FAR.underRenaming (findVarFlowCycles' . FAB.analyseBBlocks)
findVarFlowCycles' pf = cycs2
  where
    bm    = FAD.genBlockMap pf     -- get map of AST-Block-ID ==> corresponding AST-Block
    bbm   = FAB.genBBlockMap pf    -- get map of program unit ==> basic block graph
    sgr   = FAB.genSuperBBGr bbm   -- stitch all of the graphs together into a 'supergraph'
    gr    = FAB.superBBGrGraph sgr -- extract the supergraph itself
    dm    = FAD.genDefMap bm       -- get map of variable name ==> { defining AST-Block-IDs }
    rd    = FAD.reachingDefinitions dm gr   -- perform reaching definitions analysis
    flTo  = FAD.genFlowsToGraph bm dm gr rd -- create graph of definition "flows"
    -- VarFlowsToMap: A -> { B, C } indicates that A contributes to B, C.
    flMap = FAD.genVarFlowsToMap dm flTo -- create VarFlowsToMap
    -- find 2-cycles: A -> B -> A
    cycs2 = [ (n, m) | (n, ns) <- M.toList flMap
                     , m       <- S.toList ns
                     , ms      <- maybeToList $ M.lookup m flMap
                     , n `S.member` ms && n /= m ]

{- *** 1 . Core inference over blocks -}

genSpecsAndReport :: (?flowsGraph :: FAD.FlowsGraph A)
  => InferMode -> FU.SrcSpan -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Inferer ()
genSpecsAndReport mode span lhs blocks = do
    ivmap <- get
    let (specs, evalInfos) = runWriter $ genSpecifications ivmap lhs blocks
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
     else return ()

-- Match expressions which are array subscripts, returning Just of their
-- index expressions, else Nothing
isArraySubscript :: F.Expression (FA.Analysis A)
                 -> Maybe [F.Index (FA.Analysis A)]
isArraySubscript (F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable _)) subs) =
   Just $ F.aStrip subs
isArraySubscript (F.ExpDataRef _ _ _ e) = isArraySubscript e
isArraySubscript _ = Nothing

fromJustMsg msg (Just x) = x
fromJustMsg msg Nothing = error msg

-- Traverse Blocks in the AST and infer stencil specifications
perBlockInfer :: (?flowsGraph :: FAD.FlowsGraph A)
    => InferMode -> F.Block (FA.Analysis A) -> Inferer (F.Block (FA.Analysis A))

perBlockInfer mode b@(F.BlStatement ann span _ stmnt)
  | mode == AssignMode || mode == CombinedMode || mode == EvalMode = do
    -- On all StExpressionAssigns that occur in stmt....
    flip mapM [lhs | (F.StExpressionAssign _ _ lhs _)
                      <- universe stmnt :: [F.Statement (FA.Analysis A)]]
    -- ... apply the following:
      (\lhs -> do
         --let label = fromJustMsg "getting label of block in inference" (FA.insLabel ann)
         --let ivs = S.toList $ fromMaybe S.empty (IM.lookup label ivmap)
         ivmap <- get
         case isArraySubscript lhs of
           Just subs ->
             -- Left-hand side is a subscript-by relative index or by a range
             case neighbourIndex ivmap subs of
               Just lhs -> genSpecsAndReport mode span lhs [b]
               Nothing  -> if mode == EvalMode
                           then tell [(span , Right ("EVALMODE: LHS is an array \
                                              \subscript we can't handle \
                                              \(tag: LHSnotHandled)",""))]
                           else return ()
           -- Not an assign we are interested in
           _ -> return ())
    return b

perBlockInfer mode b@(F.BlDo ann span _ mDoSpec body) = do
    --let localIvs = getInductionVar mDoSpec
    -- introduce any induction variables into the induction variable state
    --DEL modify $ union localIvs
    --DEL ivs <- get
    -- DEL let label = fromJustMsg "getting label of block in inference" (FA.insLabel ann)
    -- DEL let ivs = fromMaybe S.empty (IM.lookup label ivmap)

    if (mode == DoMode || mode == CombinedMode) && isStencilDo b
     then genSpecsAndReport mode span [] body
     else return ()

    -- descend into the body of the do-statement
    mapM_ (descendBiM (perBlockInfer  mode)) body
    -- Remove any induction variable from the state
    --DEL modify $ (\\ localIvs)
    return b

perBlockInfer mode b = do
    -- Go inside child blocks
    mapM_ (descendBiM (perBlockInfer mode)) $ children b
    return b

genSpecifications :: (?flowsGraph :: FAD.FlowsGraph A)
  => FAD.InductionVarMapByASTBlock
  -> [Neighbour]
  -> [F.Block (FA.Analysis A)]
  -> Writer EvalLog [([Variable], Specification)]
genSpecifications ivs lhs blocks = do
    let subscripts = evalState (mapM (genSubscripts True) blocks) []
    varToMaybeSpecs <- ("subs# = " ++ show (length subscripts)) `trace` (sequence . map strength . mkSpecs $ subscripts)
    let varToSpecs = catMaybes . map strength $ varToMaybeSpecs
    case varToSpecs of
      [] -> do
         tell [("EVALMODE: Empty specification (tag: emptySpec)", "")]
         return []
      _ -> do
         let varsToSpecs = groupKeyBy varToSpecs
         return $ splitUpperAndLower varsToSpecs
    where
      mkSpecs = M.toList
              . M.mapWithKey (\v -> indicesToSpec ivs v lhs)
              . M.unionsWith (++)

      strength :: Monad m => (a, m b) -> m (a, b)
      strength (a, mb) = mb >>= (\b -> return (a, b))

      splitUpperAndLower = concatMap splitUpperAndLower'
      splitUpperAndLower' (vs, Specification (Left (Bound (Just l) (Just u)))) =
         [(vs, Specification (Left (Bound (Just l) Nothing))),
          (vs, Specification (Left (Bound Nothing (Just u))))]
      splitUpperAndLower' x = [x]

-- Generate all subscripting expressions (that are translations on
-- induction variables) that flow to this block
-- The State monad provides a list of the visited nodes so far
genSubscripts :: (?flowsGraph :: FAD.FlowsGraph A)
  => Bool
  -> F.Block (FA.Analysis A)
  -> State [Int] (M.Map Variable [[F.Index (FA.Analysis A)]])
genSubscripts False (F.BlStatement _ _ _ (F.StExpressionAssign _ _ e _))
    | isArraySubscript e /= Nothing
    -- Don't pull dependencies through arrays
    = return M.empty

genSubscripts top block = do
    visited <- get
    case (FA.insLabel $ F.getAnnotation block) of

      Just node ->
       ("in node = " ++ show node) `trace`
        if node `elem` visited
        -- This dependency has already been visited during this traversal
        then return $ M.empty
        -- Fresh dependency
        else do
          put $ node : visited
          let blocksFlowingIn = mapMaybe (lab ?flowsGraph) $ pre ?flowsGraph node
          dependencies <- mapM (genSubscripts False) blocksFlowingIn
          --DEL ((show (map (FA.insLabel . F.getAnnotation) blocksFlowingIn)) ++
          --   ("\n dep# = " ++ (show $ map (M.map (map (map (ixToNeighbour ivs)))) dependencies)))
          --   `trace`
          return $ M.unionsWith (++) (genRHSsubscripts block : dependencies)

      Nothing -> error $ "Missing a label for: " ++ show block

-- Get all RHS subscript which are translated induction variables
-- return as a map from (program) variables to a list of relative indices and
-- a flag marking whether there are any duplicate indices
genRHSsubscripts ::
     F.Block (FA.Analysis A)
  -> M.Map Variable [[F.Index (FA.Analysis A)]]
genRHSsubscripts b =
    collect [ (show e) `trace` (FA.varName exp, e)
      | F.ExpSubscript _ _ exp subs <- FA.rhsExprs b
      , isVariableExpr exp
      , let e = F.aStrip subs
      , not (null e)]

getInductionVar :: Maybe (F.DoSpecification (FA.Analysis A)) -> [Variable]
getInductionVar (Just (F.DoSpecification _ _ (F.StExpressionAssign _ _ e _) _ _))
  | isVariableExpr e = [FA.varName e]
getInductionVar _ = []

isStencilDo :: F.Block (FA.Analysis A) -> Bool
isStencilDo b@(F.BlDo _ span _ mDoSpec body) =
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
                 show (case offsets of [] -> 0
                                       _  -> length (head offsets)), a)]


        let spec = relativeIxsToSpec offsets
        return $ fmap (setLinearity (fromBool mult)) spec
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
      -- RHS is a range, map it to constant
      relativiseBy v i (Neighbour "" j)         = Constant (F.ValInteger "")
      relativiseBy _ _ x = x

-- Check that induction variables are used consistently
consistentIVSuse :: [Neighbour] -> [[Neighbour]] -> Bool
consistentIVSuse lhs [] = True
consistentIVSuse lhs rhses =
  consistentRHS /= Nothing && (all consistentWithLHS (fromJust consistentRHS))
    where
      cmp (Neighbour v i) (Neighbour v' _) | v == v' = Just $ Neighbour v i
      -- Cases for constants or non neighbour indices
      cmp (Neighbour {})  _              = Nothing
      cmp _ (Neighbour {})               = Nothing
      cmp _ _                            = Just $ Constant (F.ValInteger "")
      consistentRHS = foldrM (\a b -> mapM (uncurry cmp) $ zip a b) (head rhses) (tail rhses)
      -- If there is an induction variable on the RHS, then it also occurs on
      -- the LHS
      consistentWithLHS :: Neighbour -> Bool
      consistentWithLHS (Neighbour rv _) = any (matchesIV rv) lhs
      consistentWithLHS _                = True

      matchesIV :: Variable -> Neighbour -> Bool
      matchesIV v (Neighbour v' _) | v == v' = True
      -- All RHS to contain index ranges
      matchesIV v (Neighbour v' _) | v == "" = True
      matchesIV v (Neighbour v' _) | v' == "" = True
      matchesIV _ _                          = False

-- Convert list of relative offsets to a spec
relativeIxsToSpec :: [[Int]] -> Maybe Specification
relativeIxsToSpec ixs =
    if isEmpty exactSpec then Nothing else Just exactSpec
    where exactSpec = inferFromIndicesWithoutLinearity . fromLists $ ixs

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
ixToNeighbour ivmap f = (show (ixsspan f) ++ " ivslist = " ++ show ivsList ++ " ins = " ++ show insl) `trace` ixToNeighbour' ivsList f
  where
    --ivsList = indices . FA.prevAnnotation . F.getAnnotation $ f
    insl = FA.insLabel . F.getAnnotation $ f
    ivsList = S.toList $ fromMaybe S.empty $
                IM.lookup (fromJustMsg (show (ixsspan f) ++ " get IVs associated to labelled index " ++ show insl) (FA.insLabel (F.getAnnotation f))) ivmap

ixToNeighbour' ivs (F.IxRange _ _ Nothing Nothing Nothing)     = Neighbour "" 0
ixToNeighbour' ivs (F.IxRange _ _ Nothing Nothing
                  (Just (F.ExpValue _ _ (F.ValInteger "1")))) = Neighbour "" 0

ixToNeighbour' ivs (F.IxSingle _ _ _ exp)  = expToNeighbour ivs exp
ixToNeighbour' _ _ = NonNeighbour -- indexing expression is a range

-- Given a list of induction variables and an expression, compute its
-- Neighbour representation
expToNeighbour :: forall a. Data a
            => [Variable] -> F.Expression (FA.Analysis a) -> Neighbour

expToNeighbour ivs e@(F.ExpValue _ _ v@(F.ValVariable _))
    | FA.varName e `elem` ivs = Neighbour (FA.varName e) 0
    | otherwise               = ("BLEKC : " ++ show (FA.varName e) ++ " ivs = " ++ show ivs) `trace` Constant (fmap (const ()) v)

expToNeighbour ivs (F.ExpValue _ _ val) = Constant (fmap (const ()) val)

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

expToNeighbour ivs e = Constant (F.ValInteger "0")

--------------------------------------------------

-- Helper predicates
isUnaryOrBinaryExpr :: F.Expression a -> Bool
isUnaryOrBinaryExpr (F.ExpUnary {})  = True
isUnaryOrBinaryExpr (F.ExpBinary {}) = True
isUnaryOrBinaryExpr _                = False

isVariableExpr :: F.Expression a -> Bool
isVariableExpr (F.ExpValue _ _ (F.ValVariable _)) = True
isVariableExpr _                                  = False

-- Although type analysis isn't necessary anymore (Forpar does it
-- internally) I'm going to leave this infrastructure in-place in case
-- it might be useful later.
type TypeEnv a = M.Map FAT.TypeScope (M.Map String FA.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FA.idCType idty
  return $ cty == FA.CTArray

-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
