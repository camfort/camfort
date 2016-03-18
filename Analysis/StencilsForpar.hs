{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams, TupleSections #-}

module Analysis.StencilsForpar where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Analysis.Loops (collect)
import Analysis.Annotations
import Extensions.UnitsForpar (parameterise)

import qualified Forpar.AST as F
import qualified Forpar.Analysis as FA
import qualified Forpar.Analysis.Types as FAT
import qualified Forpar.Analysis.Renaming as FAR
import qualified Forpar.Util.Position as FU

import qualified Data.Map as Map
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.Ord

import Debug.Trace

--------------------------------------------------
-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f" [] () ()

-- Infer and check stencil specifications
infer :: F.ProgramFile Annotation -> String
infer = specInference . FAR.renameAndStrip . FAR.analyseRenames . FA.initAnalysis

check :: Program a -> Program a
check = error "Not yet implemented"

--------------------------------------------------

type LogLine = (FU.SrcSpan, [([Variable], [Spec])])
formatSpec :: FAR.NameMap -> LogLine -> String
formatSpec nm (span, []) = ""
formatSpec nm (span, specs) = loc ++ " \t" ++ (commaSep . nub . map doSpec $ specs) ++ "\n"
  where
    loc                      = show (spanLineCol span)
    commaSep                 = concat . intersperse ", "
    doSpec (arrayVar, spec)  = commaSep (map realName arrayVar) ++ ": " ++ showL (map fixSpec spec)
    realName v               = v `fromMaybe` (v `M.lookup` nm)
    fixSpec (TemporalFwd vs) = TemporalFwd $ map realName vs
    fixSpec (TemporalBwd vs) = TemporalBwd $ map realName vs
    fixSpec s                = s

--------------------------------------------------

-- The inferer works within this monad
type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State [Variable]))
runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT

--------------------------------------------------

-- Provides the main inference procedure for specifications which is
-- currently at the level of per statement for spatial stencils, and
-- per for-loop for temporal stencils
specInference :: (F.ProgramFile A, FAR.NameMap) -> String
specInference (pf, nm) = formatSpec nm =<< logs
  where tenv = FAT.inferTypes pf
        logs = specInference' tenv =<< flowAnalysisArrays pf

specInference' :: TypeEnv A -> (F.ProgramUnit A, FlowsMap) -> [LogLine]
specInference' tenv (pu, flMap) = runInferer cycles (F.getName pu) tenv (descendBiM perBlocks pu)
  where cycles = cyclicDependents flMap

--------------------------------------------------

-- Because loop bodies are not nested (yet), we need to look for the
-- beginning of lists (use descendBiM!!!) and scan over them.
perBlocks :: [F.Block A] -> Inferer [F.Block A]
perBlocks bs = iterateMaybe_ blockLoop bs >> return bs

-- Chomp through the list of blocks until we run out of blocks
blockLoop :: [F.Block A] -> Inferer (Maybe [F.Block A])

-- Match any assignment statements
blockLoop (b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ _ rhs)):bs) = do
  (_, puName, tenv) <- ask
  -- Get array indexing (on the RHS)
  let rhsExprs = universeBi rhs :: [F.Expression A]
  let arrayAccesses = collect [
          (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ v)) subs <- rhsExprs
                 , let e = F.aStrip subs
                 , not (null e)
        ]

  -- Create specification information
  ivs <- get
  let specs = groupKeyBy . M.toList . fmap (ixCollectionToSpec ivs) $ arrayAccesses
  tell $ [(span, specs)] -- add to report
  return $ Just bs

-- Match a Do-loop, and chomp the entire body by finding the "continue" statement
blockLoop (b@(F.BlStatement _ span _
               (F.StDo _ _ label (doSpec@F.DoSpecification {}))):bs) = do
  let F.DoSpecification _ _ (
          F.StExpressionAssign _ _ (F.ExpValue _ _ (F.ValVariable _ v)) _
        ) _ _ = doSpec
  modify $ union [v] -- introduce v into the list of induction variables
  ivs <- get
  (cycles, _, _) <- ask

  -- use label to search for end of loop and return list of blocks inside of loop
  let (body, bs') = break ((`labelEq` Just label) . F.getLabel) bs

  -- Insert temporal specs for anything inside the Do-loop
  let lexps = FA.lhsExprs =<< body

  let getTimeSpec e = do
        lhsV <- case e of F.ExpValue _ _ (F.ValVariable _ lhsV)                     -> Just lhsV
                          F.ExpValue _ _ (F.ValArray _ lhsV)                        -> Just lhsV
                          F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ lhsV)) _ -> Just lhsV
                          _                                                         -> Nothing
        v'   <- lookup lhsV cycles
        return ([lhsV], [TemporalBwd [v']])

  let tempSpecs = foldl' (\ ts -> maybe ts (:ts) . getTimeSpec) [] lexps

  tell $ [(span, tempSpecs)]

  perBlocks body                -- process loop body

  return $ Just bs'             -- return post-loop blocks

blockLoop (b:bs) = return $ Just bs
blockLoop []     = return Nothing

--------------------------------------------------

{- *** 1 . Specification syntax -}

type Dimension  = Int -- spatial dimensions are 0 indexed
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

data Spec where
     Reflexive   :: [Dimension] -> Spec
     Forward     :: Depth -> [Dimension] -> Spec
     Backward    :: Depth -> [Dimension] -> Spec
     Symmetric   :: Depth -> [Dimension] -> Spec
     -- Product of two specs (takes the intersection of their models)
     Product     :: [Spec] -> Spec

     -- Temporal specifications, with a list of variables for the arrays
     -- through which time is represented
     TemporalFwd    :: [Variable] -> Spec
     TemporalBwd    :: [Variable] -> Spec

     Unspecified :: [Dimension] -> Spec
     Constant    :: [Dimension] -> Spec
     Linear      :: Spec -> Spec

deriving instance Eq Spec

instance Ord Direction where
         Fwd <= Bwd = True
         Fwd <= Fwd = True
         Bwd <= Bwd = True
         Bwd <= Fwd = False

-- Syntax
showL :: Show a => [a] -> String
showL = concat . intersperse "," . map show
instance Show Spec where
     show (Reflexive dims)     = "reflexive, dims=" ++ showL dims
     show (Forward dep dims)   = "forward, depth="  ++ show dep ++ ", dim=" ++ showL dims
     show (Backward dep dims)  = "backward, depth=" ++ show dep ++ ", dim=" ++ showL dims
     show (Symmetric dep dims) = "centered, depth=" ++ show dep ++ ", dim=" ++ showL dims
     show (Unspecified dims)   = "unspecified "  ++ showL dims
     show (Constant dims)      = "fixed, dim=" ++ showL dims
     show (Product specs)      = concat $ intersperse " & " $ map (\spec -> "(" ++ show spec ++ ")") specs
     show (Linear spec)        = (show spec) ++ ", unique "
     show (TemporalFwd dims)   = "forward, depth=" ++ show (length dims) ++ ", dim=t{" ++ showL dims ++ "}"
     show (TemporalBwd dims)   = "backward, depth=" ++ show (length dims) ++ ", dim=t{" ++ showL dims ++ "}"

{- *** 2 . Operations on specs, and conversion from indexing expressions -}

-- Convert list of indexing expressions to list of specs
ixCollectionToSpec :: [Variable] -> [[F.Expression A]] -> [Spec]
ixCollectionToSpec ivs es = specIsToSpecs x
  where x = normalise . ixExprAToSpecIs ivs $ es

-- Simplifies lists specifications based on the 'specPlus' operation:
simplify :: [Spec] -> [Spec]
simplify = foldPair specPlus
-- Combine specs
specPlus :: Spec -> Spec -> Maybe Spec
specPlus (Reflexive dims) (Reflexive dims')                        = Just (Reflexive (dims ++ dims'))
specPlus (Forward dep dims) (Forward dep' dims')     | dep == dep' = Just (Forward dep (dims ++ dims'))
specPlus (Backward dep dims) (Backward dep' dims')   | dep == dep' = Just (Backward dep (dims ++ dims'))
specPlus (Symmetric dep dims) (Symmetric dep' dims') | dep == dep' = Just (Symmetric dep (dims ++ dims'))
specPlus (Unspecified dims) (Unspecified dims')                    = Just (Unspecified (dims ++ dims'))
specPlus x y                                                       = Nothing

{- *** 3 . Intermediate representation 'SpecI' between indexing expressions and speccs -}

-- SpecIification (intermediate) elements
data SpecI where
     -- Regular spatial spans
     Span        :: Dimension -> Depth -> Direction -> Saturation -> SpecI
     -- Reflexive access
     Reflx       :: Dimension -> SpecI
     -- Constant access
     Const       :: Dimension -> SpecI
deriving instance Show SpecI

depth :: SpecI -> Int
depth (Span _ depth _ _) = depth
depth x = 0

dim :: SpecI -> Dimension
dim (Span dim _ _ _) = dim
dim (Const dim)      = dim
dim (Reflx dim)      = dim

direction :: SpecI -> Direction
direction (Span _ _ dir _) = dir
direction x                = Fwd

{-
 This provides a representation for index ranges along with
 a normalisation function that coalesces contiguous ranges.

 Any non-reflexive index is converted to a (list of) spans
  e.g. a(i - 1, j + 1) -> [Span 0 1 Bwd False, Span 0 1 Fwd False]

 the 'normalise' function then turns a list of these spans
 into a list of list of spans (per dimension/direction)

-}

-- Ordering
deriving instance Eq SpecI
instance Ord SpecI where
  s1 <= s2 | (dim s1) < (dim s2)  = True
  s1 <= s2 | (dim s1) > (dim s2)  = False
  s1 <= s2 | (dim s1) == (dim s2) =
     case (s1, s2) of
       (Reflx _, _) -> True
       (Const _, _) -> True
       (Span dim depth dir s, Span dim' depth' dir' s')
         | (dim == dim') && (dir == dir') -> depth <= depth'
         | (dim == dim')                  -> dir <= dir'
       (_, _)       -> False

-- Types various normal forms of specifications and specification groups
data Normalised a where
  -- Two specifications belonging to the same dimension which are not duplicates
  NS :: SpecI -> SpecI -> Normalised (SpecI, SpecI)
  -- A list of specifications all of the same dimension
  NSpecIs :: [SpecI]   -> Normalised [SpecI]
  -- Grouped lists of specifications in normal form (maximally coalesced), grouped by dimension
  NSpecIGroups :: [[SpecI]] -> Normalised [[SpecI]]

deriving instance Show (Normalised a)

-- Normalise a list of spans
normalise :: [SpecI] -> Normalised [[SpecI]]
normalise = coalesce . firstAsSaturated . groupByDim

-- Takes lists of specs belonging to the same dimension/direction and coalesces contiguous regions
coalesce :: [Normalised [SpecI]] -> Normalised [[SpecI]]
coalesce = NSpecIGroups . map (\ (NSpecIs specs) -> foldPair (\ x y -> plus (NS x y)) specs)

groupByDim :: [SpecI] -> [Normalised [SpecI]]
groupByDim = map (NSpecIs . nub) . groupBy ((==) `on` dim) . sort

-- Mark spans from 0 to 1 as saturated.
firstAsSaturated :: [Normalised [SpecI]] -> [Normalised [SpecI]]
firstAsSaturated []               = []
firstAsSaturated ((NSpecIs s):xs) = NSpecIs (map go s) : firstAsSaturated xs
  where go (Span dim 1 dir sat) = Span dim 1 dir True
        go s                    = s

-- Coalesces two contiguous specifications (of the same dimension and direction)
--  This is a partial operation and fails when the two specs are not contiguous
plus :: Normalised (SpecI, SpecI) -> Maybe SpecI
plus (NS (Span _ d1 dir s1) (Span dim d2 dir' s2))
  | dir == dir'                                      =
    if d2 == d1 + 1 then -- SpecIs are one apart
       if s1 || s2 then    -- At least one is marked as saturated
           Just (Span dim d2 dir True) -- Grow the saturated area
       else
           Nothing         -- Neither span is satured so mark both as needed
    else -- d2 >= d1 by Normalised -- SpecIs are more than one apart
       if s2 then -- if the greater span is saturated, then it subsumes the smaller
           Just (Span dim d2 dir True)
       else
           Nothing
plus (NS (Const dim1) (Const dim2))                  = Just $ Const dim1 -- assumes Normalised premise
plus (NS s@(Span _ d1 dir s1) (Span dim d2 dir' s2)) = Nothing
plus (NS (Reflx d) (Reflx _))                        = Just $ Reflx d
plus (NS s@(Span _ d1 dir s1) (Reflx _))             = Just $ s
plus (NS  (Reflx _) s@(Span _ d1 dir s1))            = Just $ s
plus _ = error "Trying to coalesce a reflexive and a span"

-- Convert a normalised list of index specifications to a list of specifications
specIsToSpecs :: Normalised [[SpecI]] -> [Spec]
specIsToSpecs x@(NSpecIGroups spanss) = simplify (uncurry go =<< zip [1..] spanss)
  where
    go :: Dimension -> [SpecI] -> [Spec]
    go dim (Reflx _ : xs)            = Reflexive [dim] : go dim xs
    go dim (Const _ : xs)            = Constant [dim] : go dim xs
    go dim [Span _ d Fwd True, Span _ d' Bwd True]
      | d == d'                      = [Symmetric d [dim]]
      | d > d'                       = [Symmetric (abs (d - d')) [dim], Forward d [dim]]
      | otherwise                    = [Symmetric (abs (d - d')) [dim], Backward d' [dim]]
    go dim (Span _ d Fwd True : xs)  = Forward d [dim] : go dim xs
    go dim (Span _ d Bwd True : xs)  = Backward d [dim] : go dim xs
    go dim xs                        = []

    isReflexiveMultiDim :: Normalised [[SpecI]] -> Bool
    isReflexiveMultiDim (NSpecIGroups spanss) = flip all spanss $ \ spans ->
      case spans of (Reflx _) :_  -> True
                    _             -> False

-- From a list of index expressions (themselves a list of expressions)
--  to a set of intermediate specs
ixExprAToSpecIs :: [Variable] -> [[F.Expression A]] -> [SpecI]
ixExprAToSpecIs ivs = concatMap (fromMaybe [] . zipWithM (ixCompExprToSpecI ivs) [0..])

-- Convert a single index expression for a particular dimension to intermediate spec
-- e.g., for the expression a(i+1,j+1) then this function gets
-- passed dim = 0, expr = i + 1 and dim = 1, expr = j + 1
ixCompExprToSpecI :: [Variable] -> Dimension -> F.Expression A -> Maybe SpecI
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValVariable _ v))
  | v `elem` ivs = Just $ Reflx d
  | otherwise    = Just $ Const d
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValVariable _ v))
                                                    (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValInteger offs))
                                                    (F.ExpValue _ _ (F.ValVariable _ v)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Subtraction (F.ExpValue _ _ (F.ValVariable _ v))
                                                       (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d x (if x < 0 then Fwd else Bwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
ixCompExprToSpecI ivs d _ = Nothing

--------------------------------------------------

{- *** 4. Flows-to analysis -}

type Flows = ReaderT FlowsMapTable (State FlowsMap) -- Monad

runFlows :: FlowsMapTable -> Flows a -> (a, FlowsMap)
runFlows fmt = flip runState M.empty . flip runReaderT fmt

-- FlowsMap structure:
-- -- e.g. (v, [a, b]) means that 'a' and 'b' flow to 'v'
type FlowsMap = M.Map Variable [Variable]
type FlowsMapTable = M.Map F.ProgramUnitName (F.ProgramUnit A, FlowsMap)
type Cycles = [(Variable, Variable)]

flowAnalysisArrays :: F.ProgramFile A -> [(F.ProgramUnit A, FlowsMap)]
flowAnalysisArrays pf = M.elems fmt
  where
    F.ProgramFile cm_pus _ = parameterise pf -- identify function/subroutine parameters
    (fmt, _)               = runFlows fmt flowMaker -- intentionally recursive
    flowMaker              = (M.fromList `fmap`) . forM cm_pus $ \ (_, pu) -> do
                               put M.empty
                               res <- flowAnalysisArraysRecur pu
                               return (F.getName pu, res)

flowAnalysisArraysRecur :: F.ProgramUnit A -> Flows (F.ProgramUnit A, FlowsMap)
flowAnalysisArraysRecur p = do
  flowMap  <- get
  p'       <- flowAnalysisArraysStep p
  flowMap' <- get
  if flowMap == flowMap'
    then return (p', flowMap')
    else flowAnalysisArraysRecur p'

flowAnalysisArraysStep :: F.ProgramUnit A -> Flows (F.ProgramUnit A)
flowAnalysisArraysStep pu = transformBiM perBlock pu
  where
    -- FIXME: do function call expression. This is subtle because
    -- function calls can appear as sub-expressions to assignment
    -- statements. Function calls can change the meaning of assignment
    -- statements too. For example, consider this:
    --
    -- a = f(a, b)
    -- ...
    -- function f(a, b)
    --   f = a
    -- end
    --
    -- A naive flows analysis would conclude that ("a", ["a","b"]) but
    -- interprocedural analysis should uncover that ("a", ["a"]) only.
    --
    -- Further question: what should be the correct flows-map output
    -- of "a(i) = a(b(i))", where a and b are arrays?

    perBlock :: F.Block A -> Flows (F.Block A)
    perBlock = transformBiM perStmt

    lookupList :: Variable -> FlowsMap -> [Variable]
    lookupList v = fromMaybe [] . Map.lookup v

    perStmt :: F.Statement A -> Flows (F.Statement A)
    perStmt f@(F.StExpressionAssign _ _ lhs rhs) = do
      flowMap <- get

      -- Using the parameterisation analysis, rename function &
      -- subroutine parameters to this schema so that they may be
      -- substituted later by actual arguments.
      let p (F.ValArray (A { unitInfo = Just (Parametric (fn, n)) }) _) = fn ++ "[" ++ show n ++ "]"
          p (F.ValArray _ v) = v
      let lhses = [ p v | (F.ExpValue _ _ v@(F.ValArray _ _)) <- universeBi lhs :: [F.Expression A] ]
      let rhses = [ p v | (F.ExpValue _ _ v@(F.ValArray _ _)) <- universeBi rhs :: [F.Expression A] ]

      let pullInFlowsFromRight lhsV map rhsV = M.insertWith union lhsV (lookupList rhsV map) map

      let fromRightToLeft map lhsV = foldl' (pullInFlowsFromRight lhsV)
                                            (M.insertWith union lhsV rhses map)
                                            rhses

      put $ foldl' fromRightToLeft flowMap lhses

      return f

    perStmt f@(F.StCall _ _ (F.ExpValue _ _ (F.ValSubroutineName sn)) (Just argAList)) = do
      let args = F.aStrip argAList
      fmt <- ask

      -- lazily look-up flows analysis of subroutine
      let subFlMap  = maybe M.empty snd $ M.lookup (F.Named sn) fmt

      let doArg n (F.ExpValue _ _ (F.ValVariable _ v)) = [(sn ++ "[" ++ show n ++ "]", v)]
          doArg n (F.ExpValue _ _ (F.ValArray _ v))    = [(sn ++ "[" ++ show n ++ "]", v)]
          doArg _ _                                    = []
      -- assemble necessary substitutions
      let argSubs   = concat $ zipWith doArg [1..] args

      -- apply
      let subFlMap' = transformBi (\ s -> s `fromMaybe` lookup s argSubs) subFlMap

      -- combine with other mappings
      modify $ M.union subFlMap'

      return f
    perStmt f = return f

--------------------------------------------------

-- Find all array accesses which have a cyclic dependency
cyclicDependents :: FlowsMap -> Cycles
cyclicDependents flmap = filter (uncurry (/=)) reflSubset
  where
    self           = flmap `composeRelW` flmap
    reflSubset     = foldl' frob [] (M.assocs self)
    frob p (k, ks) = maybe p ((:p) . (k,)) $ k `lookup` map swap ks

-- Inverts a relation (represented as a map)
--invertRel :: Ord v => Map.Map k [v] -> Map.Map v [k]
--invertRel m = foldl' (\m (k, vs) -> foldl' (\m' v -> Map.insertWith (++) v [k] m') m vs) Map.empty (Map.assocs m)

-- Compose two relations with a witness of where the 'join' point in
-- the middle is.
-- e.g., for two relations R and S, if (a R b) and (b S c) then (a R.S (b, c))
composeRelW :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k] -> M.Map k [(v, k)]
composeRelW r s = foldl' frob1 M.empty (M.assocs r)
  where
    frob1 rs (k, vs) = foldl' (frob2 k) rs vs
    frob2 k rs' v    = fromMaybe rs' $ do
                         k' <- M.lookup v s
                         return $ M.insertWith (++) k (map (v,) k') rs'

--------------------------------------------------

-- Iterate on action, supplying its return value back to itself, until
-- the action results in Nothing.
iterateMaybe_ :: Monad m => (a -> m (Maybe a)) -> a -> m ()
iterateMaybe_ f x = f x >>= return () `maybe` iterateMaybe_ f

labelEq (Just (F.ExpValue _ _ (F.ValLabel l1))) (Just (F.ExpValue _ _ (F.ValLabel l2))) = l1 == l2
labelEq _ _ = False

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)

-- Helper function, reduces a list two elements at a time with a partial operation
foldPair :: (a -> a -> Maybe a) -> [a] -> [a]
foldPair f [] = []
foldPair f [a] = [a]
foldPair f (a:(b:xs)) = case f a b of
                          Nothing -> a : (foldPair f (b : xs))
                          Just c  -> foldPair f (c : xs)

groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy = groupKeyBy' . map (\ (k, v) -> ([k], v))

groupKeyBy' []                         = []
groupKeyBy' [(ks, v)]                  = [(ks, v)]
groupKeyBy' ((ks1, v1):((ks2, v2):xs))
  | v1 == v2                           = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
  | otherwise                          = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)

-- Although type analysis isn't necessary anymore (Forpar does it
-- internally) I'm going to leave this infrastructure in-place in case
-- it might be useful later.
type TypeEnv a = M.Map FAT.TypeScope (M.Map String FAT.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FAT.idCType idty
  return $ cty == FAT.CTArray
