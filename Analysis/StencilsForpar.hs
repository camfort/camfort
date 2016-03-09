{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts, ImplicitParams, TupleSections #-}

module Analysis.StencilsForpar where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Loops (unfoldrM)

import Analysis.Loops (collect)
import Analysis.Annotations (Annotation, unitAnnotation)

import qualified Forpar.AST as F
import qualified Forpar.Analysis as FA
import qualified Forpar.Analysis.Types as FAT
import qualified Forpar.Util.Position as FU

import qualified Data.Map as Map
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Debug.Trace

type A = Annotation

--------------------------------------------------

-- Infer and check stencil specifications
infer :: F.ProgramFile a -> String
infer = specInference . fmap (const unitAnnotation)

check :: Program a -> Program Annotation
check = error "Not yet implemented"

-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f" [] () ()

-- Provides the main inference procedure for specifications which is
-- currently at the level of per statement for spatial stencils, and
-- per for-loop for temporal stencils
specInference :: F.ProgramFile Annotation -> String
specInference pf = formatSpec =<< logs
  where tenv = FAT.inferTypes pf
        logs = specInference' tenv =<< flowAnalysisArrays pf

--------------------------------------------------

type LogLine = (FU.SrcSpan, [([Variable], [Spec])])
formatSpec :: LogLine -> String
formatSpec (span, []) = ""
formatSpec (span, specs) =
  show (spanLineCol span) ++ " \t" ++
  (concat . intersperse ", " . nub . map (\ (arrayVar, spec) -> (concat $ intersperse "," arrayVar) ++ ": " ++ showL spec) $ specs) ++ "\n"

type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State [Variable]))
runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT

specInference' :: TypeEnv A -> (F.ProgramUnit A, FlowsMap) -> [LogLine]
specInference' tenv (pu, flMap) = runInferer cycles (F.getName pu) tenv (descendBiM perBlocks pu)
  where cycles = cyclicDependents flMap

--------------------------------------------------

-- Because loop bodies are not nested (yet), we need to look for the
-- beginning of lists (use descendBiM!!!) and scan over them.
perBlocks :: [F.Block A] -> Inferer [F.Block A]
perBlocks bs = unfoldrM_ blockLoop bs >> return bs

-- Look at the first statement in the list and use the remaining statements if needed;
-- return the first statement again.
blockLoop :: [F.Block A] -> Inferer (Maybe [F.Block A])
-- Match any assignment statements
blockLoop (b@(F.BlStatement _ span _
               (F.StExpressionAssign _ _ _ rhs)):bs) = do
  (_, puName, tenv) <- ask
  -- Get array indexing (on the RHS)
  let rhsExprs = universeBi rhs :: [F.Expression A]
  let arrayAccesses = collect [
        (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ v)) subs <- rhsExprs
               , let e = F.aStrip subs
               , not (null e)
               , isArrayType tenv puName v
        ]

  -- Create specification information
  ivs <- get
  let specs = groupKeyBy $ M.toList $ fmap (ixCollectionToSpec ivs) $ arrayAccesses
  tell $ [(span, specs)] -- add to report
  return $ Just bs

blockLoop (b@(F.BlStatement _ span _
               (F.StDo _ _ label (doSpec@F.DoSpecification {}))):bs) = do
  let F.DoSpecification _ _ (
          F.StExpressionAssign _ _ (F.ExpValue _ _ (F.ValVariable _ v)) _
        ) _ _ = doSpec
  modify $ union [v]
  ivs <- get
  (cycles, _, _) <- ask

  -- use label to search for end of loop and return list of blocks inside of loop
  let (body, bs') = break ((`labelEq` Just label) . F.getLabel) bs

  -- Insert temporal specs for anything inside the for-loop
  let lexps = FA.lhsExprs =<< body
  let tempSpecs = foldl' (\ ts e -> case e of
                           F.ExpValue _ _ (F.ValVariable _ lhsV) ->
                           -- Insert time specification if there is
                           -- a cyclic depenency through the
                           -- assignment (for arrays)
                             case (lookup lhsV cycles) of
                               Just v' -> ([lhsV], [TemporalBwd [v']]) : ts
                               Nothing -> ts
                           F.ExpValue _ _ (F.ValArray _ lhsV) ->
                           -- Insert time specification if there is
                           -- a cyclic depenency through the
                           -- assignment (for arrays)
                             case (lookup lhsV cycles) of
                               Just v' -> ([lhsV], [TemporalBwd [v']]) : ts
                               Nothing -> ts
                           F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValArray _ lhsV)) _ ->
                           -- Insert time specification if there is
                           -- a cyclic depenency through the
                           -- assignment (for arrays)
                             case (lookup lhsV cycles) of
                               Just v' -> ([lhsV], [TemporalBwd [v']]) : ts
                               Nothing -> ts
                           )
                         [] lexps
  tell $ [(span, tempSpecs)]
  descendBiM perBlocks body -- Descend inside for-loop
  return $ Just bs'

blockLoop (b:bs) = return $ Just bs
blockLoop [] = return Nothing

--------------------------------------------------

{- *** 1 . Specification syntax -}

type Dimension  = Int -- spatial dimensions are 0 indexed
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

data Spec where
     Reflexive   :: Spec
     Forward     :: Depth -> [Dimension] -> Spec
     Backward    :: Depth -> [Dimension] -> Spec
     Symmetric   :: Depth -> [Dimension] -> Spec

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
showL = concat . (intersperse ",") . (map show)
instance Show Spec where
     show Reflexive            = "reflexive"
     show (Forward dep dims)   = "forward depth=" ++ show dep ++ " dim=" ++ showL dims
     show (Backward dep dims)  = "backward depth=" ++ show dep ++ " dim=" ++ showL dims
     show (Symmetric dep dims) = "centered depth=" ++ show dep ++ " dim=" ++ showL dims
     show (Unspecified dims)   = "unspecified "  ++ showL dims
     show (Constant dims)      = "fixed dim=" ++ showL dims
     show (Linear spec)        = (show spec) ++ " unique "
     show (TemporalFwd dims)   = "forward depth=" ++ show (length dims) ++ " dim=t{" ++ showL dims ++ "}"
     show (TemporalBwd dims)   = "backward depth=" ++ show (length dims) ++ " dim=t{" ++ showL dims ++ "}"

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
specPlus Reflexive Reflexive                                       = Just Reflexive
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
              (Span dim depth dir s, Span dim' depth' dir' s') | (dim == dim') && (dir == dir') -> depth <= depth'
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
coalesce = NSpecIGroups . (map (\(NSpecIs specs) -> foldPair (\x y -> plus (NS x y)) $ specs))



groupByDim :: [SpecI] -> [Normalised [SpecI]]
groupByDim = (map (NSpecIs . nub)) . (groupBy eqDim) . sort
                where eqDim :: SpecI -> SpecI -> Bool
                      eqDim s1 s2 = (dim s1) == (dim s2)

-- Mark spans from 0 to 1 as saturated.
firstAsSaturated :: [Normalised [SpecI]] -> [Normalised [SpecI]]
firstAsSaturated [] = []
firstAsSaturated ((NSpecIs s):xs) = (NSpecIs $ map go s) : (firstAsSaturated xs)
                                     where go (Span dim 1 dir sat) = Span dim 1 dir True
                                           go s = s


-- Coalesces two contiguous specifications (of the same dimension and direction)
--  This is a partial operation and fails when the two specs are not contiguous
plus :: Normalised (SpecI, SpecI) -> Maybe SpecI
plus (NS (Span _ d1 dir s1) (Span dim d2 dir' s2)) | dir == dir' =
     if d2 == (d1 + 1) then -- SpecIs are one apart
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
specIsToSpecs x@(NSpecIGroups spanss) =
--   ("___" ++ show spanss ++ "\n") `trace`
   (if isReflexiveMultiDim x then [Reflexive] else [])
  ++ simplify (concatMap (uncurry go) (zip [0..length spanss] spanss))
        where go :: Dimension -> [SpecI] -> [Spec]
              go dim (Reflx _ : xs) = go dim xs
              go dim (Const _ : xs) = Constant [dim] : go dim xs
              go dim [Span _ d Fwd True, Span _ d' Bwd True] =
                           if d==d' then [Symmetric d [dim]]
                           else if d > d' then [Symmetric (abs (d-d')) [dim], Forward d [dim]]
                                          else [Symmetric (abs (d-d')) [dim], Backward d' [dim]]
              go dim ((Span _ d Fwd True) : xs) = Forward d [dim] : go dim xs
              go dim ((Span _ d Bwd True) : xs) = Backward d [dim] : go dim xs
              go dim xs = []

              isReflexiveMultiDim :: Normalised [[SpecI]] -> Bool
              isReflexiveMultiDim (NSpecIGroups spanss) = all (\spans -> (length spans > 0) &&
                                                                           (case (head spans) of (Reflx _) -> True
                                                                                                 _         -> False)) spanss

-- From a list of index expressions (themselves a list of expressions)
--  to a set of intermediate specs
ixExprAToSpecIs :: [Variable] -> [[F.Expression A]] -> [SpecI]
ixExprAToSpecIs ivs ess =
  concatMap (\es -> case (mapM (uncurry (ixCompExprToSpecI ivs)) (zip [0..(length es)] es)) of
                      Nothing -> []
                      Just es -> es) ess

{- TODO: need to check that any variable in an index expression that we are adding to
         the spec is actually an induction variable
         Going to need some state pushed in here... implicit parameters are fine to do this
                                                    can get this information from the
-}
isInductionVariable v = True

-- Convert a single index expression for a particular dimension to intermediate spec
-- e.g., for the expression a(i+1,j+1) then this function gets
-- passed dim = 0, expr = i + 1 and dim = 1, expr = j + 1
ixCompExprToSpecI :: [Variable] -> Dimension -> F.Expression A -> Maybe SpecI
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValVariable _ v))
  | v `elem` ivs = Just $ Reflx d
  | otherwise    = Just $ Const d
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValVariable _ v))
                                                      (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d (read offs) (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Addition (F.ExpValue _ _ (F.ValInteger offs))
                                                      (F.ExpValue _ _ (F.ValVariable _ v)))
  | v `elem` ivs = Just $ Span d (read offs) (if x < 0 then Bwd else Fwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpBinary _ _ F.Subtraction (F.ExpValue _ _ (F.ValVariable _ v))
                                                         (F.ExpValue _ _ (F.ValInteger offs)))
  | v `elem` ivs = Just $ Span d (read offs) (if x < 0 then Fwd else Bwd) False
  where x = read offs
ixCompExprToSpecI ivs d (F.ExpValue _ _ (F.ValInteger _)) = Just $ Const d
ixCompExprToSpecI ivs d _ = Nothing


-- Helper function, reduces a list two elements at a time with a partial operation
foldPair :: (a -> a -> Maybe a) -> [a] -> [a]
foldPair f [] = []
foldPair f [a] = [a]
foldPair f (a:(b:xs)) = case f a b of
                          Nothing -> a : (foldPair f (b : xs))
                          Just c  -> foldPair f (c : xs)

groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy xs = groupKeyBy' (map (\(k, v) -> ([k], v)) xs)

groupKeyBy' []                                    = []
groupKeyBy' [(ks, v)]                             = [(ks, v)]
groupKeyBy' ((ks1, v1):((ks2, v2):xs)) | v1 == v2 = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
                                       | otherwise = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)

{- *** 4. Flows-to analysis -}

type TypeEnv a = M.Map FAT.TypeScope (M.Map String FAT.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FAT.idCType idty
  return $ cty == FAT.CTArray

type Flows = ReaderT (TypeEnv A) (State FlowsMap) -- Monad

runFlows :: TypeEnv A -> Flows a -> (a, FlowsMap)
runFlows tenv = flip runState M.empty . flip runReaderT tenv

-- FlowsMap structure:
-- -- e.g. (v, [a, b]) means that 'a' and 'b' flow to 'v'
type FlowsMap = Map.Map Variable [Variable]
type Cycles = [(Variable, Variable)]

flowAnalysisArrays :: F.ProgramFile A -> [(F.ProgramUnit A, FlowsMap)]
flowAnalysisArrays pf@(F.ProgramFile cm_pus _) = fst . runFlows tenv $ do
  flip mapM cm_pus $ \ (_, pu) -> flowAnalysisArraysRecur pu Map.empty
  where tenv = FAT.inferTypes pf

flowAnalysisArraysRecur :: F.ProgramUnit A -> FlowsMap -> Flows (F.ProgramUnit A, FlowsMap)
flowAnalysisArraysRecur p flowMap = do
  flowMap  <- get
  p'       <- flowAnalysisArraysStep p
  flowMap' <- get
  if flowMap == flowMap'
    then return (p', flowMap')
    else flowAnalysisArraysRecur p' flowMap'

flowAnalysisArraysStep :: F.ProgramUnit A -> Flows (F.ProgramUnit A)
flowAnalysisArraysStep pu = transformBiM perBlock pu
  where
    perBlock :: F.Block A -> Flows (F.Block A)
    perBlock = transformBiM perStmt

    lookupList :: Variable -> FlowsMap -> [Variable]
    lookupList v = fromMaybe [] . Map.lookup v

    perStmt :: F.Statement A -> Flows (F.Statement A)
    perStmt f@(F.StExpressionAssign _ _ lhs rhs) = do
      tenv <- ask
      flowMap <- get
      let lhses = [ v | (F.ExpValue _ _ (F.ValArray _ v)) <- universeBi lhs :: [F.Expression A]
                                                          ,  isArrayType tenv (F.getName pu) v ]
      let rhses = [ v | (F.ExpValue _ _ (F.ValArray _ v)) <- universeBi rhs :: [F.Expression A]
                                                          ,  isArrayType tenv (F.getName pu) v ]
      let pullInFlowsFromRight lhsV map rhsV =
            M.insertWith (\ a b -> nub (a++b))
                         lhsV
                         (lookupList rhsV map)
                         map
      let fromRightToLeft map lhsV =
            foldl' (pullInFlowsFromRight lhsV)
                   (Map.insertWith (++) lhsV rhses map)
                   rhses

      put $ foldl' fromRightToLeft flowMap lhses
      return f
    perStmt f = return f

-- Find all array accesses which have a cyclic dependency
cyclicDependents :: FlowsMap -> Cycles
cyclicDependents flmap = filter (\(u, v) -> not (u == v)) (reflSubset)
  where
    self = flmap `composeRelW` flmap
    reflSubset = foldl' (\ p (k, ks) -> case lookup k (map (\(a, b) -> (b, a)) ks) of
                                          Nothing -> p
                                          Just v  -> (k, v) : p)
                        [] (M.assocs self)

-- Inverts a relation (represented as a map)
--invertRel :: Ord v => Map.Map k [v] -> Map.Map v [k]
--invertRel m = foldl (\m (k, vs) -> foldl (\m' v -> Map.insertWith (++) v [k] m') m vs) Map.empty (Map.assocs m)

-- compose two relations with a witness of where the 'join' point in the middle is
-- e.g., for two relations R and S, if (a R b) and (b S c) then (a R.S (b, c))
composeRelW :: (Ord k, Ord v) => Map.Map k [v] -> Map.Map v [k] -> Map.Map k [(v, k)]
composeRelW r s = foldl' (\rs (k, vs) -> foldl' (\rs' v -> case (Map.lookup v s) of
                                                           Nothing -> rs'
                                                           Just k' -> Map.insertWith (++) k (map (\k -> (v,k)) k') rs') rs vs) Map.empty (Map.assocs r)

--------------------------------------------------

unfoldrM_ :: Monad m => (a -> m (Maybe a)) -> a -> m ()
unfoldrM_ f x = unfoldrM (((fmap ((),)) `fmap`) . f) x >> return ()

labelEq (Just (F.ExpValue _ _ (F.ValLabel l1))) (Just (F.ExpValue _ _ (F.ValLabel l2))) = l1 == l2
labelEq _ _ = False

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)
