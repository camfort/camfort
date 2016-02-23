{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleContexts #-}

module Analysis.Stencils where

import Language.Fortran hiding (Spec)

import Data.Generics.Uniplate.Operations
import Control.Monad.State.Lazy

import Analysis.Loops
import Analysis.LVA
import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types

import qualified Data.Map as Map
import Data.List
import Helpers
import Traverse

import Debug.Trace

import Transformation.Syntax

-- Infer and check stencil specifications
infer :: Program a -> String
infer p = specInference .
          -- First set the unit annotation
          map (fmap (const unitAnnotation)) $ p

check :: Program a -> Program Annotation
check = error "Not yet implemented"

-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f90" [] () ()

specInference :: Program Annotation -> String
specInference p = concatMap specInference' p
specInference' p =
             let formatSpec span (arrayVar, spec) =
                      modify (\out -> out ++ show (spanLineCol span)
                                          ++ " - " ++ (concat $ intersperse "," arrayVar)
                                          ++ ": " ++ show spec ++ "\n")
                 perBlock :: Block Annotation -> State String (Block Annotation)
                 perBlock b = let tenv = typeEnv b
                              in transformBiM (perStmt tenv) b

                 perStmt :: TypeEnv Annotation -> Fortran Annotation -> State String (Fortran Annotation)
                 perStmt tenv f@(Assg annotation span lhs rhs) =
                     do -- Get array indexing (on the RHS)
                        let arrayAccesses = collect
                                                    [(v, e) | (Var _ _ [(VarName _ v, e)]) <- rhsExpr f,
                                                              length e > 0,
                                                              isArrayType tenv v]
                        mapM (formatSpec span) (groupKeyBy $ Map.toList $ fmap ixCollectionToSpec $ arrayAccesses)
                        return f
                 perStmt tenv f@(For annotation span ivar start end inc body) = -- TODO: Get induction-variable info from here
                                                                                return f
                 perStmt _ f = return f
                 
             in case runState (transformBiM perBlock p) "" of (_, output) -> output


{- *** 1 . Specification syntax -}

type Dimension  = Int
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

data Spec where
     Reflexive   :: Spec
     Forward     :: Depth -> [Dimension] -> Spec
     Backward    :: Depth -> [Dimension] -> Spec
     Symmetric   :: Depth -> [Dimension] -> Spec
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

{- *** 2 . Operations on specs, and conversion from indexing expressions -}

-- Convert list of indexing expressions to list of specs
ixCollectionToSpec :: [[Expr p]] -> [Spec]
ixCollectionToSpec es = let x = normalise . ixExprAToSpecIs $ es
                        in specIsToSpecs x  -- (show (ixExprAToSpecIs $ (es)) ++ "\n" ++ show x) `trace`  

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
     Span        :: Dimension -> Depth -> Direction -> Saturation -> SpecI
     Reflx       :: Dimension -> SpecI
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
         (Span dim depth dir s) <= (Span dim' depth' dir' s')
           | (dim == dim') && (dir == dir') = depth <= depth'
           | (dim == dim') = dir <= dir'
         (Reflx dim) <= (Reflx dim') = dim <= dim'
         (Reflx _)   <= _            = True
         (Const dim) <= (Const dim') = dim <= dim'
         _     <= _ = False

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

-- Mark spans from 0 to 1 as saturated.
firstAsSaturated :: [Normalised [SpecI]] -> [Normalised [SpecI]]
firstAsSaturated [] = []
firstAsSaturated ((NSpecIs s):xs) = (NSpecIs $ map go s) : (firstAsSaturated xs)
                                     where go (Span dim 1 dir sat) = Span dim 1 dir True
                                           go s = s

eqDim :: SpecI -> SpecI -> Bool
eqDim (Reflx d) (Reflx d')                = (d == d')
eqDim (Span d _ dir _) (Span d' _ dir' _) = (d == d') 
eqDim (Const d) (Const d')                = (d == d')
eqDim (Const d) (Reflx d')                = d == d'
eqDim (Reflx d) (Const d')                = d == d'
eqDim (Span d _ _ _) (Reflx d')           = d == d'
eqDim (Reflx d') (Span d _ _ _)           = d == d'
eqDim (Const d) (Span d' _ _ _)           = d == d'
eqDim (Span d _ _ _) (Const d')           = d == d'
-- eqDim _ _ = False

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
ixExprAToSpecIs :: [[Expr p]] -> [SpecI]
ixExprAToSpecIs ess = 
                concatMap (\es -> case (mapM (uncurry ixCompExprToSpecI) (zip [0..(length es)] es)) of
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
ixCompExprToSpecI :: Dimension -> Expr p -> Maybe SpecI
ixCompExprToSpecI d (Var _ _ [(VarName _ v, [])]) | isInductionVariable v = Just $ Reflx d

ixCompExprToSpecI d (Bin _ _ (Plus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offset)) | isInductionVariable v =
                       let x = read offset in Just $ Span d (read offset) (if x < 0 then Bwd else Fwd) False

ixCompExprToSpecI d (Bin _ _ (Plus _) (Con _ _ offset) (Var _ _ [(VarName _ v, [])])) | isInductionVariable v =
                       let x = read offset in Just $ Span d (read offset) (if x < 0 then Bwd else Fwd) False

ixCompExprToSpecI d (Bin _ _ (Minus _) (Var _ _ [(VarName _ v, [])]) (Con _ _ offset)) | isInductionVariable v =
                       let x = read offset in Just $ Span d (read offset) (if x < 0 then Fwd else Bwd) False

ixCompExprToSpecI d (Con _ _ offset) = Just $ Const d

ixCompExprToSpecI d _ = Nothing

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

type FlowsMap = [(Variable, [Variable])] -- e.g. (v, [a, b]) means that 'a' and 'b' flow to 'v'

{-
flowAnalysisArrays :: Program Annotation -> Program (Annotation, FlowsMap)
flowAnalysisArrays p = 
         let perBlock :: Block Annotation -> State String (Block Annotation)
             perBlock b = let tenv = typeEnv b
                          in transformBiM (perStmt tenv) b

             perStmt :: Fortran Annotation -> Fortran (Annotation, FlowsMap)
             perStmt tenv f@(Assg annotation span lhs rhs) = 
-}