{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}

module Camfort.Analysis.StencilSpecs where

import Camfort.Helpers
import Data.Generics.Uniplate.Data
import Data.List
import Data.Data

{- *** 1 . Specification syntax -}

type Dimension  = Int -- spatial dimensions are 0 indexed
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

-- Disjunctive normal form (A * B) U (C * D)
-- with linear / irreflexive (A * B) U (C * D)

{- 
-- The inner elements of a specification
data Spec where
     --ReflexiveA :: [Dimension] -> Spec
     ForwardA   :: Depth -> [Dimension] -> Spec
     BackwardA  :: Depth -> [Dimension] -> Spec
     SymmetricA :: Depth -> [Dimension] -> Spec
     ConstantA  :: [Dimenson] -> Spec

-- Product of specifications
data SpecProd where
     Product :: [Spec] -> SpecProd
-- 
data SpecUnion where
     Union :: [SpecProd] -> SpecUnion

data SpecInner where
     Reflexivity :: ([Dimension] :? "irreflexives") -> [Dimension] -> SpecUnion -> SpecInner -}
     
data Specification where
     Linear    :: SpecInner -> Specification
     NonLinear :: SpecInner -> Specification     

data Specification where
     Reflexive   :: [Dimension]          -> Specification
     Forward     :: Depth -> [Dimension] -> Specification
     Backward    :: Depth -> [Dimension] -> Specification
     Symmetric   :: Depth -> [Dimension] -> Specification

     -- Product of specs
     Product     :: [Specification] -> Specification
     -- Union of specs
     Union       :: [Specification] -> Specification
     

     -- The only specification that causes exclusion
     Irrefl  :: Specification -> [Dimension] -> Specification

     -- Temporal specifications, with a list of variables for the arrays
     -- through which time is represented
     TemporalFwd :: [String] -> Specification
     TemporalBwd :: [String] -> Specification

     Unspecified :: [Dimension] -> Specification
     Constant    :: [Dimension] -> Specification
     Linear      :: Specification -> Specification
     Empty       :: Specification

deriving instance Eq Specification
deriving instance Data Specification
deriving instance Typeable Specification

{-| An (arbitrary) ordering specifications for the sake of normalisation -}
instance Ord Specification where
     Empty           <= _                = True
     _               <= Empty            = False
     
     (Reflexive ds)  <= (Reflexive ds')  = ds <= ds'
     (Reflexive ds)  <= _                = True
     _               <= (Reflexive ds)   = False
     
     (Forward depth ds)  <= (Forward depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Backward depth ds) <= (Backward depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Symmetric depth ds) <= (Symmetric depth' ds')
       | ds == ds' = depth <= depth'
       | otherwise = ds <= ds'
       
     (Product specs)  <= (Product specs')  = specs <= specs'
     (Irrefl ds spec) <= (Irrefl ds' spec')
       | ds = ds' = spec <= spec'
       | otherwise = ds <= ds'
     
     (TemporalFwd vs) <= (TemporalFwd vs') = vs <= vs'
     (TemporalBwd vs) <= (TemporalBwd vs') = vs <= vs'
     (Unspecified ds) <= (Unspecified ds') = ds <= ds'
     (Constant ds)    <= (Constant ds')    = ds <= ds'
     (Linear s)       <= (Linear s')       = s <= s'
     -- Otherwise do lexicographic ordering on the pretty printed output
     s                <= s'                = (head $ show s) <= (head $ show s')

instance Ord Direction where
         Fwd <= Bwd = True
         Fwd <= Fwd = True
         Bwd <= Bwd = True
         Bwd <= Fwd = False

-- `specPlus` combines specs by coalescing specifications with the same
-- direction and depth into one
--   e.g. forward, depth=1, dims=1 `specPlus` forward, depth=1,dims=2
--       = forward, depth=1, dims=1,2

specPlus :: Specification -> Specification -> Maybe Specification
specPlus Empty x = Just x
specPlus x Empty = Just x

specPlus (Product [s]) (Product [s'])
    = Just $ Product $ foldPair specPlus [s, s']
specPlus (Reflexive ds) (Irreflexive ds' spec)
    = Just $ Irreflexive (ds' \\ ds) (
specPlus (Irrefl ds spec) (Irrefl ds' spec')
    = (specPlus spec spec') >>= (\spec'' -> Just (Irrefl (ds ++ ds') spec''))
specPlus (Reflexive dims) (Reflexive dims')
    = Just $ Reflexive (sort $ dims ++ dims')
specPlus (Forward dep dims) (Forward dep' dims')
    | dep == dep' = Just $ Forward dep (sort $ dims ++ dims')
specPlus (Backward dep dims) (Backward dep' dims')
    | dep == dep' = Just $ Backward dep (sort $ dims ++ dims')
specPlus (Symmetric dep dims) (Symmetric dep' dims')
    | dep == dep' = Just $ Symmetric dep (sort $ dims ++ dims')
specPlus (Unspecified dims) (Unspecified dims')
    = Just $ Unspecified (dims ++ dims')
specPlus x y
    = Nothing

-- Show a list with ',' separator (used to represent union of regions)
showL :: Show a => [a] -> String
showL = concat . (intersperse ",") . (map show)

showUnionSpecs :: Show a => [a] -> String
showUnionSpecs = showL

-- Show a list with '*' separator (used to represent product of regions)
showProdSpecs :: Show a => [a] -> String
showProdSpecs = concat . (intersperse "*") . (map show)

-- Syntax
instance Show Specification where
    show Empty                = "none"
    show (Reflexive dims)     = "reflexive, dims=" ++ showL dims
    show (Forward dep dims)   = showRegion "forward" (show dep) (showL dims)
    show (Backward dep dims)  = showRegion "backward" (show dep) (showL dims)
    show (Symmetric dep dims) = showRegion "centered" (show dep) (showL dims)
    show (Unspecified dims)   = "unspecified "  ++ showL dims
    show (Constant dims)      = "fixed, dim=" ++ showL dims
    show (Irrefl [] spec  )   = "irreflexive, " ++ show spec
    show (Irrefl dims spec)   = "irreflexive, dims=" ++ showL dims ++ ", (" ++ show spec ++ ")"
    
  {-  For products of regions, we provide a compact pretty-printed output that coalesces specifications 
     of the same depth and direction

   e.g. (forward, depth=1, dim=1) * (forward, depth=1, dim=2)
   is actually output as
        forward, depth=1, dim=1*2

  This is done via the specPlus normalisation and a custom pretty-printing routine triggered
  by the InsideProd wrapepr which interprets normalised specs as produts of specs -}

    show (Product specs)      = concat . (intersperse "*") . (map ((\s -> "(" ++ s ++ ")") . show . InsideProd))
                                  . normaliseBy specPlus $ specs

    show (Linear spec)        = (show spec) ++ ", unique "
    show (TemporalFwd tdims)  = showRegion "forward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")
    show (TemporalBwd tdims)  = showRegion "backward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS

-- InsideProd wrapper to indicate how to treated specs on multi-dimensions
data InsideProd a = InsideProd a deriving (Eq, Ord)

instance Show (InsideProd Specification) where
  show (InsideProd (Reflexive dims))     = "reflexive, dims=" ++ showProdSpecs dims
  show (InsideProd (Forward dep dims))   = showRegion "forward" (show dep) (showProdSpecs dims) 
  show (InsideProd (Backward dep dims))  = showRegion "backward" (show dep) (showProdSpecs dims)
  show (InsideProd (Symmetric dep dims)) = showRegion "symmetry" (show dep) (showProdSpecs dims)
  show (InsideProd x)                    = show x
