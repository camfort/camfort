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

data Specification where
     Reflexive   :: [Dimension]          -> Specification
     Forward     :: Depth -> [Dimension] -> Specification
     Backward    :: Depth -> [Dimension] -> Specification
     Symmetric   :: Depth -> [Dimension] -> Specification

     -- Product of two specs (takes the intersection of their models)
     Product     :: [Specification] -> Specification
     
     -- This specification modifier means that all other indices not described by it are undefined
     Only        :: Specification   -> Specification

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
     (Only spec)      <= (Only spec')      = spec <= spec'
     
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
    show (Only spec)          = "only, " ++ show spec
    
    -- Products of specifications have some specialse case printing.
    show (Product specs)      = showProdSpecs . pmonoidNormalise' $ map InsideProd specs

    show (Linear spec)        = (show spec) ++ ", unique "
    show (TemporalFwd tdims)  = showRegion "forward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")
    show (TemporalBwd tdims)  = showRegion "backward" (show $ length tdims) ("t{" ++ showL tdims ++ "}")

-- Helper for showing regions
showRegion typ depS dimS = typ ++ ", depth=" ++ depS ++ ", dim=" ++ dimS

{- For products of regions, we provide a compact pretty-printed output that coalesces specifications 
    of the same depth and direction

  e.g. (forward, depth=1, dim=1) * (forward, depth=1, dim=2)
  is actually output as
        forward, depth=1, dim=1*2

  This is done via a bit of normalisation indicator by a wrapper datatype 'InsideProd'
  where the 'InsideProd Specification' is the type of specifications over which a 
  a product is being taken
-}

data InsideProd a = InsideProd a deriving (Eq, Ord)

instance Show (InsideProd Specification) where
  show (InsideProd (Forward dep dims))   = showRegion "forward" (show dep) (showProdSpecs dims) 
  show (InsideProd (Backward dep dims))  = showRegion "backward" (show dep) (showProdSpecs dims)
  show (InsideProd (Symmetric dep dims)) = showRegion "symmetry" (show dep) (showProdSpecs dims)
  show (InsideProd x)                    = show x

{-
-- Equality of specifications inside a product is modulo the dimensionality so that
-- specifications can be grouped together by their PartialMonoid instance (see below)
instance Eq (InsideProd Specification) where
    (InsideProd x) == (InsideProd y) = eqModDim x y
      where
        eqModDim (Reflexive _) (Reflexive _)        = True
        eqModDim (Forward dep _) (Forward dep' _)   = dep == dep'
        eqModDim (Backward dep _) (Backward dep' _) = dep == dep'
        eqModDim (Symmetric dep _) (Symmetric dep' _) = dep == dep'
        eqModDim (Only s) (Only s')                 = InsideProd s == InsideProd s'
        eqModDim (Product ss) (Product ss')         = and (zipWith (\s s' -> InsideProd s == InsideProd s') ss ss')
        eqModDim (TemporalFwd _) (TemporalFwd _)    = True
        eqModDim (TemporalBwd _) (TemporalBwd _)    = True
        eqModDim (Unspecified _) (Unspecified _)    = True
        eqModDim (Constant _) (Constant _)          = True
        eqModDim (Linear s) (Linear s')             = InsideProd s == InsideProd s'
        eqModDim Empty Empty                        = True
        eqModDim _     _                            = False
-}

instance PartialMonoid (InsideProd Specification) where
  pmempty = InsideProd Empty
  -- Coalesce 
  pmappend (InsideProd x) (InsideProd y) = plus x y >>= (Just . InsideProd)
    where plus (Reflexive ds) (Reflexive ds')
            = Just $ Reflexive (ds ++ ds')

          plus (Forward dep ds) (Forward dep' ds')
            | dep == dep' = Just $ Forward dep (ds ++ ds')
            
          plus (Backward dep ds) (Backward dep' ds')
            | dep == dep' = Just $ Backward dep (ds ++ ds')
            
          plus (Symmetric dep ds) (Symmetric dep' ds')
            | dep == dep' = Just $ Symmetric dep (ds ++ ds')
            
          plus x y = Nothing

-- Ordering is usual
--instance Ord (InsideProd Specification) where
--  (InsideProd x) <= (InsideProd y) = x <= y


