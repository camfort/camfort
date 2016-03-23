{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module Camfort.Analysis.StencilSpecs where

import Data.Generics.Uniplate.Data
import Data.List
import Data.Data

{- *** 1 . Specification syntax -}

type Dimension  = Int -- spatial dimensions are 0 indexed
type Depth      = Int
type Saturation = Bool
data Direction  = Fwd | Bwd deriving (Eq, Show)

data Spec where
     Reflexive   :: [Dimension]          -> Spec
     Forward     :: Depth -> [Dimension] -> Spec
     Backward    :: Depth -> [Dimension] -> Spec
     Symmetric   :: Depth -> [Dimension] -> Spec

     -- Product of two specs (takes the intersection of their models)
     Product     :: [Spec] -> Spec
     
     -- This specification modifier means that all other indices not described by it are undefined
     Only        :: Spec   -> Spec

     -- Temporal specifications, with a list of variables for the arrays
     -- through which time is represented
     TemporalFwd :: [String] -> Spec
     TemporalBwd :: [String] -> Spec

     Unspecified :: [Dimension] -> Spec
     Constant    :: [Dimension] -> Spec
     Linear      :: Spec -> Spec
     Empty       :: Spec

deriving instance Eq Spec
deriving instance Data Spec
deriving instance Typeable Spec

{-| An (arbitrary) ordering specifications for the sake of normalisation -}
instance Ord Spec where
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

-- Syntax
showL :: Show a => [a] -> String
showL = concat . (intersperse ",") . (map show)
instance Show Spec where
    show Empty                = "none"
    show (Reflexive dims)     = "reflexive, dims=" ++ showL dims
    show (Forward dep dims)   = "forward, depth="  ++ show dep ++ ", dim=" ++ showL dims
    show (Backward dep dims)  = "backward, depth=" ++ show dep ++ ", dim=" ++ showL dims
    show (Symmetric dep dims) = "centered, depth=" ++ show dep ++ ", dim=" ++ showL dims
    show (Unspecified dims)   = "unspecified "  ++ showL dims
    show (Constant dims)      = "fixed, dim=" ++ showL dims
    show (Only spec)          = "only, " ++ show spec
    show (Product specs)      = concat $ intersperse " & " $ map (\spec -> "(" ++ show spec ++ ")") specs
    show (Linear spec)        = (show spec) ++ ", unique "
    show (TemporalFwd dims)   = "forward, depth=" ++ show (length dims) ++ ", dim=t{" ++ showL dims ++ "}"
    show (TemporalBwd dims)   = "backward, depth=" ++ show (length dims) ++ ", dim=t{" ++ showL dims ++ "}"
