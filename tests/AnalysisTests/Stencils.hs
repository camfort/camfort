{-# LANGUAGE GADTs, FlexibleInstances #-}

module Tests.Stencils where

import Camfort.Analysis.Stencils

import Test.QuickCheck

instance Arbitrary Direction where
   arbitrary = do coin <- arbitrary
                  return $ if coin then Fwd else Bwd

instance Arbitrary Spec where
   arbitrary = do coin <- arbitrary
                  if coin then return $ Reflexive
                  else do dir <- arbitrary
                          depth <- choose (0, 10)
                          dim   <- choose (0, 7)
                          sat   <- arbitrary
                          return $ Span depth dim dir sat

incrSpecSat :: Spec -> Spec
incrSpecSat Reflexive = Reflexive
incrSpecSat (Span depth dim dir s) = Span (depth + 1) dim dir True

coalesceConsecutive =
  quickCheck (\spec -> case (plus (NS spec (incrSpecSat spec))) of
                        Just Reflexive -> spec == Reflexive
                        Just (Span sdepth sdim sdir True) -> sdepth == depth (incrSpecSat spec) && sdir == direction spec && sdim == dim spec
                        _ -> False)
                                    
             

normalisedSpecs =
  quickCheck (\specs ->
    all (\(NSpecs sp) ->
     let hd = head sp
     in all (\spec -> (dim spec == dim hd) && (direction spec == direction hd)) sp) (groupByDim specs))
                           
