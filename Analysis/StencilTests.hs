{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, GADTs, RankNTypes, KindSignatures, UndecidableInstances #-}

module Analysis.StencilTests where

import Test.HUnit
import Test.QuickCheck

import Data.List

import Helpers.Vec
import Analysis.StencilInferenceEngine
import Analysis.Annotations
import Analysis.StencilsForpar
import qualified Forpar.AST as F
import Forpar.Util.Position

a = unitAnnotation
s = SrcSpan (Position 0 0 0) (Position 0 0 0)

mkOffset v o | o == 0    = F.ExpValue a s (F.ValVariable a v)
             | o  > 0    = F.ExpBinary a s F.Addition (F.ExpValue a s (F.ValVariable a v))
                                                      (F.ExpValue a s (F.ValInteger $ show o))
             | otherwise = F.ExpBinary a s F.Subtraction (F.ExpValue a s (F.ValVariable a v))
                                                         (F.ExpValue a s (F.ValInteger $ show (abs o)))


fromFormatToExprs [] = [] 
fromFormatToExprs ((ri,rj):xs) = [mkOffset "i" ri, mkOffset "j" rj] : fromFormatToExprs xs


test2DSpecVariations [] = []
test2DSpecVariations ((forms,spec):xs) =
  let t = test (assertEqual ("format= " ++ show forms) spec (ixCollectionToSpec ["i", "j"] (fromFormatToExprs forms)))
      ts = test2DSpecVariations xs
  in t : ts

{-

 0 0 0
 0 1 1
 0 1 1

-}

variations = [
     ([(0,0)], [Reflexive [1,2]]),
     ([(1,0),(0,0)], [Forward 1 [1], Reflexive [2]]),
     ([(0,1),(0,0)], [Reflexive [1], Forward 1 [2]]),
     ([(1,1),(0,1),(1,0),(0,0)], [Forward 1 [1,2]]),
     ([(-1,0),(0,0)], [Backward 1 [1], Reflexive [2]]),
     ([(0,-1),(0,0)], [Reflexive [1], Backward 1 [2]]),
     ([(-1,-1),(0,-1),(-1,0),(0,0)], [Backward 1 [1,2]]),
     ([(0, -1), (1, -1), (0, 0), (1, 0), (1, 1), (0, 1)], [Product [Symmetric 1 [2], Forward 1 [1]]])]

testEngine = [
       test (assertEqual "sorting on indices"
                         ([Cons 1 (Cons 1 (Cons 1 Nil)),
                          Cons 2 (Cons 1 (Cons 1 Nil)),
                          Cons 1 (Cons 2 (Cons 1 Nil)),
                          Cons 0 (Cons 3 (Cons 1 Nil)),
                          Cons 1 (Cons 0 (Cons 2 Nil)),
                          Cons 2 (Cons 2 (Cons 3 Nil)),
                          Cons 1 (Cons 3 (Cons 3 Nil))]::[Vec (S (S (S Z))) Int])
                    (sort [Cons 1 (Cons 2 (Cons 1 Nil)),
                           Cons 2 (Cons 2 (Cons 3 Nil)),
                           Cons 1 (Cons 3 (Cons 3 Nil)),
                           Cons 0 (Cons 3 (Cons 1 Nil)),
                           Cons 1 (Cons 0 (Cons 2 Nil)),
                           Cons 1 (Cons 1 (Cons 1 Nil)),
                           Cons 2 (Cons 1 (Cons 1 Nil))])),
        test (assertEqual "composeRegions (1,0)-(1,0) span and (2,0)-(2,0) span"
                (Just ((Cons 1 (Cons 0 Nil)), (Cons 2 (Cons 0 Nil))))
                (composeConsecutiveSpans (Cons 1 (Cons 0 Nil), Cons 1 (Cons 0 Nil)) 
                        (Cons 2 (Cons 0 Nil), Cons 2 (Cons 0 Nil)))),
        test (assertEqual "composeRegions failing on (1,0)-(2,0) span and (4,0)-(5,0) span"
                Nothing
                (composeConsecutiveSpans (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil))
                        (Cons 4 (Cons 0 Nil), Cons 5 (Cons 0 Nil)))),
        test (assertEqual "composeRegions failing on (1,0)-(2,0) span and (3,1)-(3,1) span"
                Nothing
                (composeConsecutiveSpans (Cons 1 (Cons 0 Nil), Cons 2 (Cons 0 Nil)) 
                        (Cons 3 (Cons 1 Nil), Cons 3 (Cons 1 Nil)))),
        test (assertEqual "five point stencil 2D"
                -- Sort the expected value for the sake of easy equality
                (sort [(Cons (-1) (Cons 0 Nil), Cons 1 (Cons 0 Nil)),
                       (Cons 0 (Cons (-1) Nil), Cons 0 (Cons 1 Nil))])
                (inferMinimalVectorRegions $ fivepoint)),
        test (assertEqual "seven point stencil 3D"
                (sort  [(Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 1 (Cons 0 (Cons 0 Nil))),
                        (Cons 0 (Cons (-1) (Cons 0 Nil)), Cons 0 (Cons 1 (Cons 0 Nil))),
                        (Cons 0 (Cons 0 (Cons (-1) Nil)), Cons 0 (Cons 0 (Cons 1 Nil)))])
                (inferMinimalVectorRegions $ sevenpoint))
        ]

-- Indices for the 2D five point stencil (deliberately in an odd order)
fivepoint = [Cons (-1) (Cons 0 Nil), Cons 0 (Cons (-1) Nil), Cons 1 (Cons 0 Nil),
             Cons 0 (Cons 1 Nil), Cons 0 (Cons 0 Nil)]
-- Indices for the 3D seven point stencil
sevenpoint = [Cons (-1) (Cons 0 (Cons 0 Nil)), Cons 0 (Cons (-1) (Cons 0 Nil)),
              Cons 0 (Cons 0 (Cons 1 Nil)), Cons 0 (Cons 1 (Cons 0 Nil)),
              Cons 1 (Cons 0 (Cons 0 Nil)), Cons 0 (Cons 0 (Cons (-1) Nil)),
              Cons 0 (Cons 0 (Cons 0 Nil))]
centeredFwd = [Cons 1 (Cons 0 Nil), Cons 0 (Cons 1 Nil), Cons 0 (Cons (-1) Nil),
               Cons 1 (Cons 1 Nil), Cons 0 (Cons 0 Nil), Cons 1 (Cons (-1) Nil)]::[Vec (S (S Z)) Int]




main = do
   -- Test the inference enging
    runTestTT $ TestList testEngine
    -- Test basic properties of spanBounding box calculation
    let four = Succ (Succ (Succ (Succ Zero)))
    putStrLn "Idempotence of spanBounding"
    upTo four (\n -> quickCheck (prop_spanBoundingIdem n))
    putStrLn "Associativity of spanBounding"    
    upTo four (\n -> quickCheck (uncurry (uncurry (prop_spanBoundingAssoc n))))
    putStrLn "Un-permutable permutations on vectors"
    upTo four (\n -> quickCheck (prop_perms_invertable n))
    runTestTT $ TestList $ test2DSpecVariations variations

{- Construct arbtirary vectors and test up to certain sizes -}
instance Arbitrary a => Arbitrary (Vec Z a) where
    arbitrary = return Nil
      
instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
    arbitrary = do x  <- arbitrary 
                   xs <- arbitrary 
                   return $ Cons x xs

{- Apply a property (Natural n -> IO a) at each n <= m -}
class UpTo (m :: Nat) where
    upTo :: Natural m
         -> (forall n . (Arbitrary (Vec n Int), Permutable n) => Natural n -> IO a) -> IO a
instance UpTo Z where
    upTo Zero k     = k Zero
instance (Arbitrary (Vec n Int), Permutable (S n), Permutable n, UpTo n) => UpTo (S n) where
    upTo (Succ n) k = k (Succ n) >> upTo n k

