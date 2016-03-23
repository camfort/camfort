{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Camfort.Helpers.VecSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen(..))

import Camfort.Helpers.Vec

instance Arbitrary a => Arbitrary (Vec Z a) where
      arbitrary = return Nil
instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
      arbitrary = do x  <- arbitrary :: Gen a
                     xs <- arbitrary :: Gen (Vec n a)
                     return $ Cons x xs

spec :: Spec
spec = 
    describe "Vector" $
        it "TODO" 
            pending
