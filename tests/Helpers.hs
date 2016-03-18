{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Helpers where

import Test.QuickCheck
import Helpers.Vec

instance Arbitrary a => Arbitrary (Vec Z a) where
      arbitrary = return Nil
instance (Arbitrary (Vec n a), Arbitrary a) => Arbitrary (Vec (S n) a) where
      arbitrary = do x  <- arbitrary :: Gen a
                     xs <- arbitrary :: Gen (Vec n a)
                     return $ Cons x xs
                     
