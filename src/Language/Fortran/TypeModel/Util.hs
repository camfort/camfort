module Language.Fortran.TypeModel.Util where

import           Control.Applicative (Alternative (..))

import           Data.Function       ((&))

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

matchingWithBoth :: (Monad m) => (a -> b -> m c) -> (c -> m r) -> a -> b -> m r
matchingWithBoth f k x y = k =<< f x y


matchingWith2 :: (Monad m) => (a -> m a') -> (b -> m b') -> ((a', b') -> m r) -> a -> b -> m r
matchingWith2 f g k x y = do
  x' <- f x
  y' <- g y
  k (x', y')


alt2 :: (Alternative f) => (a -> b -> f c) -> (a -> b -> f c) -> a -> b -> f c
alt2 f g x y = f x y <|> g x y


(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)


with :: a -> (a -> b) -> b
with = (&)
