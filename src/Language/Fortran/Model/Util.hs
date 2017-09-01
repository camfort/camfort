module Language.Fortran.Model.Util where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Function       ((&), on)

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------


on2 :: (c -> d -> e) -> (a -> c) -> (b -> d) -> a -> b -> e
on2 h g f = (h . g) *.. f


matchingWithBoth :: (Monad m) => (a -> b -> m c) -> (c -> m r) -> a -> b -> m r
matchingWithBoth f k = (>>= k) ..* f


matchingWith2 :: (Monad m) => (a -> m a') -> (b -> m b') -> ((a', b') -> m r) -> a -> b -> m r
matchingWith2 = matchingWithBoth ..* on2 (liftA2 (,))


altf :: (Alternative f) => (a -> f b) -> (a -> f b) -> a -> f b
altf = runReaderT ..* (<|>) `on` ReaderT


altf2 :: (Alternative f) => (a -> b -> f c) -> (a -> b -> f c) -> a -> b -> f c
altf2 = curry ..* altf `on` uncurry


(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)


with :: a -> (a -> b) -> b
with = (&)


(*..) :: (a -> c -> d) -> (b -> c) -> a -> b -> d
(f *.. g) x y = f x (g y)

(..*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ..* g = curry (f . uncurry g)
