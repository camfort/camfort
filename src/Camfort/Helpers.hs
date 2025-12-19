{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Camfort.Helpers
  (
    -- * Datatypes and Aliases
    Directory
  , FileOrDir
  , Filename
  , SourceText
    -- * Directory Helpers
  , checkDir
  , getDir
  , isDirectory
    -- * Misc Helpers
  , collect
  , descendBiReverseM
  , descendReverseM
  ) where

import Data.Generics.Uniplate.Operations
import qualified Data.Generics.Str as Str
import Data.Data
import Data.List (elemIndices, union)
import qualified Data.ByteString.Char8 as B
import System.Directory
import qualified Data.Map.Lazy as Map hiding (map, (\\))
import Control.Monad (liftM)
import Control.Monad.Writer
import Language.Fortran.Repr.Value.Scalar.Int.Machine (withFInt)
import qualified Language.Fortran.Repr as FRepr

-- collect: from an association list to a map with list-based bins for matching keys
collect :: (Eq a, Ord k) => [(k, a)] -> Map.Map k [a]
collect = Map.fromListWith union . map (fmap (:[]))

type Filename = String
type Directory = String
type SourceText = B.ByteString
type FileOrDir = String

-- Filename and directory related helpers

-- gets the directory part of a filename
getDir :: Filename -> Directory
getDir file = let ixs = elemIndices '/' file
              in if null ixs then file
                 else take (last $ ixs) file


{-| Creates a directory (from a filename string) if it doesn't exist -}
checkDir :: Directory -> IO ()
checkDir f = case (elemIndices '/' f) of
               [] -> return ()
               ix -> let d = take (last ix) f
                     in createDirectoryIfMissing True d

isDirectory :: FileOrDir -> IO Bool
isDirectory s = doesDirectoryExist s

#if __GLASGOW_HASKELL__ < 800
instance Monoid x => Monad ((,) x) where
    return a = (mempty, a)
    (x, a) >>= k = let (x', b) = k a
                   in (mappend x x', b)
#endif

-- Data-generic generic descend but processes children in reverse order
-- (good for backwards analysis)
data Reverse f a = Reverse { unwrapReverse :: f a }

instance Functor (Reverse Str.Str) where
    fmap f (Reverse s) = Reverse (fmap f s)

instance Foldable (Reverse Str.Str) where
    foldMap f (Reverse x) = foldMap f x

instance Traversable (Reverse Str.Str) where
    traverse _ (Reverse Str.Zero) = pure $ Reverse Str.Zero
    traverse f (Reverse (Str.One x)) = (Reverse . Str.One) <$> f x
    traverse f (Reverse (Str.Two x y)) = (\y' x' -> Reverse $ Str.Two x' y')
                             <$> (fmap unwrapReverse . traverse f . Reverse $ y)
                             <*> (fmap unwrapReverse . traverse f . Reverse $ x)


-- Custom version of descend that process tree in reverse order
descendReverseM :: (Data on, Monad m, Uniplate on) => (on -> m on) -> on -> m on
descendReverseM f x =
    liftM generate . fmap unwrapReverse . traverse f . Reverse $ current
  where (current, generate) = uniplate x

descendBiReverseM :: (Data from, Data to, Monad m, Biplate from to) => (to -> m to) -> from -> m from
descendBiReverseM f x =
    liftM generate . fmap unwrapReverse . traverse f . Reverse $ current
  where (current, generate) = biplate x