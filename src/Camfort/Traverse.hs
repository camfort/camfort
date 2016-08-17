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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Camfort.Traverse where

import GHC.Generics
import Data.Generics.Zipper
import Data.Generics.Aliases
import Data.Generics.Str
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Maybe
import Data.Monoid
import Control.Monad.Writer.Strict

#if __GLASGOW_HASKELL__ < 800
instance Monoid x => Monad ((,) x) where
    return a = (mempty, a)
    (x, a) >>= k = let (x', b) = k a
                   in (mappend x x', b)
#endif

-- Data-type generic reduce traversal
reduceCollect :: (Data s, Data t, Uniplate t, Biplate t s) => (s -> Maybe a) -> t -> [a]
reduceCollect k x = execWriter (transformBiM (\y -> do case k y of
                                                         Just x -> tell [x]
                                                         Nothing -> return ()
                                                       return y) x)

-- Data-type generic comonad-style traversal with zipper (contextual traversal)
everywhere :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
everywhere k z = everywhere' z
  where
    everywhere' = enterRight . enterDown . k

    enterDown z =
        case (down' z) of
          Just dz -> let dz' = everywhere' dz
                     in case (up $ dz') of
                          Just uz -> uz
                          Nothing -> dz'
          Nothing -> z

    enterRight z =
         case (right z) of
           Just rz -> let rz' = everywhere' rz
                      in case (left $ rz') of
                           Just lz -> lz
                           Nothing -> rz'
           Nothing -> z


zfmap :: Data a => (a -> a) -> Zipper (d a) -> Zipper (d a)
zfmap f x = zeverywhere (mkT f) x