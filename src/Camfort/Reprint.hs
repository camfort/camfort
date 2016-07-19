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

{-# LANGUAGE RankNTypes #-}

module Camfort.Reprint where

import Data.Generics.Zipper

import Camfort.PrettyPrint
import Camfort.Analysis.Annotations
import Camfort.Helpers
import Camfort.Traverse

import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity
import Data.Data
import Control.Monad.Trans.State.Lazy

import Language.Fortran
{- data SrcLoc
  = SrcLoc {srcFilename :: String, srcLine :: Int, srcColumn :: Int}
-}
import Camfort.Analysis.Syntax

type Query m a = a -> StateT SrcLoc m (String, Bool)

-- Start of GLORIOUS REFACTORING ALGORITHM!
-- FIXME: Use ByteString! (Or Data.Text, at least)

reprint :: (Monad m, Data (p Annotation), PrettyPrint (p Annotation))
        => (forall b . Typeable b => [String] -> Query m b)
        -> SourceText -> Filename -> p Annotation -> m String
reprint refactoring input f p
  -- If the inupt is null then switch into pretty printer
  | B.null input = return $ prettyPrint p
  -- Otherwise go with the normal algorithm
  | otherwise = do
      let input' = map B.unpack $ B.lines input
      let len    = Prelude.length input'
      let start  = SrcLoc f 1 0
      let end    = SrcLoc f len (1 + (Prelude.length $ Prelude.last input'))
      (pn, cursorn) <- runStateT (reprintC refactoring input' (toZipper p)) start
      let (_, inpn) = takeBounds (start, cursorn) input'
      let (pe, _)   = takeBounds (cursorn, end) inpn
      return $ pn ++ pe

reprintC :: Monad m
         => (forall b . (Typeable b) => [String] -> Query m b)
         -> [String] -> Zipper a -> StateT SrcLoc m String
reprintC refactoring inp z = do
  cursor     <- get
  (p1, flag) <- query (refactoring inp) z
  cursor'    <- get
  (_, inp')  <- return $ takeBounds (cursor, cursor') inp
  p2         <- if flag then return ""
                        else enterDown refactoring inp' z
  cursor''   <- get
  (_, inp'') <- return $ takeBounds (cursor', cursor'') inp'
  p3         <- enterRight refactoring inp'' z
  return $ p1 ++ p2 ++ p3

enterDown, enterRight ::
              Monad m
           => (forall b . (Typeable b) => [String] -> Query m b)
           -> [String] -> Zipper a -> StateT SrcLoc m String
enterDown refactoring inp z =
  case (down' z) of
    -- Go to children
    Just dz -> reprintC refactoring inp dz
    -- No children
    Nothing -> return $ ""

enterRight refactoring inp z =
  case (right z) of
    -- Go to right sibling
    Just rz -> reprintC refactoring inp rz
    -- No right sibling
    Nothing -> return $ ""

takeBounds :: (SrcLoc, SrcLoc) -> [String] -> (String, [String])
takeBounds (l, u) inp = takeBounds' (lineCol l, lineCol u) [] inp
takeBounds' ((ll, lc), (ul, uc)) tk inp  =
    if (ll == ul && lc == uc) || (ll > ul) then (Prelude.reverse tk, inp)
    else case inp of []             -> (Prelude.reverse tk, inp)
                     ([]:[])        -> (Prelude.reverse tk, inp)
                     ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
                     ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)

-- End of GLORIOUS REFACTORING ALGORITHM
