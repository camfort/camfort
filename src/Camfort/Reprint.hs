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
import Camfort.Traverse
import Camfort.Helpers

import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity
import Data.Data
import Control.Monad.Trans.State.Lazy

import Language.Fortran
{- data SrcLoc
  = SrcLoc {srcFilename :: String, srcLine :: Int, srcColumn :: Int}
-}
import Camfort.Analysis.Syntax

--type SourceText    = B.ByteString
type NoChildReprint = Bool
type Refactoring m =
    forall b . Typeable b => SourceText -> b -> StateT SrcLoc m (SourceText, NoChildReprint)

reprint :: (Monad m, Data (p Annotation), PrettyPrint (p Annotation))
        => Refactoring m -> SourceText -> Filename -> p Annotation -> m SourceText
reprint refactoring input f p
  -- If the inupt is null then switch into pretty printer
  | B.null input = return $ prettyPrint p
  -- Otherwise go with the normal algorithm
  | otherwise = do
      let numLines = length (B.lines input)
      let lastCol  = 1 + (B.length $ last (B.lines input))
      let startLoc = SrcLoc f 1 0
      let endLoc   = SrcLoc f numLines lastCol
      (output, cursorn) <- runStateT (reprintC refactoring input (toZipper p)) startLoc
      let (_, input')  = takeBounds (startLoc, cursorn) input
      let (output', _) = takeBounds (cursorn, endLoc) input'
      return $ B.concat [output, output']

reprintC :: Monad m
         => Refactoring m -> SourceText -> Zipper a -> StateT SrcLoc m SourceText
reprintC refactoring inp z = do
  cursor     <- get
  (p1, dontReprintChildren) <- query (refactoring inp) z
  cursor'    <- get
  (_, inp')  <- return $ takeBounds (cursor, cursor') inp
  p2         <- if dontReprintChildren
                   then return B.empty
                   else enterDown refactoring inp' z
  cursor''   <- get
  (_, inp'') <- return $ takeBounds (cursor', cursor'') inp'
  p3         <- enterRight refactoring inp'' z
  return $ B.concat [p1, p2, p3]

enterDown, enterRight ::
              Monad m
           => Refactoring m -> SourceText -> Zipper a -> StateT SrcLoc m SourceText
enterDown refactoring inp z =
  case (down' z) of
    -- Go to children
    Just dz -> reprintC refactoring inp dz
    -- No children
    Nothing -> return $ B.empty

enterRight refactoring inp z =
  case (right z) of
    -- Go to right sibling
    Just rz -> reprintC refactoring inp rz
    -- No right sibling
    Nothing -> return $ B.empty

takeBounds :: (SrcLoc, SrcLoc) -> SourceText -> (SourceText, SourceText)
takeBounds (l, u) inp = takeBounds' (lineCol l, lineCol u) B.empty inp
takeBounds' ((ll, lc), (ul, uc)) tk inp  =
    if (ll == ul && lc == uc) || (ll > ul) then (B.reverse tk, inp)
    else
      case B.uncons inp of
         Nothing         -> (B.reverse tk, inp)
         Just ('\n', ys) -> takeBounds' ((ll+1, 0), (ul, uc)) (B.cons '\n' tk) ys
         Just (x, xs)    -> takeBounds' ((ll, lc+1), (ul, uc)) (B.cons x tk) xs