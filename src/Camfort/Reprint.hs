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
import Camfort.Analysis.Syntax

-- Start of GLORIOUS REFACTORING ALGORITHM!
-- FIXME: Use ByteString! (Or Data.Text, at least)

reprint :: (Data (p Annotation), PrettyPrint (p Annotation))
        => (forall a . Typeable a => [String] -> SrcLoc -> a -> State Int (String, SrcLoc, Bool))
        -> SourceText -> Filename -> p Annotation -> String
reprint refactoring input f p
  -- If the inupt is null then switch into pretty printer
  | B.null input = prettyPrint p
  -- Otherwise go with the normal algorithm
  | otherwise =
    pn ++ pe
  where input' = map B.unpack $ B.lines input
        len = Prelude.length input'
        start = SrcLoc f 1 0
        end = SrcLoc f len (1 + (Prelude.length $ Prelude.last input'))
        (pn, cursorn) = evalState (reprintC refactoring start input' (toZipper p)) 0
        (_, inpn) = takeBounds (start, cursorn) input'
        (pe, _) = takeBounds (cursorn, end) inpn

reprintC :: (forall b . (Typeable b) => [String] -> SrcLoc -> b -> State Int (String, SrcLoc, Bool))
         -> SrcLoc -> [String] -> Zipper a -> State Int (String, SrcLoc)
reprintC refactoring cursor inp z = do
  (p1, cursor', flag) <- query (refactoring inp cursor) z

  (_, inp')       <- return $ takeBounds (cursor, cursor') inp
  (p2, cursor'')  <- if flag then return ("", cursor')
                             else enterDown refactoring cursor' inp' z

  (_, inp'')      <- return $ takeBounds (cursor', cursor'') inp'
  (p3, cursor''') <- enterRight refactoring cursor'' inp'' z

  return (p1 ++ p2 ++ p3, cursor''')

enterDown, enterRight ::
             (forall b . (Typeable b) => [String] -> SrcLoc -> b -> State Int (String, SrcLoc, Bool))
          -> SrcLoc -> [String] -> Zipper a -> State Int (String, SrcLoc)
enterDown refactoring cursor inp z = case (down' z) of
                             Just dz -> reprintC refactoring cursor inp dz
                             Nothing -> return $ ("", cursor)

enterRight refactoring cursor inp z = case (right z) of
                             Just rz -> reprintC refactoring cursor inp rz
                             Nothing -> return $ ("", cursor)

takeBounds (l, u) inp = takeBounds' (lineCol l, lineCol u) [] inp
takeBounds' ((ll, lc), (ul, uc)) tk inp  =
    if (ll == ul && lc == uc) || (ll > ul) then (Prelude.reverse tk, inp)
    else case inp of []             -> (Prelude.reverse tk, inp)
                     ([]:[])        -> (Prelude.reverse tk, inp)
                     ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
                     ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)

-- End of GLORIOUS REFACTORING ALGORITHM
