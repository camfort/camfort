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

module Camfort.Reprint
  ( reprint
  , subtext
  , splitBySpan
  ) where

import Data.Generics.Zipper

import Camfort.Helpers

import qualified Data.ByteString.Char8 as B
import Data.Data
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import qualified Language.Fortran.Util.Position as FU

{-
Reminder:
 -- type SourceText    = B.ByteString
 -- data FU.Position = FU.Position { posAsbsoluteOffset :: Int,
                                     posColumn :: Int,
                                     posLine   :: Int }
-}


-- A refactoring takes a 'Typeable' value
-- into a stateful SourceText (B.ByteString) transformer,
-- which returns a pair of a stateful computation of an updated SourceText
-- paired with a boolean flag denoting whether a refactoring has been
-- performed.  The state contains a FU.Position which is the "cursor"
-- within the original source text. The incoming value corresponds to
-- the position of the first character in the input SourceText. The
-- outgoing value is a cursor ahead of the incoming one which shows
-- the amount of SourceText that is consumed by the refactoring.

type Refactored = Bool
type Refactoring m =
  forall b . Typeable b
         => b -> SourceText -> StateT FU.Position m (SourceText, Refactored)

-- The reprint algorithm takes a refactoring (parameteric in
-- some monad m) and turns an arbitrary pretty-printable type 'p'
-- into a monadic SourceText transformer.

reprint :: (Monad m, Data p)
        => Refactoring m -> p -> SourceText -> m SourceText
reprint refactoring tree input
  -- If the inupt is null then null is returned
  | B.null input = return B.empty
  -- Otherwise go with the normal algorithm
  | otherwise = do
   -- Create an initial cursor at the start of the file
   let cursor0 = FU.initPosition
   -- Enter the top-node of a zipper for 'tree'
   -- setting the cursor at the start of the file
   (out, (_, remaining)) <- runStateT (enter refactoring (toZipper tree)) (cursor0, input)
   -- Add to the output source the reamining input source
   return $ out `B.append` remaining

-- The enter, enterDown, enterRight each take a refactoring and a
-- zipper producing a stateful computation with (FU.Position, SourceText)
-- state.

enter, enterDown, enterRight
  :: Monad m
  => Refactoring m -> Zipper a -> StateT (FU.Position, SourceText) m SourceText

-- `enter` applies the generic refactoring to the current context
-- of the zipper
enter refactoring z = do

  -- Part 1.
  -- Apply a refactoring
  (cursor, inp)     <- get
  ((p1, refactored), cursor') <- lift $ runStateT (query (`refactoring` inp) z) cursor

  -- Part 2.
  p2 <- if refactored
        then do
          -- If the node was refactored then...
          -- cut out the portion of source text consumed by the refactoring
          (_, inp') <- return $ splitBySpan (cursor, cursor') inp
          put (cursor', inp')
          return B.empty
        else do
          -- If a refactoring was not output,
          -- enter the children of the current context
          put (cursor', inp)
          enterDown refactoring z

  -- Part 3.
  -- Enter the right sibling of the current context
  p3 <- enterRight refactoring z

  -- Concat the output for the current context, children, and right sibling
  return $ B.concat [p1, p2, p3]

-- `enterDown` navigates to the children of the current context
enterDown refactoring z =
  case down' z of
    -- Go to children
    Just dz -> enter refactoring dz
    -- No children
    Nothing -> return B.empty

-- `enterRight` navigates to the right sibling of the current context
enterRight refactoring z =
  case right z of
    -- Go to right sibling
    Just rz -> enter refactoring rz
    -- No right sibling
    Nothing -> return B.empty

-- Given a lower-bound and upper-bound pair of FU.Positions, split the
-- incoming SourceText based on the distanceF between the FU.Position pairs
splitBySpan :: (FU.Position, FU.Position) -> SourceText -> (SourceText, SourceText)
splitBySpan (l, u) = subtext (ll, lc) (ll, lc) (ul, uc)
  where (FU.Position _ lc ll) = l
        (FU.Position _ uc ul) = u

{-|
  Split a text.

  Returns a tuple containing:
    1. the bit of input text between upper and lower bounds
    2. the remaining input text

  Takes:
    1. current cursor position
    2. lower bound
    3. upper bound
    4. input text

-}
subtext :: (Int, Int) -> (Int, Int) -> (Int, Int) -> B.ByteString -> (B.ByteString, B.ByteString)
subtext cursor (lowerLn, lowerCol) (upperLn, upperCol) =
    subtext' B.empty cursor
  where
    subtext' acc (cursorLn, cursorCol) input

      | cursorLn <= lowerLn && (cursorCol >= lowerCol ==> cursorLn < lowerLn) =
        case B.uncons input of
          Nothing -> (B.reverse acc, input)
          Just ('\n', input') -> subtext' acc (cursorLn+1, 1) input'
          Just (_, input')    -> subtext' acc (cursorLn, cursorCol+1) input'

      | cursorLn <= upperLn && (cursorCol >= upperCol ==> cursorLn < upperLn) =
        case B.uncons input of
          Nothing -> (B.reverse acc, input)
          Just ('\n', input') -> subtext' (B.cons '\n' acc) (cursorLn+1, 1) input'
          Just (x, input')    -> subtext' (B.cons x acc) (cursorLn, cursorCol+1) input'

      | otherwise =
        (B.reverse acc, input)

-- | Logical implication operator.
(==>) :: Bool -> Bool -> Bool; infix 2 ==>
a ==> b = a <= b

-- Infrastructure for building the reprinter "plugins"

data RefactorType = Before | After | Replace

class Refactorable t where
  isRefactored :: t -> Maybe RefactorType
  getSpan      :: t -> (Position, Position)

-- Essentially wraps the refactorable interface
genReprinting :: (Monad m, Refactorable t, Typeable t)
    => (t -> m Source)
    -> t -> m (Maybe (RefactorType, Source, (Position, Position)))
genReprinting f z = do
  case isRefactored z of
    Nothing -> return Nothing
    Just refactorType -> do
      output <- f z
      return $ Just (refactorType, output, getSpan z)

catchAll :: Monad m => a -> m (Maybe b)
catchAll _ = return Nothing
