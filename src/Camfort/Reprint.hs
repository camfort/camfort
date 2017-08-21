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
  , Reprinting
  , ReprintingOut
  , catchAll
  , genReprinting
  , Refactorable(..)
  , RefactorType(..)
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

type Reprinting m =
 forall b . Typeable b
         => b -> m ReprintingOut

type ReprintingOut = Maybe (RefactorType, SourceText, (FU.Position, FU.Position))


-- The reprint algorithm takes a refactoring (parameteric in
-- some monad m) and turns an arbitrary pretty-printable type 'p'
-- into a monadic Source transformer.

reprint :: (Monad m, Data p)
        => Reprinting m -> p -> SourceText -> m SourceText
reprint reprinting tree input
  -- If the inupt is null then null is returned
  | B.null input = return B.empty

  -- Otherwise go with the normal algorithm
  | otherwise = do
   -- Initial state comprises start cursor and input source
   let state0 = (FU.initPosition, input)
   -- Enter the top-node of a zipper for 'tree'
   -- setting the cursor at the start of the file
   (out, (_, remaining)) <- runStateT (enter reprinting (toZipper tree)) state0
   -- Add to the output source the reamining input source
   return $ out `B.append` remaining

-- The enter, enterDown, enterRight each take a refactoring and a
-- zipper producing a stateful computation with (FU.Position, SourceText)
-- state.

enter, enterDown, enterRight
  :: Monad m
  => Reprinting m -> Zipper a -> StateT (FU.Position, SourceText) m SourceText

-- `enter` applies the generic refactoring to the current context
-- of the zipper
enter reprinting z = do

  -- Step 1.
  -- Apply a refactoring
  refactoringInfo <- lift $ query reprinting z

  -- Step 2.
  output <-
    case refactoringInfo of
      -- No refactoring, so go into the children
      Nothing -> enterDown reprinting z

      -- A refactoring was applied
      Just (typ, output, (lb, ub)) -> do
        (cursor, inp) <- get
        case typ of
          Replace -> do
             -- Get the soure text up to the start of the refactored expr
             let (p0, inp') = splitBySpan (cursor, lb) inp
             -- Cut out the portion of source text consumed by the refactoring
             let (_, inp'') = splitBySpan (lb, ub) inp'
             put (ub, inp'')
             return $ B.concat [p0, output]
          After -> do
             -- Get the soure text up to the end of the refactored expr
             let (p0, inp') = splitBySpan (cursor, ub) inp
             put (ub, inp')
             return $ B.concat [p0, output]
          Before -> do
             -- Get the soure text up to the start of the refactored expr
             let (p0, inp')  = splitBySpan (cursor, lb) inp
             -- Cut out the portion of source text consumed by the refactoring
             let (p1, inp'') = splitBySpan (lb, ub) inp'
             put (ub, inp'')
             return $ B.concat [p0, output, p1]

  -- Part 3.
  -- Enter the right sibling of the current context
  output' <- enterRight reprinting z

  -- Concat the output for the current context, children, and right sibling
  return $ B.concat [output, output']

-- `enterDown` navigates to the children of the current context
enterDown reprinting z =
  case down' z of
    -- Go to children
    Just dz -> enter reprinting dz
    -- No children
    Nothing -> return B.empty

-- `enterRight` navigates to the right sibling of the current context
enterRight reprinting z =
  case right z of
    -- Go to right sibling
    Just rz -> enter reprinting rz
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
  getSpan      :: t -> (FU.Position, FU.Position)

-- Essentially wraps the refactorable interface
genReprinting :: (Monad m, Refactorable t, Typeable t)
    => (t -> m SourceText)
    -> t -> m (Maybe (RefactorType, SourceText, (FU.Position, FU.Position)))
genReprinting f z = do
  case isRefactored z of
    Nothing -> return Nothing
    Just refactorType -> do
      output <- f z
      return $ Just (refactorType, output, getSpan z)

catchAll :: Monad m => a -> m (Maybe b)
catchAll _ = return Nothing
