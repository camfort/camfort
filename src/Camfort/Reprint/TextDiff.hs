-- | This module provides an interface for rewriting textual, unparsed Fortran
-- using a diff-like algorithm.
--
-- Original code from Bloomberg, used with permission.

module Camfort.Reprint.TextDiff
  ( RI.SourceLocation(..)
  , RI.SourceRange(..)
  , RI.Replacement(..)
  , RI.ReplacementError(..)
  , RI.ReplacementMap
  , partitionOverlapping
  , processReplacements
  , spanToSourceRange
  , spanToSourceRange2
  , sourceRangeBetweenTwoSpans
  )
where

import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Camfort.Reprint.TextDiff.Internal
                                               as RI
import           Control.Exception              ( finally )
import           Control.Monad                  ( when )
import           Data.List                      ( partition )
import qualified Data.Map                      as M
import           Language.Fortran.Util.Position ( lineCol
                                                , SrcSpan(..)
                                                )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                , renameFile
                                                )

-- | Remove overlapping items from a list of replacements and return a pair of
-- lists containing disjoint items and overlapping items, respectively.
--
-- __Important notes:__
--
-- Replacements that come first in the list will be given precedence over later
-- items.
partitionOverlapping :: [RI.Replacement] -> ([RI.Replacement], [RI.Replacement])
partitionOverlapping [] = ([], [])
partitionOverlapping repls =
  let currentRepl = head repls
      (overlapping, remaining) =
          partition (not . RI.areDisjoint currentRepl) (tail repls)
      nextResult = partitionOverlapping remaining
  in  (currentRepl : fst nextResult, overlapping <> snd nextResult)

-- | Apply a list of 'Replacement's to the orginal source file.
--
-- __Important notes:__
--
-- Source locations specified in replacements are 0-indexed.
--
-- Rewriting applies continuation lines when lines are longer than 72 characters.
--
-- __Example replacements:__
--
-- Delete the first character in a file
--
-- @ Replacement (SourceRange (SourceLocation 0 0) (SourceLocation 0 1)) "" @
--
-- Prepend "a" to 1 line, 2 column character
--
-- @ Replacement (SourceRange (SourceLocation 0 1) (SourceLocation 0 1)) "a" @
--
-- Replace a character located in 2 line, 4 column with "a"
--
-- @ Replacement (SourceRange (SourceLocation 1 3) (SourceLocation 1 4)) "a" @
--
-- Replace string starting in 2 line, 4 column and ending in 2 line, 6 column (inclusive) with "a"
--
-- @ Replacement (SourceRange (SourceLocation 1 3) (SourceLocation 1 6)) "a" @
processReplacements :: RI.ReplacementMap -> IO ()
processReplacements rm = processReplacements_ $ M.toList rm

processReplacements_ :: [(String, [RI.Replacement])] -> IO ()
processReplacements_ []                       = return ()
processReplacements_ ((filePath, repls) : xs) = do
  contents <- BC.readFile filePath
  let newContents  = RI.applyReplacements contents repls
      tempFilePath = filePath ++ ".temp"
      maybeRm      = do
        exists <- doesFileExist tempFilePath
        when exists $ removeFile tempFilePath
  flip finally maybeRm $ do
    BC.writeFile tempFilePath newContents
    renameFile tempFilePath filePath
  processReplacements_ xs

-- | Utility function to convert 'SrcSpan' to 'SourceRange'
spanToSourceRange :: SrcSpan -> RI.SourceRange
spanToSourceRange (SrcSpan start end) =
  let (l1, c1) = lineCol start
      (l2, c2) = lineCol end
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) (c1 - 1))
                     (RI.SourceLocation (l2 - 1) c2)


-- | Given two 'Span's, returns a 'SourceRange' that starts at the starting
-- location of the first span, and ends at the starting location of the second
-- span
spanToSourceRange2 :: SrcSpan -> SrcSpan -> RI.SourceRange
spanToSourceRange2 (SrcSpan start1 _) (SrcSpan start2 _) =
  let (l1, c1) = lineCol start1
      (l2, c2) = lineCol start2
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) (c1 - 1))
                     (RI.SourceLocation (l2 - 1) (c2 - 1))

-- | Given two 'Span's, returns a 'SourceRange' that starts at the ending
-- location of the first span, and ends at the starting location of the second
-- span
sourceRangeBetweenTwoSpans :: SrcSpan -> SrcSpan -> RI.SourceRange
sourceRangeBetweenTwoSpans (SrcSpan _ end1) (SrcSpan start2 _) =
  let (l1, c1) = lineCol end1
      (l2, c2) = lineCol start2
  in  RI.SourceRange (RI.SourceLocation (l1 - 1) c1)
                     (RI.SourceLocation (l2 - 1) (c2 - 1))
