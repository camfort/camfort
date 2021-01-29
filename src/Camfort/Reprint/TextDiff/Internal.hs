{-# LANGUAGE OverloadedStrings #-}

-- Original code from Bloomberg, used with permission.

module Camfort.Reprint.TextDiff.Internal where

import           Data.Int
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BC
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Data.List                      ( sort
                                                , find
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromMaybe
                                                , fromJust
                                                , maybeToList
                                                )
import qualified Data.Map                      as M
import           Data.Typeable                  ( Typeable )

-- | Represents location in source code.
--
-- Note that, 'SourceLocation' indicates space between characters,
-- i.e the following example:
--
-- @ SourceLocation 0 1 @
--
-- indicates position between first and second characters in a file.
data SourceLocation = SourceLocation Int Int deriving (Show, Eq)

-- | Represents range in source code.
data SourceRange = SourceRange SourceLocation SourceLocation deriving (Eq)
instance Show SourceRange where
  show (SourceRange (SourceLocation l1 c1) (SourceLocation l2 c2)) =
    "("
      ++ show (l1 + 1)
      ++ ":"
      ++ show (c1 + 1)
      ++ ")-("
      ++ show (l2 + 1)
      ++ ":"
      ++ show (c2 + 1)
      ++ ")"

-- | Represents a character in the original source text along with
-- any replacement operations applied to the character in place.
--
-- It expects a character (in case it's empty, Nothing should be used),
-- whether it should be removed, its 'SourceLocation' and a string that
-- should be put in place of it.
data RChar = RChar (Maybe Char) Bool SourceLocation ByteString deriving (Show, Eq)

-- | Represents the intent to replace content in the file.
--
-- The content in 'Replacement' will be used in place of what is in
-- the range described. Note that the replacement text can be shorter
-- or larger than the original span, and it can also be multi-line.
data Replacement = Replacement SourceRange String deriving (Show, Eq)
instance Ord Replacement where
  (Replacement (SourceRange a _) _) <= (Replacement (SourceRange b _) _) =
    a < b

-- | Exception raised when two 'Replacement' objects overlap
-- ('OverlappingError') or 'Replacement' points at invalid locations
-- ('InvalidRangeError').
data ReplacementError
    = OverlappingError [(Replacement, Replacement)]
    | InvalidRangeError
    deriving (Show, Typeable, Eq)

-- | As we advance through the ['RChar'] list, we consider "chunks"
-- as the unit of text written out. A chunk is either:
--
--     1. original source text up to a newline character, end of file
--        or 'RChar' described in 2.
--     2. a single 'RChar' that has non-empty replacement string
--        or is deleted.
type Chunk = [RChar]

-- | Represents map of files and replacements that will be done.
type ReplacementMap = M.Map String [Replacement]

instance Exception ReplacementError
instance Ord SourceLocation where
  (SourceLocation l1 c1) <= (SourceLocation l2 c2) =
    l1 < l2 || l1 == l2 && c1 <= c2

-- | Parses input string into a list of annotated characters.
toRCharList :: ByteString -> [RChar]
toRCharList = reverse . uncurry appendLast . BC.foldl'
  go
  (SourceLocation 0 0, [])
 where
  go :: (SourceLocation, [RChar]) -> Char -> (SourceLocation, [RChar])
  go (loc@(SourceLocation line col), rcs) c =
    let newLoc = if c /= '\n'
          then SourceLocation line (col + 1)
          else SourceLocation (line + 1) 0
    in  (newLoc, RChar (Just c) False loc "" : rcs)
  appendLast loc = (RChar Nothing False loc "" :)

-- | Marks 'RChars' in a given range to be removed later.
markRChars :: [RChar] -> SourceRange -> [RChar]
markRChars rchars sr = markRChars_ rchars sr (SourceLocation 0 0)

markRChars_ :: [RChar] -> SourceRange -> SourceLocation -> [RChar]
markRChars_ [] _ _ = []
markRChars_ (RChar x odel _ orepl : xs) sr@(SourceRange (SourceLocation sl sc) (SourceLocation el ec)) (SourceLocation l c)
  = (if    l == sl && l == el && c >= sc && c <  ec
        || l == sl && l <  el && c >= sc
        || l == el && l >  sl && c <  ec
        || l >  sl && l <  el
     then RChar x True (SourceLocation l c) ""
     else RChar x odel (SourceLocation l c) orepl
    )
    : if x /= Just '\n'
        then markRChars_ xs sr (SourceLocation l (c + 1))
        else markRChars_ xs sr (SourceLocation (l + 1) 0)

-- | Sets replacement string to be prepended to the given location.
setReplacementStringSL
  :: [RChar] -> SourceLocation -> ByteString -> Bool -> [RChar]
setReplacementStringSL [] _ _ _ = []
setReplacementStringSL (RChar och odel osl@(SourceLocation ol oc) orepl : xs) sl@(SourceLocation l c) repl isInsert
  = if l == ol && c == oc
    then if isInsert
      then
        RChar och
              odel
              osl
              -- (repl <> if isNothing och then "" else [fromJust och])
              (repl <> maybe "" BC.singleton och)
          : xs
      else RChar och odel osl repl : xs
    else RChar och odel osl orepl : setReplacementStringSL xs sl repl isInsert

-- | Sets replacement string to be prepended to the begining of the
-- given range.
setReplacementStringSR
  :: [RChar] -> SourceRange -> ByteString -> Bool -> [RChar]
setReplacementStringSR rchars (SourceRange sls _) =
  setReplacementStringSL rchars sls

-- | Applies all deletions and additions and transforms 'RChars' back
-- to a string.
evaluateRChars :: [RChar] -> ByteString
evaluateRChars = BC.concat . map evaluateRChar

-- | If 'RChar' is marked as deleted, it'll be evaluated to its
-- replacement string, otherwise original character will be returned.
evaluateRChar :: RChar -> ByteString
evaluateRChar (RChar char del _ repl) | del = repl
                                      | isNothing char = ""
                                      | otherwise = BC.singleton $ fromJust char

-- | From ['RChar'], obtain a ('Chunk', ['RChars']) where the 'Chunk'
-- is the next 'Chunk' and the ['RChar'] are the remaining 'RChar's.
nextChunk :: [RChar] -> (Chunk, [RChar])
nextChunk [] = ([], [])
-- if the current chunk is the start of inline comment, prepend it to next
nextChunk (rchar@(RChar (Just '!') True _ _) : xs) = (rchar : fst rec, snd rec)
  where rec = nextChunk xs
nextChunk (rchar@(RChar _ True _ _) : xs) = ([rchar], xs)
nextChunk rchars                          = nextChunk_ rchars

nextChunk_ :: [RChar] -> (Chunk, [RChar])
nextChunk_ [] = ([], [])
nextChunk_ ls@(RChar _ True _ _ : _) = ([], ls)
nextChunk_ (rchar@(RChar (Just '\n') _ _ _) : xs) = ([rchar], xs)
nextChunk_ (rchar : xs) = (rchar : fst rec, snd rec) where rec = nextChunk_ xs

-- | Splits ['RChar'] into 'Chunk's.
allChunks :: [RChar] -> [Chunk]
allChunks []     = []
allChunks rchars = chunk : allChunks rest
  where (chunk, rest) = nextChunk rchars

-- | Transform a list of 'Chunk's into a single string, applying
-- continuation lines when neccessary.
evaluateChunks :: [Chunk] -> ByteString
evaluateChunks ls = evaluateChunks_ ls 0

evaluateChunks_ :: [Chunk] -> Int64 -> ByteString
evaluateChunks_ []       _       = ""
evaluateChunks_ (x : xs) currLen = if overLength
  then
    "\n     +"
    <> evaluateRChars xPadded
    <> maybe (evaluateChunks_ xs (6 + nextLen)) (evaluateChunks_ xs) lastLen
  else
    chStr
      <> maybe (evaluateChunks_ xs (currLen + nextLen))
               (evaluateChunks_ xs)
               lastLen
 where
  overLength = currLen + nextLen > 72 && currLen > 0
  xPadded    = padImplicitComments x (72 - 6)
  chStr      = evaluateRChars x
  nextLen    = fromMaybe
    (BC.length chStr)
    (myMin (BC.elemIndex '\n' chStr) (BC.elemIndex '!' chStr)) -- don't line break for comments
  lastLen = BC.elemIndex '\n' $ BC.reverse chStr
  -- min for maybes that doesn't short circuit if there's a Nothing
  myMin y z = case (y, z) of
    (Just a , Just b ) -> Just $ min a b
    (Nothing, Just a ) -> Just a
    (Just a , Nothing) -> Just a
    (Nothing, Nothing) -> Nothing
  -- Text after line 72 is an implicit comment, so should stay there
  padImplicitComments :: Chunk -> Int -> Chunk
  padImplicitComments chunk targetCol = case findCommentRChar chunk of
    Just (index, rc) ->
      take index chunk
        ++ padCommentRChar rc (targetCol - index + 1)
        :  drop (index + 1) chunk
    Nothing -> chunk
   where
    findCommentRChar :: Chunk -> Maybe (Int, RChar)
    findCommentRChar =
      find ((\(RChar _ _ (SourceLocation _ cl) _) -> cl == 72) . snd)
        . zip [1 ..]
    padCommentRChar :: RChar -> Int -> RChar
    padCommentRChar (RChar char _ loc repl) padding = RChar
      char
      True
      loc
      (BC.pack (replicate padding ' ' ++ maybeToList char) `BC.append` repl)



-- | Return TRUE iff the 'Replacement' constitutes a character
-- insertion.
isInsertion :: Replacement -> Bool
isInsertion (Replacement (SourceRange (SourceLocation sl sc) (SourceLocation el ec)) repl)
  = sl == el && sc == ec && repl /= ""

insertionSR :: SourceRange -> SourceRange
insertionSR (SourceRange (SourceLocation sl sc) _) =
  SourceRange (SourceLocation sl sc) (SourceLocation sl (sc + 1))

-- | Sets a single 'Replacement' given a list of 'RChar's.
setReplacement :: [RChar] -> Replacement -> [RChar]
setReplacement rchars repl@(Replacement sr replS) =
  let replBS = BC.pack replS
  in  if isInsertion repl
        then setReplacementStringSR (markRChars rchars (insertionSR sr))
                                    (insertionSR sr)
                                    replBS
                                    True
        else setReplacementStringSR (markRChars rchars sr) sr replBS False

-- | Sets a list of 'Replacement's given a list of 'RChar's.
setReplacements :: [RChar] -> [Replacement] -> [RChar]
setReplacements rchars repls =
  let rchar' = foldl setReplacement rchars repls in adjustLineWrap rchar'


-- | heuristic to wrap line after comma or right parenthesis if applicable
adjustLineWrap :: [RChar] -> [RChar]
adjustLineWrap []  = []
adjustLineWrap [x] = [x]
adjustLineWrap (rc@(RChar _ True _ _) : rs@(RChar (Just c) False _ _ : _))
  | c `elem` [',', ')'] = adjustLineWrapAux rc [] rs
adjustLineWrap (x : xs) = x : adjustLineWrap xs


adjustLineWrapAux :: RChar -> [RChar] -> [RChar] -> [RChar]
adjustLineWrapAux rc1 deleted (rc2@(RChar (Just c) False _ _) : rs)
  | c `elem` [',', ')'] = adjustLineWrapAux (appendRC rc1 c)
                                            (deleteRC rc2 : deleted)
                                            rs
adjustLineWrapAux rc1 deleted rs = (rc1 : reverse deleted) <> adjustLineWrap rs


-- | Mark removal for the input 'RChar'
deleteRC :: RChar -> RChar
deleteRC (RChar _ _ loc s) = RChar Nothing True loc s


-- | Append the input character to the replacement string
appendRC :: RChar -> Char -> RChar
appendRC (RChar mc _ loc s) c = RChar mc True loc (s `BC.snoc` c)


-- | Checks whether two 'Replacement's are not overlapping.
areDisjoint :: Replacement -> Replacement -> Bool
areDisjoint (Replacement (SourceRange (SourceLocation r1sl r1sc) (SourceLocation r1el r1ec)) _) (Replacement (SourceRange (SourceLocation r2sl r2sc) (SourceLocation r2el r2ec)) _)
  | r2sl > r1el || r1sl > r2el
  = True
  | r1el == r2sl && r1ec <= r2sc
  = True
  | r1sl == r2el && r1sc >= r2ec
  = True
  | otherwise
  = False

-- | Checks whether:
--
--     1. the start is before the end of the range and
--     2. both start and end locations are within the code.
isValidRange :: SourceRange -> [RChar] -> Bool
isValidRange (SourceRange sl1 sl2) rchars =
  sl1 <= sl2 && isValidLocation sl1 rchars && isValidLocation sl2 rchars

isValidLocation :: SourceLocation -> [RChar] -> Bool
isValidLocation _  []                     = False
isValidLocation sl (RChar _ _ csl _ : xs) = sl == csl || isValidLocation sl xs

checkRanges :: [RChar] -> [Replacement] -> [RChar]
checkRanges rchars repls = if and validList
  then rchars
  else throw InvalidRangeError
  where validList = [ isValidRange sr rchars | (Replacement sr _) <- repls ]

checkOverlapping :: [Replacement] -> [Replacement]
checkOverlapping repls = if null overlappingPairs
  then repls
  else throw $ OverlappingError overlappingPairs
 where
  overlappingPairs = findOverlappingPairs (sort repls)

  findOverlappingPairs :: [Replacement] -> [(Replacement, Replacement)]
  findOverlappingPairs [] = []
  findOverlappingPairs repls' =
    let currentRepl = head repls'
        overlapping = takeWhile (not . areDisjoint currentRepl) (tail repls')
        nextResult  = findOverlappingPairs (tail repls')
    in  [ (currentRepl, x) | x <- overlapping ] <> nextResult

-- | Applies 'Replacement's to a string and return it.
--
-- Firstly, it transforms the string into a list of 'RChar's.
--
-- After that, it validates the 'SourceRange' of each 'Replacement'.
--
-- In the end, it splits up 'RChar's in 'Chunk's, set the
-- 'Replacement's and evaluates the 'Chunk's.
applyReplacements :: ByteString -> [Replacement] -> ByteString
applyReplacements str repls = applyReplacements_ (checkRanges rchars repls)
                                                 (checkOverlapping repls)
  where rchars = toRCharList str

applyReplacements_ :: [RChar] -> [Replacement] -> ByteString
applyReplacements_ rchars repls = evaluateChunks chunks
 where
  replRchars = setReplacements rchars repls
  chunks     = allChunks replRchars
