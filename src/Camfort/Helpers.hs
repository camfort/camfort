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
{-# LANGUAGE TypeOperators, PolyKinds #-}

module Camfort.Helpers where

import Data.List (elemIndices, group, sort, nub)
import qualified Data.ByteString.Char8 as B
import System.Directory
import Language.Fortran

lineCol :: SrcLoc -> (Int, Int)
lineCol s = (srcLine s, srcColumn s)

spanLineCol :: SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (l, u) = (lineCol l, lineCol u)

type Filename = String
type Directory = String
type SourceText = B.ByteString
type FileOrDir = String

-- Filename and directory related helpers

-- gets the directory part of a filename
getDir :: String -> String
getDir file = let ixs = elemIndices '/' file
              in if null ixs then file
                 else take (last $ ixs) file


{-| Creates a directory (from a filename string) if it doesn't exist -}
checkDir f = case (elemIndices '/' f) of
               [] -> return ()
               ix -> let d = take (last ix) f
                     in createDirectoryIfMissing True d

isDirectory :: FileOrDir -> IO Bool
isDirectory s = doesDirectoryExist s


-- Helpers

fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
fanout f g x = (f x, g x)

(<>) :: (a -> b) -> (a -> c) -> a -> (b, c)
f <> g = fanout f g

(><) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
f >< g = \(x, y) -> (f x, g y)

-- Lookup functions over relation s

lookups :: Eq a => a -> [(a, b)] -> [b]
lookups _ [] = []
lookups x ((a, b):xs) = if (x == a) then b : lookups x xs
                                    else lookups x xs

lookups' :: Eq a => a -> [((a, b), c)] -> [(b, c)]
lookups' _ [] = []
lookups' x (((a, b), c):xs) = if (x == a) then (b, c) : lookups' x xs
                                          else lookups' x xs

{-| Computes all pairwise combinations -}
pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = (zip (repeat x) xs) ++ (pairs xs)

{-| Functor composed with list functor -}
mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
mfmap f = map (fmap f)

{-| An infix `map` operation.-}
each = flip (map)

{-| Is the Ordering an EQ? -}
cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

cmpFst :: (a -> a -> Ordering) -> (a, b) -> (a, b) -> Ordering
cmpFst c (x1, y1) (x2, y2) = c x1 x2

cmpSnd :: (b -> b -> Ordering) -> (a, b) -> (a, b) -> Ordering
cmpSnd c (x1, y1) (x2, y2) = c y1 y2

{-| used for type-level annotations giving documentation -}
type (:?) a (b :: k) = a

-- Helper function, reduces a list two elements at a time with a partial operation
foldPair :: (a -> a -> Maybe a) -> [a] -> [a]
foldPair f [] = []
foldPair f [a] = [a]
foldPair f (a:(b:xs)) = case f a b of
                          Nothing -> a : (foldPair f (b : xs))
                          Just c  -> foldPair f (c : xs)


-- Helper function, reduces a list two elements at a time with a partial operation
foldL :: (a -> a -> [a]) -> [a] -> [a]
foldL f [] = []
foldL f [a] = [a]
foldL f (a:(b:xs)) = a : b : (foldL f (f a b ++ xs))

class PartialMonoid x where
  -- Satisfies equations:
   --   pmappend x pmempty = Just x
   --   pmappend pempty x  = Just x
   --   (pmappend y z) >>= (\w -> pmappend x w) = (pmappend x y) >>= (\w -> pmappend w z)

   emptyM  :: x
   appendM :: x -> x -> Maybe x

normalise :: (Ord t, PartialMonoid t) => [t] -> [t]
normalise = nub . reduce . sort
  where reduce = foldPair appendM

normaliseBy :: Ord t => (t -> t -> Maybe t) -> [t] -> [t]
normaliseBy plus = nub . (foldPair plus) . sort
