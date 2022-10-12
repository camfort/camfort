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

{-# LANGUAGE FlexibleInstances, UndecidableInstances,
    DoAndIfThenElse, MultiParamTypeClasses, FlexibleContexts,
    ScopedTypeVariables #-}

{- Provides support for outputting source files and analysis information -}

module Camfort.Output
  (
    -- * Classes
    OutputFiles(..)
  , Show'(..)
    -- * Refactoring
  , refactoring
  ) where

import Prelude hiding (span)

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.PrettyPrint as PP
import qualified Language.Fortran.Util.Position as FU
import Language.Fortran.Version ( FortranVersion )

import Camfort.Analysis.Annotations
import Camfort.Reprint
import Camfort.Helpers
import Camfort.Helpers.Syntax

import System.Directory

import qualified Data.ByteString.Char8 as B
import Data.Generics
import Data.Functor.Identity
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

-- Custom 'Show' which on strings is the identity
class Show' s where
      show' :: s -> String
instance {-# OVERLAPS #-} Show' String where
      show' = id
instance {-# OVERLAPS #-} (Show' a, Show' b) => Show' (a, b) where
      show' (a, b) = "(" ++ show' a ++ "," ++ show' b ++")"
instance {-# OVERLAPPABLE #-} (Show a) => Show' a where
      show' = show

class OutputFiles t where
  {-| Given a directory and list of triples of filenames, with their source
       text (if it exists) and their AST, write these to the directory -}
  mkOutputText :: FileOrDir -> t -> SourceText
  outputFile   :: t -> Filename
  isNewFile    :: t -> Bool

  outputFiles :: FileOrDir -> FileOrDir -> [t] -> IO ()
  outputFiles inp outp pdata = do
      outIsDir <- isDirectory outp
      if outIsDir then do
          -- Output to a directory, create if missing
          createDirectoryIfMissing True outp
          -- Report which directory the files are going to
          putStrLn $ "Writing refactored files to directory: " ++ outp ++ "/"
          -- If the input was a directory then work out the path prefix
          -- which needs to be replaced with the new directory path
          isdir <- isDirectory inp
          let inSrc = if isdir then inp else getDir inp
          forM_ pdata (\x -> let f' = changeDir outp inSrc (outputFile x)
                             in do checkDir f'
                                   putStrLn $ "Writing " ++ f'
                                   B.writeFile f' (mkOutputText outp x))
       else
          forM_ pdata (\x -> do
                let out = if isNewFile x then outputFile x else outp
                putStrLn $ "Writing " ++ out
                B.writeFile out (mkOutputText outp x))


{-| changeDir is used to change the directory of a filename string.
    If the filename string has no directory then this is an identity  -}
changeDir :: Eq a => [a] -> [a] -> [a] -> [a]
changeDir newDir oldDir oldFilename =
    newDir ++ listDiffL oldDir oldFilename
  where
    listDiffL []     ys = ys
    listDiffL _      [] = []
    listDiffL (x:xs) (y:ys)
        | x==y      = listDiffL xs ys
        | otherwise = ys

-- When the new source text is already provided
instance OutputFiles (Filename, SourceText) where
  mkOutputText _ (_, output) = output
  outputFile (f, _) = f
  isNewFile _ = True

-- When there is a file to be reprinted (for refactoring)
instance OutputFiles (F.ProgramFile Annotation, SourceText) where
  mkOutputText _ (ast@(F.ProgramFile (F.MetaInfo version _) _), input) =
     -- If we are create a file, call the pretty printer directly
     if B.null input
      then B.pack $ PP.pprintAndRender version ast (Just 0)
      -- Otherwise, applying the refactoring system with reprint
      else runIdentity $ reprint (refactoring version) ast input

  outputFile (pf, _) = F.pfGetFilename pf
  isNewFile (_, inp) = B.null inp

{- Specifies how to do specific refactorings
  (uses generic query extension - remember extQ is non-symmetric) -}

refactoring :: Typeable a
            => FortranVersion
            -> a -> SourceText -> StateT FU.Position Identity (SourceText, Bool)
refactoring v z inp = ((catchAll inp `extQ` refactoringsForProgramUnits v inp) `extQ` refactoringsForBlocks v inp) $ z
  where
    catchAll :: SourceText -> a -> StateT FU.Position Identity (SourceText, Bool)
    catchAll _ _ = return (B.empty, False)

refactoringsForProgramUnits :: FortranVersion
                            -> SourceText
                            -> F.ProgramUnit Annotation
                            -> StateT FU.Position Identity (SourceText, Bool)
refactoringsForProgramUnits v inp z =
   mapStateT (\n -> Identity $ n `evalState` 0) (refactorProgramUnits v inp z)

refactorProgramUnits :: FortranVersion
                     -> SourceText
                     -> F.ProgramUnit Annotation
                     -> StateT FU.Position (State Int) (SourceText, Bool)
-- Output comments
refactorProgramUnits _ inp (F.PUComment ann span (F.Comment comment)) = do
    cursor <- get
    if pRefactored ann
     then    let (FU.SrcSpan lb ub) = span
                 (p0, _)  = takeBounds (cursor, lb) inp
                 nl       = if null comment then B.empty else B.pack "\n"
             in (put ub >> return (B.concat [p0, B.pack comment, nl], True))
     else return (B.empty, False)

refactorProgramUnits _ _ _ = return (B.empty, False)

refactoringsForBlocks :: FortranVersion
                      -> SourceText
                      -> F.Block Annotation
                      -> StateT FU.Position Identity (SourceText, Bool)
refactoringsForBlocks v inp z =
   mapStateT (\n -> Identity $ n `evalState` 0) (refactorBlocks v inp z)

refactorBlocks :: FortranVersion
               -> SourceText
               -> F.Block Annotation
               -> StateT FU.Position (State Int) (SourceText, Bool)
-- Output comments
refactorBlocks _ inp (F.BlComment ann span (F.Comment comment)) = do
    cursor <- get
    let FU.SrcSpan lb ub     = span
        lb' | deleteNode ann = lb { FU.posColumn = 0 }
            | otherwise      = lb
        (p0, _)              = takeBounds (cursor, lb') inp
        nl | null comment ||
             deleteNode ann  = B.empty
           | otherwise       =  B.pack "\n"
    if pRefactored ann
      then put ub >> return (B.concat [p0, B.pack comment, nl], True)
      else return (B.empty, False)

-- Refactor use statements
refactorBlocks v inp b@(F.BlStatement _ _ _ u@F.StUse{}) = do
    cursor <- get
    case refactored $ F.getAnnotation u of
           Just (FU.Position _ rCol _ _ _) -> do
               let (FU.SrcSpan lb _) = FU.getSpan u
               let (p0, _) = takeBounds (cursor, lb) inp
               let out  = B.pack $ PP.pprintAndRender v b (Just (rCol -1))
               added <- lift get
               when (newNode $ F.getAnnotation u)
                    (lift $ put $ added + countLines out)
               put $ toCol0 lb
               return (p0 `B.append` out, True)
           Nothing -> return (B.empty, False)

-- Common blocks, equivalence statements, and declarations can all
-- be refactored by the default refactoring
refactorBlocks v inp (F.BlStatement _ _ _ s@F.StEquivalence{}) =
    refactorStatements v inp s
refactorBlocks v inp (F.BlStatement _ _ _ s@F.StCommon{}) =
    refactorStatements v inp s
-- Arbitrary statements can be refactored *as blocks* (in order to
-- get good indenting)
refactorBlocks v inp b@F.BlStatement {} = refactorSyntax v inp b
refactorBlocks _ _ _ = return (B.empty, False)

-- Wrapper to fix the type of refactorSyntax to deal with statements
refactorStatements :: FortranVersion -> SourceText
                   -> F.Statement A -> StateT FU.Position (State Int) (SourceText, Bool)
refactorStatements = refactorSyntax

refactorSyntax ::
   (Typeable s, F.Annotated s, FU.Spanned (s A), PP.IndentablePretty (s A))
   => FortranVersion -> SourceText
   -> s A -> StateT FU.Position (State Int) (SourceText, Bool)
refactorSyntax v inp e = do
    cursor <- get
    let a = F.getAnnotation e
    case refactored a of
      Nothing -> return (B.empty, False)
      Just (FU.Position _ rCol _ _ _) -> do
        let FU.SrcSpan lb ub     = FU.getSpan e
            lb' | deleteNode a   = lb { FU.posColumn = 0 }
                | otherwise      = lb
            (pre, _)             = takeBounds (cursor, lb') inp
        let indent | newNode a = Just (rCol - 1)
                   | otherwise = Nothing
        let output | deleteNode a = B.empty
                   | otherwise = B.pack $ PP.pprintAndRender v e indent
        out <- if newNode a then do
                  -- If a new node is begin created then
                  numAdded <- lift get
                  let diff = linesCovered ub lb
                  -- remove empty newlines here if extra lines were added
                  let (out, numRemoved) = if numAdded <= diff
                                           then removeNewLines output numAdded
                                           else removeNewLines output diff
                  lift $ put (numAdded - numRemoved)
                  return out
                else return output
        put $ if FU.posColumn ub == 1
              then ub else ub { FU.posLine = FU.posLine ub + 1, FU.posColumn = 1 }
        return (B.concat [pre, out], True)

countLines :: B.ByteString -> Int
countLines = B.count '\n'

{- 'removeNewLines xs n' removes at most 'n' new lines characters from
the input string xs, returning the new string and the number of new
lines that were removed. Note that the number of new lines removed
might actually be less than 'n'- but in principle this should not
happen with the usaage in 'refactorDecl' -}

removeNewLines :: B.ByteString -> Int -> (B.ByteString, Int)
removeNewLines xs 0 = (xs, 0)
-- Deal with CR LF in the same way as just LF
removeNewLines topXS n =
    case unpackFst (B.splitAt 4 topXS) of
      ("\r\n\r\n", xs) -> (xs', n' + 1)
          where (xs', n') = removeNewLines (B.pack "\r\n" `B.append` xs) (n - 1)
      _ ->
        case unpackFst (B.splitAt 2 topXS) of
          ("\n\n", xs)     -> (xs', n' + 1)
              where (xs', n') = removeNewLines (B.pack "\n" `B.append` xs) (n - 1)
          _ ->
           case B.uncons topXS of
               Nothing -> (topXS, 0)
               Just (x, xs) -> (B.cons x xs', n)
                   where (xs', _) = removeNewLines xs n

unpackFst :: (B.ByteString, b) -> (String, b)
unpackFst (x, y) = (B.unpack x, y)
