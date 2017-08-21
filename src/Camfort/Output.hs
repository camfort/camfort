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

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.PrettyPrint as PP
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.ParserMonad as FPM

import Camfort.Analysis.Annotations
import qualified Camfort.Reprint as R
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
      else runIdentity $ R.reprint (refactoring version) ast input

  outputFile (pf, _) = F.pfGetFilename pf
  isNewFile (_, inp) = B.null inp

{- Specifies how to do specific refactorings
  (uses generic query extension - remember extQ is non-symmetric) -}

refactoring :: Typeable a
            => FPM.FortranVersion
            -> a -> Identity R.ReprintingOut
refactoring v z = R.catchAll
                    `extQ` refactoringsForProgramUnits v
                    `extQ` refactoringsForBlocks v $ z

refactoringsForProgramUnits :: FPM.FortranVersion
                            -> F.ProgramUnit Annotation
                            -> Identity (Maybe (R.RefactorType, SourceText, (FU.Position, FU.Position)))
refactoringsForProgramUnits v z =
   Identity $ evalState (refactorProgramUnits v z) 0

refactorProgramUnits :: FPM.FortranVersion
                     -> F.ProgramUnit Annotation
                     -> State Int R.ReprintingOut
-- Output comments
refactorProgramUnits _ (F.PUComment ann (FU.SrcSpan lb ub) (F.Comment comment)) = do
    if pRefactored ann
     then    let nl = if null comment then B.empty else B.pack "\n"
             in return $ Just (R.Replace, B.concat [B.pack comment, nl], (lb, ub))
     else return Nothing

refactorProgramUnits _ _ = return Nothing

refactoringsForBlocks :: FPM.FortranVersion
                      -> F.Block Annotation
                      -> Identity R.ReprintingOut
refactoringsForBlocks v z =
   Identity $ evalState (refactorBlocks v z) 0

refactorBlocks :: FPM.FortranVersion
               -> F.Block Annotation
               -> State Int R.ReprintingOut
-- Output comments
refactorBlocks _ (F.BlComment ann span (F.Comment comment)) = do
    if pRefactored ann
     then    let (FU.SrcSpan lb ub) = span
                 nl       = if null comment then B.empty else B.pack "\n"
             in return $ Just (R.Replace, B.concat [B.pack comment, nl], (lb, ub))
     else return Nothing

-- Refactor use statements
refactorBlocks v b@(F.BlStatement _ _ _ u@F.StUse{}) = do
    case refactored $ F.getAnnotation u of
           Just (FU.Position _ rCol _) -> do
               let (FU.SrcSpan lb _) = FU.getSpan u
               let out  = B.pack $ PP.pprintAndRender v b (Just (rCol -1))
               added <- get
               when (newNode $ F.getAnnotation u)
                    (put $ added + countLines out)
               return $ Just (R.Replace, out, (lb, toCol0 lb))
           Nothing -> return Nothing

-- Common blocks, equivalence statements, and declarations can all
-- be refactored by the default refactoring
refactorBlocks v (F.BlStatement _ _ _ s@F.StEquivalence{}) =
    refactorStatements v s
refactorBlocks v (F.BlStatement _ _ _ s@F.StCommon{}) =
    refactorStatements v s
refactorBlocks v (F.BlStatement _ _ _ s@F.StDeclaration{}) =
    refactorStatements v s
-- Arbitrary statements can be refactored *as blocks* (in order to
-- get good indenting)
refactorBlocks v b@F.BlStatement {} = refactorSyntax v b
refactorBlocks _ _ = return Nothing

-- Wrapper to fix the type of refactorSyntax to deal with statements
refactorStatements :: FPM.FortranVersion
                   -> F.Statement A -> State Int R.ReprintingOut
refactorStatements = refactorSyntax

refactorSyntax ::
   (Typeable s, F.Annotated s, FU.Spanned (s A), PP.IndentablePretty (s A))
   => FPM.FortranVersion
   -> s A -> State Int R.ReprintingOut
refactorSyntax v e = do
    let a = F.getAnnotation e
    case refactored a of
      Nothing -> return Nothing
      Just (FU.Position _ rCol _) -> do
        let (FU.SrcSpan lb ub) = FU.getSpan e

        let indent = if newNode a then Just (rCol - 1) else Nothing
        let output = if deleteNode a then B.empty
                                     else B.pack $ PP.pprintAndRender v e indent
        out <- if newNode a then do
                  -- If a new node is begin created then
                  numAdded <- get
                  let diff = linesCovered ub lb
                  -- remove empty newlines here if extra lines were added
                  let (out, numRemoved) = if numAdded <= diff
                                           then removeNewLines output numAdded
                                           else removeNewLines output diff
                  put (numAdded - numRemoved)
                  return out
                else return output
        return $ Just (R.Replace, out, (lb, ub))

countLines xs =
  case B.uncons xs of
    Nothing -> 0
    Just ('\n', xs) -> 1 + countLines xs
    Just (_, xs)    -> countLines xs

{- 'removeNewLines xs n' removes at most 'n' new lines characters from
the input string xs, returning the new string and the number of new
lines that were removed. Note that the number of new lines removed
might actually be less than 'n'- but in principle this should not
happen with the usaage in 'refactorDecl' -}

removeNewLines xs 0 = (xs, 0)
-- Deal with CR LF in the same way as just LF
removeNewLines xs n =
    case unpackFst (B.splitAt 4 xs) of
      ("\r\n\r\n", xs) -> (xs', n' + 1)
          where (xs', n') = removeNewLines (B.pack "\r\n" `B.append` xs) (n - 1)
      _ ->
        case unpackFst (B.splitAt 2 xs) of
          ("\n\n", xs)     -> (xs', n' + 1)
              where (xs', n') = removeNewLines (B.pack "\n" `B.append` xs) (n - 1)
          _ ->
           case B.uncons xs of
               Nothing -> (xs, 0)
               Just (x, xs) -> (B.cons x xs', n)
                   where (xs', _) = removeNewLines xs n

unpackFst (x, y) = (B.unpack x, y)
