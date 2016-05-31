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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

module Camfort.Analysis.CommentAnnotator ( annotateComments
                                         , Logger
                                         , ASTEmbeddable(..)
                                         , Linkable(..)
                                         , AnnotationParseError(..)
                                         , AnnotationParser
                                         , failWith
                                         ) where

import Control.Monad.Writer.Lazy (Writer(..), tell)
import Data.Generics.Uniplate.Operations
import Data.Data (Data)

import Language.Fortran.AST
import Language.Fortran.Util.Position

type Logger = Writer [ String ]
type AnnotationParser ast = String -> Either AnnotationParseError ast

data AnnotationParseError =
    NotAnnotation
  | ProbablyAnnotation String
  deriving (Eq, Show)

-- A parser that throws an annotation parsing error
failWith :: AnnotationParser ast
failWith = Left . ProbablyAnnotation

annotateComments :: forall a ast . (Data a, Linkable a, ASTEmbeddable a ast)
                                 => AnnotationParser ast
                                 -> ProgramFile a
                                 -> Logger (ProgramFile a)
annotateComments parse pf = do
    pf' <- transformBiM (writeAST parse) pf
    return $ transformBi linkBlocks pf'
  where
    writeAST :: (Data a, ASTEmbeddable a ast)
             => AnnotationParser ast -> Block a -> Logger (Block a)
    writeAST parse b@(BlComment a srcSpan comment) =
      case parse comment of
        Right ast -> return $ setAnnotation (annotateWithAST a ast) b
        Left NotAnnotation -> return b
        Left (ProbablyAnnotation err) -> parserWarn srcSpan err >> return b
    writeAST _ b = return b

    {-| Link all comment blocks to first non-comment block in the list. |-}
    linkBlocks :: (Data a, Linkable a) => [ Block a ] -> [ Block a ]
    linkBlocks [ ] = [ ]
    linkBlocks [ x ] = [ x ]
    linkBlocks blocks@(b:bs)
      | BlComment{} <- b =
        let (comments, rest) = span isComment blocks
        in if null rest -- Does the group of blocks end with comments
             then comments
             else map (fmap $ flip link (head rest)) comments ++ linkBlocks rest
      | otherwise = b : linkBlocks bs
      where
        isComment BlComment{} = True
        isComment _ = False

class ASTEmbeddable a ast where
  annotateWithAST :: a -> ast -> a

class Linkable a where
  link :: a -> Block a -> a

parserWarn :: SrcSpan -> String -> Logger ()
parserWarn srcSpan err = tell [ "Error " ++ show srcSpan ++ ": " ++ err ]
