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

module Camfort.Analysis.CommentAnnotator
  ( annotateComments
  , Logger
  , ASTEmbeddable(..)
  , Linkable(..)
  , AnnotationParseError(..)
  , AnnotationParser
  , failWith
  ) where


import Control.Monad.Writer.Strict (Writer(..), tell)
import Data.Generics.Uniplate.Operations
import Data.Data (Data)
import Debug.Trace

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
    pf' <- transformBiM (writeASTProgramUnits parse) =<< transformBiM (writeASTBlocks parse) pf
    return . descendBi linkProgramUnits $ descendBi linkBlocks pf'
  where
    writeASTBlocks :: (Data a, ASTEmbeddable a ast) => AnnotationParser ast -> Block a -> Logger (Block a)
    writeASTBlocks parse b@(BlComment a srcSpan (Comment comment)) =
      case parse comment of
        Right ast -> return $ setAnnotation (annotateWithAST a ast) b
        Left NotAnnotation -> return b
        Left (ProbablyAnnotation err) -> parserWarn srcSpan err >> return b
    writeASTBlocks _ b = return b

    writeASTProgramUnits :: (Data a, ASTEmbeddable a ast) => AnnotationParser ast -> ProgramUnit a -> Logger (ProgramUnit a)
    writeASTProgramUnits parse pu@(PUComment a srcSpan (Comment comment)) =
      case parse comment of
        Right ast -> return $ setAnnotation (annotateWithAST a ast) pu
        Left NotAnnotation -> return pu
        Left (ProbablyAnnotation err) -> parserWarn srcSpan err >> return pu
    writeASTProgramUnits _ pu = return pu

    {-| Link all comment blocks to first non-comment block in the list. |-}
    linkBlocks :: (Data a, Linkable a) => [ Block a ] -> [ Block a ]
    linkBlocks [ ] = [ ]
    linkBlocks blocks@(b:bs)
      | BlComment{} <- b =
        let (comments, rest) = span isComment blocks
        in if null rest -- Does the group of blocks end with comments
             then comments
             else let (bs, bs') = linkMultiple comments rest
                  in bs ++ linkBlocks bs'
      | otherwise = (descendBi linkBlocks b) : linkBlocks bs
      where
        isComment BlComment{} = True
        isComment _           = False

    {-| Link all comment 'program units' to first non-comment program unit in the list. |-}
    linkProgramUnits :: (Data a, Linkable a) => [ ProgramUnit a ] -> [ ProgramUnit a ]
    linkProgramUnits [ ] = [ ]
    linkProgramUnits programUnits@(pu:pus)
      | PUComment{} <- pu =
        let (comments, rest) = span isComment programUnits
        in if null rest -- Does the group of blocks end with comments
             then comments
             else let (procPUs, unprocPUs) = linkMultiplePUs comments rest
                  in procPUs ++ linkProgramUnits unprocPUs
      | otherwise = (descendBi linkProgramUnits pu) : linkProgramUnits pus
      where
        isComment PUComment{} = True
        isComment _           = False

class ASTEmbeddable a ast where
  annotateWithAST :: a -> ast -> a

class Linkable a where
  link   :: a   -> Block a -> a
  linkPU :: a -> ProgramUnit a -> a

  -- Given a list of comments and a list of non-comment blocks which occur
  -- afterward in the code, then link them together (either forward or backward)
  -- returning a pair of processed blocks and unprocessed blocks

  -- pre-condition: first parameter is a list of comments

  -- default uses 'link' to associate every comment to the first following block
  linkMultiple :: [Block a] -> [Block a] -> ([Block a], [Block a])
  linkMultiple comments blocks =
     (map (fmap $ flip link (head blocks)) comments, blocks)

  linkMultiplePUs :: [ProgramUnit a] -> [ProgramUnit a] -> ([ProgramUnit a], [ProgramUnit a])
  linkMultiplePUs comments pus = -- trace (show (map (fmap (const ())) comments, (map (fmap (const ())) pus))) $
     (map (fmap $ flip linkPU (head pus)) comments, pus)

parserWarn :: SrcSpan -> String -> Logger ()
parserWarn srcSpan err = tell [ "Error " ++ show srcSpan ++ ": " ++ err ]
