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

module Camfort.Analysis.CommentAnnotator
  ( annotateComments
  , Logger
  , ASTEmbeddable(..)
  , Linkable(..)
  , AnnotationParseError(..)
  , AnnotationParser
  , failWith
  ) where


import Control.Monad.Writer.Strict (Writer, tell)
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
    pf' <- transformBiM writeASTProgramUnits =<< transformBiM writeASTBlocks pf
    return . descendBi linkProgramUnits $ descendBi linkBlocks pf'
  where
    writeAST a d srcSpan comment =
      case parse comment of
        Right ast -> return $ setAnnotation (annotateWithAST a ast) d
        Left NotAnnotation -> return d
        Left (ProbablyAnnotation err) -> parserWarn srcSpan err >> return d

    writeASTProgramUnits :: (Data a) => ProgramUnit a -> Logger (ProgramUnit a)
    writeASTProgramUnits pu@(PUComment a srcSpan (Comment comment)) =
      writeAST a pu srcSpan comment
    writeASTProgramUnits pu = return pu

    writeASTBlocks :: (Data a) => Block a -> Logger (Block a)
    writeASTBlocks b@(BlComment a srcSpan (Comment comment)) =
      writeAST a b srcSpan comment
    writeASTBlocks b = return b

    -- | Link all comments to first non-comment in the list.
    joinComments [ ] = [ ]
    joinComments dss@(d:ds)
      | isComment d =
        let (comments, rest) = span isComment dss
            -- Given a list of comments and a list of non-comment blocks which occur
            -- afterward in the code, then link them together (either forward or backward)
            -- returning a pair of processed blocks and unprocessed blocks

            -- pre-condition: first parameter is a list of comments

            -- default uses 'link' to associate every comment to the first following block
            linkMulti = (map (fmap $ flip linker (head rest)) comments, rest)
        in if null rest -- Does the group end with comments
             then comments
             else let (procs, unprocs) = linkMulti
                  in procs ++ joinComments unprocs
      | otherwise = descendBi joinComments d
                    : joinComments ds

    {-| Link all comment blocks to first non-comment block in the list. |-}
    linkBlocks :: (Data a, Linkable a) => [ Block a ] -> [ Block a ]
    linkBlocks = joinComments

    {-| Link all comment 'program units' to first non-comment program unit in the list. |-}
    linkProgramUnits :: (Data a, Linkable a) => [ ProgramUnit a ] -> [ ProgramUnit a ]
    linkProgramUnits = joinComments

class ASTEmbeddable a ast where
  annotateWithAST :: a -> ast -> a

-- | Instances of this class can be combined with 'Block' and 'ProgramUnit'.
class Linkable a where
  -- ^ Combine an @a@ with a 'Block'
  link   :: a   -> Block a -> a
  -- ^ Combine an @a@ with a 'ProgramUnit'
  linkPU :: a -> ProgramUnit a -> a

-- | Interface for types that can be combined with 'Linkable' types.
class Linked a where
  linker :: (Linkable b) => b -> a b -> b

instance Linked Block where
  linker = link

instance Linked ProgramUnit where
  linker = linkPU

-- | Interface for types that can have comments.
class HasComment a where
  isComment :: a -> Bool

instance HasComment (Block a) where
  isComment BlComment{} = True
  isComment _           = False

instance HasComment (ProgramUnit a) where
  isComment PUComment{} = True
  isComment _           = False

parserWarn :: SrcSpan -> String -> Logger ()
parserWarn srcSpan err = tell [ "Error " ++ show srcSpan ++ ": " ++ err ]
