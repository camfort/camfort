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
{-# LANGUAGE TypeApplications #-}

module Camfort.Analysis.CommentAnnotator
  ( annotateComments
  , isComment
  , ASTEmbeddable(..)
  , Linkable(..)
  ) where


import Data.Data (Data)
import Data.Generics.Uniplate.Data

import Language.Fortran.AST
import Language.Fortran.Util.Position

import Camfort.Specification.Parser ( looksLikeASpec
                                    , runParser
                                    , SpecParseError
                                    , SpecParser)

annotateComments :: forall m e a ast .
  (Monad m, Data a, Linkable a, ASTEmbeddable a ast)
  => SpecParser e ast
  -> (SrcSpan -> SpecParseError e -> m ())
  -> ProgramFile a
  -> m (ProgramFile a)
annotateComments parser handleErr pf = do
    pf' <- transformBiM writeASTProgramUnits =<< transformBiM writeASTBlocks pf
    return . descendBi linkProgramUnits $ descendBi linkBlocks pf'
  where
    writeAST a d srcSpan comment =
      if looksLikeASpec parser comment
      then case runParser parser comment of
             Left  err -> handleErr srcSpan err >> pure d
             Right ast -> pure $ setAnnotation (annotateWithAST a ast) d
      else pure d

    writeASTProgramUnits :: ProgramUnit a -> m (ProgramUnit a)
    writeASTProgramUnits pu@(PUComment a srcSpan (Comment comment)) =
      writeAST a pu srcSpan comment
    writeASTProgramUnits pu = pure pu

    writeASTBlocks :: Block a -> m (Block a)
    writeASTBlocks b@(BlComment a srcSpan (Comment comment)) =
      writeAST a b srcSpan comment
    writeASTBlocks b = pure b

    {-| Link all comment blocks to first non-comment block in the list. |-}
    linkBlocks :: (Data a, Linkable a) => [ Block a ] -> [ Block a ]
    linkBlocks = joinComments

    {-| Link all comment 'program units' to first non-comment program unit in the list. |-}
    linkProgramUnits :: (Data a, Linkable a) => [ ProgramUnit a ] -> [ ProgramUnit a ]
    linkProgramUnits = joinComments

-- | Link all comments to first non-comment in the list.
joinComments
    :: forall f a. (HasComment (f a), Linked f, Linkable a, Functor f, Data (f a))
    => [f a] -> [f a]
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
  | otherwise = descendBi @(f a) @[f a] joinComments d : joinComments ds

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
