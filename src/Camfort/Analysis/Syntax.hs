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
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             FlexibleContexts, GADTs, DeriveGeneric #-}

{-|

This module provides a number of helper functions for working with Fortran syntax that are useful
between different analyses and transformations.

-}
module Camfort.Analysis.Syntax where

-- Standard imports
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Debug.Trace

-- Data-type generics imports
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Zipper
import Data.Typeable

-- CamFort specific functionality
import Camfort.Analysis.Annotations
import Camfort.Traverse
import Language.Fortran

import qualified Language.Fortran.AST as F
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

-- * Comparison and ordering

{-|  'AnnotationFree' is a data type that wraps other types and denotes terms
     which should  be compared for equality modulo their annotations and source
     location information -}
data AnnotationFree t = AnnotationFree { annotationBound :: t } deriving Show

{-| short-hand constructor for 'AnnotationFree' -}
af = AnnotationFree
{-| short-hand deconstructor for 'AnnotationFree' -}
unaf = annotationBound

{-| A helpful function, used by the 'Eq AnnotationFree' instance that
     resets and source  location information -}
eraseSrcLocs :: (Typeable (t a), Data (t a)) => t a -> t a
eraseSrcLocs =
    transformBi erase'
  where
    erase' :: SrcLoc -> SrcLoc
    erase' _ = SrcLoc { srcFilename = "", srcLine = 0, srcColumn = 0 }

{-| Sets the @SrcLoc@ information to have the filename "compact" which triggers a special
  compact form of pretty printing in the @Show SrcLoc@ instances -}
setCompactSrcLocs :: (Typeable (t a), Data (t a)) => t a -> t a
setCompactSrcLocs =
    transformBi cmpact'
  where
    cmpact' :: SrcLoc -> SrcLoc
    cmpact' (SrcLoc _ l c) = SrcLoc { srcFilename = "compact", srcLine = l, srcColumn = c }

lower = map toLower

-- Here begins varioous 'Eq' instances for instantiations of 'AnnotationFree'

instance Eq (AnnotationFree a) => Eq (AnnotationFree [a]) where
    (AnnotationFree xs) == (AnnotationFree xs') =
               if (length xs == length xs')
               then foldl (\b -> \(x, x') -> ((af x) == (af x')) && b) True (zip xs xs')
               else False

instance Eq (AnnotationFree Int) where
    x == y = (unaf x) == (unaf y)

instance Eq (AnnotationFree Char) where
    x == y = (unaf x) == (unaf y)

instance Eq (AnnotationFree (AccessP ())) where
    x == y = (unaf x) == (unaf y)

instance (Eq (AnnotationFree a), Eq (AnnotationFree b)) => Eq (AnnotationFree (a, b)) where
    (AnnotationFree (x, y)) == (AnnotationFree (x', y')) = ((af x) == (af x')) && ((af y) == (af y'))

instance Eq a => Eq (AnnotationFree (F.Expression a)) where
    (AnnotationFree x) == (AnnotationFree y) = x == y''
        where y'' = setSecondParameter (getSecondParameter x) y'
              y' = setFirstParameter (getFirstParameter x) y

{-| Partial-ordering for expressions (constructors only so far), ignores annotations -}
instance Eq p => Ord (Expr p) where
    (Con _ _ c) <= (Con  _ _ c') = c <= c'
    e <= e'                      = error "Ordering on expressions only for constructors so far"

-- * Accessor functions for extracting various pieces of information out of syntax trees


{-| Extracts a string of the (root) variable name from a variable expression (if it is indeed a variable
    expression -}
varExprToVariableF :: F.Expression a -> Maybe F.Name
varExprToVariableF (F.ExpValue _ _ (F.ValVariable v)) = Just v
varExprToVariableF _                                  = Nothing

{-| Set a default monoid instances for Int -}
instance Monoid Int where
    mempty = 0
    mappend = (+)


-- * An embedded domain-specific language for describing syntax tree queries
{-| 'QueryCmd' provides 'commands' of which pieces of syntax to find -}

data QueryCmd t where
    Exprs  :: QueryCmd (Expr Annotation)
    Blocks :: QueryCmd (Block Annotation)
    Decls  :: QueryCmd (Decl Annotation)
    Locs   :: QueryCmd Access
    Vars   :: QueryCmd (Expr Annotation)

{-| 'from' takes a command as its first parameter, a piece of syntax as its second, and
     returns all pieces of syntax matching the query request.

     For example: @from Decls x@ returns a list of all declarations in @x@, of type @[Decl Annotation]@
     If @x@ is itself a declaration then this is returned as well (so be careful with recursive functions
     over things defined in turns of 'from'. See 'topFrom' for a solution to this.
-}
from :: forall t synTyp . (Data t, Data synTyp) => QueryCmd synTyp -> t -> [synTyp]
from Locs x = accesses x
from Vars x = [v | v@(Var _ _ _) <- (universeBi x)::[Expr Annotation]]
from _ x = (universeBi x)::[synTyp]

{-| 'topFrom' takes a command as first parameter, a piece of syntax as its second, and
     returns all pieces of syntax matching the query request that are *children* of the current
     piece of syntax. This means that it will not return itself. -}

topFrom :: forall t synTyp . (Data t, Data synTyp) => QueryCmd synTyp -> t -> [synTyp]
topFrom Locs x = accesses x
topFrom _ x = (childrenBi x)::[synTyp]
