{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Camfort.Helpers.TypeLevel where

import Data.Functor.Identity

import Language.Expression.Pretty

--------------------------------------------------------------------------------
--  Types
--------------------------------------------------------------------------------

data Some f where
  Some :: f a -> Some f

data PairOf f g a where
  PairOf :: f a -> g a -> PairOf f g a

--------------------------------------------------------------------------------
--  Patterns
--------------------------------------------------------------------------------

pattern SomePair :: f a -> g a -> Some (PairOf f g)
pattern SomePair x y = Some (PairOf x y)

--------------------------------------------------------------------------------
--  Combinators
--------------------------------------------------------------------------------

traverseSome
  :: Applicative m
  => (forall a. f a -> m (g a))
  -> Some f -> m (Some g)
traverseSome f (Some x) = Some <$> f x

traversePairOf
  :: Applicative m
  => (f a -> g a -> m (f' b, g' b))
  -> PairOf f g a -> m (PairOf f' g' b)
traversePairOf f (PairOf x y) = uncurry PairOf <$> f x y

mapSome :: (forall a. f a -> g a) -> Some f -> Some g
mapSome f = runIdentity . traverseSome (Identity . f)

--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance Pretty1 f => Pretty (Some f) where
  prettysPrec p = \case
    Some x -> prettys1Prec p x

instance Pretty1 g => Pretty1 (PairOf f g) where
  prettys1Prec p = \case
    PairOf _ x -> prettys1Prec p x

instance Pretty1 f => Show (Some f) where
  show = pretty
