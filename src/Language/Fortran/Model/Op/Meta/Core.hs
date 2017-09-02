{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wall      #-}

module Language.Fortran.Model.Op.Meta.Core where

-- import           Data.Int                        (Int16, Int32, Int64, Int8)
-- import           Data.Word                       (Word8)

-- import           Control.Monad.Reader.Class      (MonadReader (ask))

-- import           Data.SBV
-- import           Data.SBV.Dynamic
-- import           Data.SBV.Internals              (SBV (..))

-- import           Language.Expression
-- import           Language.Expression.Pretty

-- import           Language.Fortran.Model.EvalPrim
-- import           Language.Fortran.Model.Types
-- import           Language.Fortran.Model.MetaOp.Repr


-- | Operations that can't actually occur in Fortran but which nevertheless are
-- used in converting it to a logical representation. E.g. immutable array update.
data MetaOp t a where


-- | High-level calculations and assertions over high-level data. E.g. folds
-- over arrays and high-level mathematical functions such as factorial.
data HighOp t a where
