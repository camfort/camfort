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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Camfort.Traverse where

import Camfort.Analysis.Annotations
import Language.Fortran

import Generics.Deriving.Base
import Generics.Deriving.Copoint
import GHC.Generics

import Control.Monad.Trans.Writer.Lazy

import Data.Generics.Zipper
import Data.Generics.Aliases
import Data.Generics.Str
import Data.Generics.Uniplate.Operations

import Language.Fortran.Lexer

import Control.Comonad

import Data.Data
import Data.Maybe
import Data.Monoid

import Debug.Trace

-- Data-type generic comonad-style traversal

extendBi :: (Biplate (from a) (to a), RComonad to) => (to a -> a) -> (from a) -> (from a)
extendBi f x = case biplate x of
                     (current, generate) -> generate $ strMap (rextend f) current

reduceCollect :: (Data s, Data t, Uniplate t, Biplate t s) => (s -> Maybe a) -> t -> [a]
reduceCollect k x = execWriter (transformBiM (\y -> do case k y of
                                                         Just x -> tell [x]
                                                         Nothing -> return ()
                                                       return y) x)

-- Data-type generic comonad-style traversal with zipper (contextual traversal)

everywhere :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
everywhere k z = let everywhere' = enterRight . enterDown . k

                     enterDown z = case (down' z) of
                                     Just dz -> let dz' = everywhere' dz
                                                in case (up $ dz') of
                                                     Just uz -> uz
                                                     Nothing -> dz'
                                     Nothing -> z

                     enterRight z = case (right z) of
                                      Just rz -> let rz' = everywhere' rz
                                                 in case (left $ rz') of
                                                     Just lz -> lz
                                                     Nothing -> rz'
                                      Nothing -> z
                  in everywhere' z

zfmap :: Data a => (a -> a) -> Zipper (d a) -> Zipper (d a)
zfmap f x = zeverywhere (mkT f) x

-- This one is less useful as the definitions for comonads are then very annoying

extendBi' :: (Biplate (from a) (to a), Comonad to) => (to a -> a) -> (from a) -> (from a)
extendBi' f x = case biplate x of
                     (current, generate) -> generate $ strMap (extend f) current

class RComonad t where
    rextract :: t a -> a
    rextend :: (t a -> a) -> t a -> t a

class RFunctor t where
    rfmap :: (a -> a) -> t a -> t a

instance RComonad Fortran where
    rextract x = tag x

    rextend k y@(Assg _ sp e1 e2)        = Assg (k y) sp e1 e2
    rextend k y@(For _ sp v e1 e2 e3 fs) = For (k y) sp v e1 e2 e3 (rextend k fs)
    rextend k y@(FSeq _ sp f1 f2)        = FSeq (k y) sp (rextend k f1) (rextend k f2)
    rextend k y@(If _ sp e f1 fes f3)    = let fes' = map (\(e, f) -> (e, rextend k f)) fes
                                               f3' = case f3 of
                                                    Nothing -> Nothing
                                                    Just f3a -> Just (rextend k f3a)
                                            in If (k y) sp e (rextend k f1) fes' f3'
    rextend k y@(Allocate _ sp e1 e2)      = Allocate (k y) sp e1 e2
    rextend k y@(Backspace _ sp sp')        = Backspace (k y) sp sp'
    rextend k y@(Call _ sp e as)           = Call (k y) sp e as
    rextend k y@(Open _ sp s)              = Open (k y) sp s
    rextend k y@(Close _ sp s)             = Close (k y) sp s
    rextend k y@(Continue _ sp)            = Continue (k y) sp
    rextend k y@(Cycle _ sp s)             = Cycle (k y) sp s
    rextend k y@(Deallocate _ sp es e)     = Deallocate (k y) sp es e
    rextend k y@(Endfile _ sp s)           = Endfile (k y) sp s
    rextend k y@(Exit _ sp s)              = Exit (k y) sp s
    rextend k y@(Forall _ sp es f)         = Forall (k y) sp es (rextend k f)
    rextend k y@(Goto _ sp s)              = Goto (k y) sp s
    rextend k y@(Nullify _ sp e)           = Nullify (k y) sp e
    rextend k y@(Inquire _ sp s e)         = Inquire (k y) sp s e
    rextend k y@(Rewind _ sp s)            = Rewind (k y) sp s
    rextend k y@(Stop _ sp e)              = Stop (k y) sp e
    rextend k y@(Where _ sp e f Nothing)   = Where (k y) sp e (rextend k f) Nothing
    rextend k y@(Where _ sp e f (Just f')) = Where (k y) sp e (rextend k f) (Just (rextend k f'))
    rextend k y@(Write _ sp s e)           = Write (k y) sp s e
    rextend k y@(PointerAssg _ sp e1 e2)   = PointerAssg (k y) sp e1 e2
    rextend k y@(Return _ sp e)            = Return (k y) sp e
    rextend k y@(Label _ sp s f)           = Label (k y) sp s (rextend k f)
    rextend k y@(Print _ sp e es)          = Print (k y) sp e es
    rextend k y@(ReadS _ sp s e)           = ReadS (k y) sp s e
    rextend k y@(TextStmt _ sp s)          = TextStmt (k y) sp s
    rextend k y@(NullStmt _ sp)            = NullStmt (k y) sp

class Refill d where
    refill :: d a -> a -> d a

instance Refill Fortran where
    refill y@(Assg _ sp e1 e2)         a = Assg a sp e1 e2
    refill y@(For _ sp v e1 e2 e3 fs)  a = For a sp v e1 e2 e3 fs
    refill y@(DoWhile _ sp e f)        a = DoWhile a sp e f
    refill y@(FSeq _ sp f1 f2)         a = FSeq a sp f1 f2
    refill y@(If _ sp e f1 fes f3)     a = If a sp e f1 fes f3
    refill y@(Allocate _ sp e1 e2)     a = Allocate a sp e1 e2
    refill y@(Backspace _ sp sp')      a = Backspace a sp sp'
    refill y@(Call _ sp e as)          a = Call a sp e as
    refill y@(Open _ sp s)             a = Open a sp s
    refill y@(Close _ sp s)            a = Close a sp s
    refill y@(Continue _ sp)           a = Continue a sp
    refill y@(Cycle _ sp s)            a = Cycle a sp s
    refill y@(DataStmt _ sp p)         a = DataStmt a sp p
    refill y@(Deallocate _ sp es e)    a = Deallocate a sp es e
    refill y@(Endfile _ sp s)          a = Endfile a sp s
    refill y@(Exit _ sp s)             a = Exit a sp s
    refill y@(Forall _ sp es f)        a = Forall a sp es f
    refill y@(Format _ sp s)           a = Format a sp s
    refill y@(Goto _ sp s)             a = Goto a sp s
    refill y@(Nullify _ sp e)          a = Nullify a sp e
    refill y@(Inquire _ sp s e)        a = Inquire a sp s e
    refill y@(Pause _ sp s)            a = Pause a sp s
    refill y@(Rewind _ sp s)           a = Rewind a sp s
    refill y@(Stop _ sp e)             a = Stop a sp e
    refill y@(Where _ sp e f f')       a = Where a sp e f f'
    refill y@(Write _ sp s e)          a = Write a sp s e
    refill y@(PointerAssg _ sp e1 e2)  a = PointerAssg a sp e1 e2
    refill y@(Return _ sp e)           a = Return a sp e
    refill y@(Label _ sp s f)          a = Label a sp s f
    refill y@(Print _ sp e es)         a = Print a sp e es
    refill y@(ReadS _ sp s e)          a = ReadS a sp s e
    refill y@(TextStmt _ sp s)         a = TextStmt a sp s
    refill y@(NullStmt _ sp)           a = NullStmt a sp


annotation :: Tagged g => g a -> a
annotation = tag
