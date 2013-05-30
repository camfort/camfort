> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}

 {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE ScopedTypeVariables #-}

 {-# LANGUAGE OverlappingInstances #-}

 {-# LANGUAGE ImplicitParams #-}
 {-# LANGUAGE KindSignatures #-}

> module Traverse where

> import Language.Fortran

> import Generics.Deriving.Base
> import GHC.Generics

> import Data.Generics.Str
> import Data.Generics.Uniplate.Operations

> import Control.Comonad

> class Copoint d where
>     copoint :: d a -> a

> instance Copoint SubName where
>     copoint (SubName x _) = x
>     copoint (NullSubName x) = x

> instance Copoint VarName where
>     copoint (VarName x _) = x

> instance Copoint ArgName where
>     copoint (ArgName x _) = x
>     copoint (ASeq x _ _) = x
>     copoint (NullArg x) = x

> instance Copoint Fortran where
>     copoint (Assg x e1 e2)        = x
>     copoint (For x v e1 e2 e3 fs) = x
>     copoint (FSeq x f1 f2)        = x
>     copoint (If x e f1 fes f3)    = x
>     copoint (Allocate x e1 e2)    = x
>     copoint (Backspace x sp)      = x
>     copoint (Call x e as)         = x
>     copoint (Open x s)            = x
>     copoint (Close x s)           = x 
>     copoint (Continue x)          = x
>     copoint (Cycle x s)           = x
>     copoint (Deallocate x es e)   = x
>     copoint (Equivalence x _)     = x
>     copoint (Endfile x s)         = x
>     copoint (Exit x s)            = x
>     copoint (Forall x es f)       = x
>     copoint (Goto x s)            = x
>     copoint (Nullify x e)         = x
>     copoint (Inquire x s e)       = x
>     copoint (Rewind x s)          = x 
>     copoint (Stop x e)            = x
>     copoint (Where x e f)         = x 
>     copoint (Write x s e)         = x
>     copoint (PointerAssg x e1 e2) = x
>     copoint (Return x e)          = x
>     copoint (Label x s f)         = x
>     copoint (Print x e es)        = x
>     copoint (ReadS x s e)         = x
>     copoint (TextStmt x s)        = x
>     copoint (NullStmt x)          = x

Other helpers

> fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
> fanout f g x = (f x, g x)

> prod :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> prod f g (x, y) = (f x, g y)

> mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
> mfmap f = map (fmap f)


Data-type generic comonad-style traversal

> extendBi :: (Biplate (from a) (to a), RComonad to) => (to a -> a) -> (from a) -> (from a)
> extendBi f x = case biplate x of
>                      (current, generate) -> generate $ strMap (rextend f) current

This one is less useful as the definitions for comonads are then very annoying

> extendBi' :: (Biplate (from a) (to a), Comonad to) => (to a -> a) -> (from a) -> (from a)
> extendBi' f x = case biplate x of
>                      (current, generate) -> generate $ strMap (extend f) current

> class RComonad t where
>     rextract :: t a -> a
>     rextend :: (t a -> a) -> t a -> t a

> instance RComonad Fortran where
>     rextract x = copoint x

>     rextend k y@(Assg _ e1 e2)        = Assg (k y) e1 e2
>     rextend k y@(For _ v e1 e2 e3 fs) = For (k y) v e1 e2 e3 (rextend k fs)
>     rextend k y@(FSeq _ f1 f2)        = FSeq (k y) (rextend k f1) (rextend k f2)
>     rextend k y@(If _ e f1 fes f3)    = let fes' = map (\(e, f) -> (e, rextend k f)) fes
>                                             f3' = case f3 of 
>                                                     Nothing -> Nothing
>                                                     Just f3a -> Just (rextend k f3a)
>                                         in If (k y) e (rextend k f1) fes' f3'
>     rextend k y@(Allocate _ e1 e2)      = Allocate (k y) e1 e2
>     rextend k y@(Backspace _ sp)        = Backspace (k y) sp
>     rextend k y@(Call _ e as)           = Call (k y) e as
>     rextend k y@(Equivalence _ es)      = Equivalence (k y) es
>     rextend k y@(Open _ s)              = Open (k y) s
>     rextend k y@(Close _ s)             = Close (k y) s
>     rextend k y@(Continue _)            = Continue (k y)
>     rextend k y@(Cycle _ s)             = Cycle (k y) s
>     rextend k y@(Deallocate _ es e)     = Deallocate (k y) es e
>     rextend k y@(Endfile _ s)           = Endfile (k y) s
>     rextend k y@(Exit _ s)              = Exit (k y) s
>     rextend k y@(Forall _ es f)         = Forall (k y) es (rextend k f)
>     rextend k y@(Goto _ s)              = Goto (k y) s
>     rextend k y@(Nullify _ e)           = Nullify (k y) e
>     rextend k y@(Inquire _ s e)         = Inquire (k y) s e
>     rextend k y@(Rewind _ s)            = Rewind (k y) s
>     rextend k y@(Stop _ e)              = Stop (k y) e
>     rextend k y@(Where _ e f)           = Where (k y) e (rextend k f)
>     rextend k y@(Write _ s e)           = Write (k y) s e
>     rextend k y@(PointerAssg _ e1 e2)   = PointerAssg (k y) e1 e2
>     rextend k y@(Return _ e)            = Return (k y) e
>     rextend k y@(Label _ s f)           = Label (k y) s (rextend k f)
>     rextend k y@(Print _ e es)          = Print (k y) e es
>     rextend k y@(ReadS _ s e)           = ReadS (k y) s e
>     rextend k y@(TextStmt _ s)          = TextStmt (k y) s
>     rextend k y@(NullStmt _)            = NullStmt (k y)


> instance Comonad Fortran where
>     extract x = rextract x

>     extend k y@(Assg _ e1 e2)        = Assg (k y) (fmap (k . NullStmt) e1) 
>                                                     (fmap (k . NullStmt) e2)

>     extend k y@(For _ v e1 e2 e3 fs) = For (k y) (fmap (k . NullStmt) v)
>                                                   (fmap (k . NullStmt) e1)
>                                                    (fmap (k . NullStmt) e2)
>                                                     (fmap (k . NullStmt) e3)
>                                                      (extend k fs)

>     extend k y@(FSeq _ f1 f2)        = FSeq (k y) (extend k f1) (extend k f2)
>     extend k y@(If _ e f1 fes f3)    = let fes' = map (\(e, f) -> (fmap (k . NullStmt) e, extend k f)) fes
>                                            f3' = case f3 of 
>                                                     Nothing -> Nothing
>                                                     Just f3a -> Just (extend k f3a)
>                                         in If (k y) (fmap (k . NullStmt) e) (extend k f1) fes' f3'

>     extend k y@(Allocate _ e1 e2)      = Allocate (k y) (fmap (k . NullStmt) e1)
>                                                           (fmap (k . NullStmt) e2)
>     extend k y@(Backspace _ sp)        = Backspace (k y) (map (fmap (k . NullStmt)) sp)
>     extend k y@(Call _ e as)           = Call (k y) (fmap (k . NullStmt) e)
>                                                       (fmap (k . NullStmt) as)
>     extend k y@(Open _ s)              = Open (k y) (map (fmap (k . NullStmt)) s)
>     extend k y@(Close _ s)             = Close (k y) (map (fmap (k . NullStmt)) s)
>     extend k y@(Continue _)            = Continue (k y)
>     extend k y@(Cycle _ s)             = Cycle (k y) s
>     extend k y@(Deallocate _ es e)     = Deallocate (k y) (map (fmap (k . NullStmt)) es) (fmap (k . NullStmt) e)
>     extend k y@(Endfile _ s)           = Endfile (k y) (map (fmap (k . NullStmt)) s)
>     extend k y@(Exit _ s)              = Exit (k y) s
>     extend k y@(Forall _ (es, e) f)    = let g (s, e1, e2, e3) = (s, fmap (k . NullStmt) e1, 
>                                                                        fmap (k . NullStmt) e2,
>                                                                          fmap (k . NullStmt) e3)
>                                          in Forall (k y) (map g es, fmap (k . NullStmt) e) (extend k f)
>     extend k y@(Goto _ s)              = Goto (k y) s
>     extend k y@(Nullify _ es)          = Nullify (k y) (map (fmap (k . NullStmt)) es)
>     extend k y@(Inquire _ ss es)       = Inquire (k y) (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
>     extend k y@(Rewind _ ss)           = Rewind (k y) (map (fmap (k . NullStmt)) ss)
>     extend k y@(Stop _ e)              = Stop (k y) (fmap (k . NullStmt) e)
>     extend k y@(Where _ e f)           = Where (k y) (fmap (k . NullStmt) e) (extend k f)
>     extend k y@(Write _ ss es)         = Write (k y) (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
>     extend k y@(PointerAssg _ e1 e2)   = PointerAssg (k y) (fmap (k . NullStmt) e1) (fmap (k . NullStmt) e2)
>     extend k y@(Return _ e)            = Return (k y) (fmap (k . NullStmt) e)
>     extend k y@(Label _ s f)           = Label (k y) s (extend k f)
>     extend k y@(Print _ e es)          = Print (k y) (fmap (k . NullStmt) e) (map (fmap (k . NullStmt)) es)
>     extend k y@(ReadS _ ss es)         = ReadS (k y) (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
>     extend k y@(TextStmt _ s)          = TextStmt (k y) s
>     extend k y@(NullStmt _)            = NullStmt (k y)



Accessors

> class Successors t where
>     successors :: t -> [t]

> instance Successors (Fortran t) where
>     successors (FSeq _ f1 f2)           = f2 : successors f1
>     successors (For _ _ _ _ _ f)        = [f]
>     successors (If _ _ f efs Nothing)   = f : map snd efs
>     successors (If _ _ f efs (Just f')) = [f, f'] ++ map snd efs
>     successors (Forall _ _ f)           = [f]
>     successors (Where _ _ f)            = [f]
>     successors (Label _ _ f)            = [f]
>     successors _                        = []


> reassociate :: Fortran () -> Fortran ()
> reassociate (FSeq () (FSeq () a b) c) = FSeq () (reassociate a) (FSeq ()(reassociate b) (reassociate c))
> reassociate t = t