> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}

 {-# LANGUAGE MultiParamTypeClasses #-}

 {-# LANGUAGE ScopedTypeVariables #-}
 {-# LANGUAGE OverlappingInstances #-}

 {-# LANGUAGE ImplicitParams #-}
 {-# LANGUAGE KindSignatures #-}

> module Traverse where

> import Language.Fortran.Parser
> import Language.Fortran

> import Generics.Deriving.Base
> import GHC.Generics

> import Data.Generics.Str
> import Data.Generics.Uniplate.Operations

> import Control.Comonad

Data-type generic comonad-style traversal

> extendBi :: (Biplate (from a) (to a), RComonad to) => (to a -> a) -> (from a) -> (from a)
> extendBi f x = case biplate x of
>                      (current, generate) -> generate $ strMap (rextend f) current

> class RComonad t where
>     rextract :: t a -> a
>     rextend :: (t a -> a) -> t a -> t a

> instance RComonad Fortran where
>     rextract (Assg x e1 e2)        = x
>     rextract (For x v e1 e2 e3 fs) = x
>     rextract (FSeq x f1 f2)        = x
>     rextract (If x e f1 fes f3)    = x
>     rextract (Allocate x e1 e2)    = x
>     rextract (Backspace x sp)      = x
>     rextract (Call x e as)         = x
>     rextract (Open x s)            = x
>     rextract (Close x s)           = x 
>     rextract (Continue x)          = x
>     rextract (Cycle x s)           = x
>     rextract (Deallocate x es e)   = x
>     rextract (Endfile x s)         = x
>     rextract (Exit x s)            = x
>     rextract (Forall x es f)       = x
>     rextract (Goto x s)            = x
>     rextract (Nullify x e)         = x
>     rextract (Inquire x s e)       = x
>     rextract (Rewind x s)          = x 
>     rextract (Stop x e)            = x
>     rextract (Where x e f)         = x 
>     rextract (Write x s e)         = x
>     rextract (PointerAssg x e1 e2) = x
>     rextract (Return x e)          = x
>     rextract (Label x s f)         = x
>     rextract (Print x e es)        = x
>     rextract (ReadS x s e)         = x
>     rextract (TextStmt x s)        = x
>     rextract (NullStmt x)          = x

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