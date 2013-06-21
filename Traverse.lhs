> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE DeriveGeneric #-}

 {-# LANGUAGE MultiParamTypeClasses #-}

 {-# LANGUAGE OverlappingInstances #-}

 {-# LANGUAGE ImplicitParams #-}
 {-# LANGUAGE KindSignatures #-}

> module Traverse where

> import Language.Fortran

> import Generics.Deriving.Base
> import Generics.Deriving.Copoint
> import GHC.Generics

> import Data.Generics.Str
> import Data.Generics.Uniplate.Operations

> import Language.Fortran.Lexer

> import Control.Comonad

Other helpers

> fanout :: (a -> b) -> (a -> c) -> a -> (b, c)
> fanout f g x = (f x, g x)

> prod :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> prod f g (x, y) = (f x, g y)

> mfmap :: Functor f => (a -> b) -> [f a] -> [f b]
> mfmap f = map (fmap f)

> data Test0 a = Test0 a deriving Generic1 
> data Test a = Test (Test0 a) deriving Generic1




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

>     rextend k y@(Assg _ sp e1 e2)        = Assg (k y) sp e1 e2
>     rextend k y@(For _ sp v e1 e2 e3 fs) = For (k y) sp v e1 e2 e3 (rextend k fs)
>     rextend k y@(FSeq _ sp f1 f2)        = FSeq (k y) sp (rextend k f1) (rextend k f2)
>     rextend k y@(If _ sp e f1 fes f3)    = let fes' = map (\(e, f) -> (e, rextend k f)) fes
>                                                f3' = case f3 of 
>                                                     Nothing -> Nothing
>                                                     Just f3a -> Just (rextend k f3a)
>                                             in If (k y) sp e (rextend k f1) fes' f3'
>     rextend k y@(Allocate _ sp e1 e2)      = Allocate (k y) sp e1 e2
>     rextend k y@(Backspace _ sp sp')        = Backspace (k y) sp sp'
>     rextend k y@(Call _ sp e as)           = Call (k y) sp e as
>     rextend k y@(Equivalence _ sp es)      = Equivalence (k y) sp es
>     rextend k y@(Open _ sp s)              = Open (k y) sp s
>     rextend k y@(Close _ sp s)             = Close (k y) sp s
>     rextend k y@(Continue _ sp)            = Continue (k y) sp
>     rextend k y@(Cycle _ sp s)             = Cycle (k y) sp s
>     rextend k y@(Deallocate _ sp es e)     = Deallocate (k y) sp es e
>     rextend k y@(Endfile _ sp s)           = Endfile (k y) sp s
>     rextend k y@(Exit _ sp s)              = Exit (k y) sp s
>     rextend k y@(Forall _ sp es f)         = Forall (k y) sp es (rextend k f)
>     rextend k y@(Goto _ sp s)              = Goto (k y) sp s
>     rextend k y@(Nullify _ sp e)           = Nullify (k y) sp e
>     rextend k y@(Inquire _ sp s e)         = Inquire (k y) sp s e
>     rextend k y@(Rewind _ sp s)            = Rewind (k y) sp s
>     rextend k y@(Stop _ sp e)              = Stop (k y) sp e
>     rextend k y@(Where _ sp e f)           = Where (k y) sp e (rextend k f)
>     rextend k y@(Write _ sp s e)           = Write (k y) sp s e
>     rextend k y@(PointerAssg _ sp e1 e2)   = PointerAssg (k y) sp e1 e2
>     rextend k y@(Return _ sp e)            = Return (k y) sp e
>     rextend k y@(Label _ sp s f)           = Label (k y) sp s (rextend k f)
>     rextend k y@(Print _ sp e es)          = Print (k y) sp e es
>     rextend k y@(ReadS _ sp s e)           = ReadS (k y) sp s e
>     rextend k y@(TextStmt _ sp s)          = TextStmt (k y) sp s
>     rextend k y@(NullStmt _ sp)            = NullStmt (k y) sp


-- > instance Comonad Fortran where
-- >     extract x = rextract x

-- >     extend k y@(Assg _ sp e1 e2)        = Assg (k y) sp  (fmap (k . NullStmt) e1) 
-- >                                                     (fmap (k . NullStmt) e2)

-- >     extend k y@(For _ sp v e1 e2 e3 fs) = For (k y) sp  (fmap (k . NullStmt) v)
-- >                                                   (fmap (k . NullStmt) e1)
-- >                                                    (fmap (k . NullStmt) e2)
-- >                                                     (fmap (k . NullStmt) e3)
-- >                                                      (extend k fs)

-- >     extend k y@(FSeq _ sp f1 f2)        = FSeq (k y) sp  (extend k f1) (extend k f2)
-- >     extend k y@(If _ sp e f1 fes f3)    = let fes' = map (\(e, f) -> (fmap (k . NullStmt) e, extend k f)) fes
-- >                                               f3' = case f3 of 
-- >                                                       Nothing -> Nothing
-- >                                                       Just f3a -> Just (extend k f3a)
-- >                                           in If (k y) sp  (fmap (k . NullStmt) e) (extend k f1) fes' f3'

-- >     extend k y@(Allocate _ sp e1 e2)      = Allocate (k y) sp  (fmap (k . NullStmt) e1)
-- >                                                           (fmap (k . NullStmt) e2)
-- >     extend k y@(Backspace _ sp sp')        = Backspace (k y) sp  (map (fmap (k . NullStmt)) sp')
-- >     extend k y@(Call _ sp e as)           = Call (k y) sp  (fmap (k . NullStmt) e)
-- >                                                       (fmap (k . NullStmt) as)
-- >     extend k y@(Open _ sp s)              = Open (k y) sp  (map (fmap (k . NullStmt)) s)
-- >     extend k y@(Close _ sp s)             = Close (k y) sp  (map (fmap (k . NullStmt)) s)
-- >     extend k y@(Continue _ sp)            = Continue (k y) sp 
-- >     extend k y@(Cycle _ sp s)             = Cycle (k y) sp  s
-- >     extend k y@(Deallocate _ sp es e)     = Deallocate (k y) sp  (map (fmap (k . NullStmt)) es) (fmap (k . NullStmt) e)
-- >     extend k y@(Endfile _ sp s)           = Endfile (k y) sp  (map (fmap (k . NullStmt)) s)
-- >     extend k y@(Exit _ sp s)              = Exit (k y) sp  s
-- >     extend k y@(Forall _ sp (es, e) f)    = let g (s, e1, e2, e3) = (s, fmap (k . NullStmt) e1, 
-- >                                                                        fmap (k . NullStmt) e2,
-- >                                                                          fmap (k . NullStmt) e3)
-- >                                             in Forall (k y) sp  (map g es, fmap (k . NullStmt) e) (extend k f)
-- >     extend k y@(Goto _ sp s)              = Goto (k y) sp  s
-- >     extend k y@(Nullify _ sp es)          = Nullify (k y) sp  (map (fmap (k . NullStmt)) es)
-- >     extend k y@(Inquire _ sp ss es)       = Inquire (k y) sp  (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
-- >     extend k y@(Rewind _ sp ss)           = Rewind (k y) sp  (map (fmap (k . NullStmt)) ss)
-- >     extend k y@(Stop _ sp e)              = Stop (k y) sp  (fmap (k . NullStmt) e)
-- >     extend k y@(Where _ sp e f)           = Where (k y) sp  (fmap (k . NullStmt) e) (extend k f)
-- >     extend k y@(Write _ sp ss es)         = Write (k y) sp  (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
-- >     extend k y@(PointerAssg _ sp e1 e2)   = PointerAssg (k y) sp  (fmap (k . NullStmt) e1) (fmap (k . NullStmt) e2)
-- >     extend k y@(Return _ sp e)            = Return (k y) sp  (fmap (k . NullStmt) e)
-- >     extend k y@(Label _ sp s f)           = Label (k y) sp  s (extend k f)
-- >     extend k y@(Print _ sp e es)          = Print (k y) sp  (fmap (k . NullStmt) e) (map (fmap (k . NullStmt)) es)
-- >     extend k y@(ReadS _ sp ss es)         = ReadS (k y) sp  (map (fmap (k . NullStmt)) ss) (map (fmap (k . NullStmt)) es)
-- >     extend k y@(TextStmt _ sp s)          = TextStmt (k y) sp  s
-- >     extend k y@(NullStmt _ sp)            = NullStmt (k y) sp 



Accessors

> class Successors t where
>     successors :: t -> [t]

> instance Successors (Fortran t) where
>     successors (FSeq _ _ f1 f2)         = f2 : successors f1
>     successors (For _ _ _ _ _ _ f)        = [f]
>     successors (If _ _ _ f efs Nothing)   = f : map snd efs
>     successors (If _ _ _ f efs (Just f')) = [f, f'] ++ map snd efs
>     successors (Forall _ _ _ f)           = [f]
>     successors (Where _ _ _ f)            = [f]
>     successors (Label _ _ _ f)            = [f]
>     successors _                        = []


TODO: Needs fixing with the spans - need to pull apart and put back together

> reassociate :: Fortran () -> Fortran ()
> reassociate (FSeq () sp1 (FSeq () sp2 a b) c) = FSeq () sp1 (reassociate a) (FSeq () sp2  (reassociate b) (reassociate c))
> reassociate t = t

> class Copointed d where
>    copoint :: d a -> a 

> instance Copointed Attr where copoint = gcopoint
> instance Copointed BaseType where copoint = gcopoint
> instance Copointed SubName where copoint = gcopoint
> instance Copointed VarName where copoint = gcopoint
> instance Copointed ArgName where copoint = gcopoint
> instance Copointed Arg     where copoint = gcopoint
> instance Copointed Implicit where copoint = gcopoint

> instance Copointed ArgList where 
>     copoint (ArgList x _) = x

> instance Copointed Program where
>     copoint (Main x sp _ _ _ _)      = x
>     copoint (Sub x sp _ _ _ _)       = x
>     copoint (Function x sp _ _ _ _)  = x
>     copoint (Module x sp _ _ _ _ _ ) = x
>     copoint (BlockData x sp _ _ _ _) = x
>     copoint (PSeq x sp _ _)          = x
>     copoint (Prog x sp _)            = x
>     copoint (NullProg x sp)          = x
> 
> instance Copointed Decl where
>     copoint (Decl x _ _)          = x
>     copoint (Namelist x _)        = x
>     copoint (Data x _)            = x
>     copoint (AccessStmt x _ _)    = x
>     copoint (ExternalStmt x _)    = x
>     copoint (Interface x _ _)     = x
>     copoint (Common x _ _)        = x
>     copoint (DerivedTypeDef x _ _ _ _) = x
>     copoint (Include x _)         = x
>     copoint (DSeq x _ _)          = x
>     copoint (TextDecl x _)        = x
>     copoint (NullDecl x)        = x

> instance Copointed Fortran where
>     copoint (Assg x s e1 e2)        = x
>     copoint (For x s v e1 e2 e3 fs) = x
>     copoint (FSeq x sp f1 f2)       = x
>     copoint (If x sp e f1 fes f3)   = x
>     copoint (Allocate x sp e1 e2)   = x
>     copoint (Backspace x sp _)      = x
>     copoint (Call x sp e as)        = x
>     copoint (Open x sp s)           = x
>     copoint (Close x sp s)          = x 
>     copoint (Continue x sp)         = x
>     copoint (Cycle x sp s)          = x
>     copoint (Deallocate x sp es e)  = x
>     copoint (Equivalence x sp _)    = x
>     copoint (Endfile x sp s)        = x
>     copoint (Exit x sp s)           = x
>     copoint (Forall x sp es f)      = x
>     copoint (Goto x sp s)           = x
>     copoint (Nullify x sp e)        = x
>     copoint (Inquire x sp s e)      = x
>     copoint (Rewind x sp s)         = x 
>     copoint (Stop x sp e)           = x
>     copoint (Where x sp e f)        = x 
>     copoint (Write x sp s e)        = x
>     copoint (PointerAssg x sp e1 e2) = x
>     copoint (Return x sp e)         = x
>     copoint (Label x sp s f)        = x
>     copoint (Print x sp e es)       = x
>     copoint (ReadS x sp s e)        = x
>     copoint (TextStmt x sp s)       = x
>     copoint (NullStmt x sp)         = x

> instance Copointed Expr where
>    copoint (Con x sp _) = x
>    copoint (ConS x sp _) = x
>    copoint (Var x sp _ ) = x
>    copoint (Bin x sp _ _ _) = x
>    copoint (Unary x sp _ _) = x
>    copoint (CallExpr x sp _ _) = x
>    copoint (NullExpr x _) = x
>    copoint (Null x _) = x
>    copoint (ESeq x sp _ _) = x
>    copoint (Bound x sp _ _) = x
>    copoint (Sqrt x sp _) = x
>    copoint (ArrayCon x sp _) = x
>    copoint (AssgExpr x sp _ _) = x

> instance Copointed GSpec where
>    copoint (GName x _) = x
>    copoint (GOper x _) = x
>    copoint (GAssg x)   = x