-- 
-- Fortran.hs  - 
-- Based on FortranP.hs from Parameterized Fortran by Martin Erwig.
--
-- A Fortran program generator implemented using the boilerplate approach and
-- existential types

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE DeriveGeneric #-}

module Language.Fortran where

---------------------------------------------------------------------------
-- IMPORTS
---------------------------------------------------------------------------

import Data.Generics -- Typeable class and boilerplate generic functions

import Data.Maybe
import Data.List

import Generics.Deriving.Base
import Generics.Deriving.Copoint
import GHC.Generics

---------------------------------------------------------------------------
-- Language definition for parametric Fortran
---------------------------------------------------------------------------


-- Definition of data types
-- 

-- All kinds of names ...
-- 


type Variable = String

type ProgName = String               -- Fortran program names

data SubName p  = SubName p String   -- Fortran subroutine names
                 | NullSubName p
                 deriving (Show, Functor, Typeable, Data, Eq, Generic1)
 
data VarName  p = VarName p String 
                  deriving (Show, Functor, Typeable, Data, Eq, Read, Generic1)

data ArgName  p = ArgName p String
                | ASeq p (ArgName p) (ArgName p)
                | NullArg p
                 deriving (Show, Functor, Typeable, Data, Eq, Generic1)

-- Syntax defintions
--

data Arg      p = Arg p (ArgName p)
                  deriving (Show, Functor, Typeable, Data, Eq, Generic1)

data ArgList  p = ArgList p (Expr p)
                  deriving (Show, Functor, Typeable, Data, Eq)

             -- Prog type   (type of result)   name      args  body    use's  
data Program  p = Main       p                      (SubName p)  (Arg p)  (Block p) [Program p]
                | Sub        p (Maybe (BaseType p)) (SubName p)  (Arg p)  (Block p)
                | Function   p (Maybe (BaseType p)) (SubName p)  (Arg p)  (Block p)
                | Module     p                      (SubName p)  [String] (Implicit p) (Decl p) [Program p]
                | BlockData  p                      (SubName p)  [String] (Implicit p) (Decl p)
                | PSeq       p (Program p) (Program p)   -- sequence of programs
                | Prog       p (Program p)               -- useful for {#p: #q : program ... }
                | NullProg   p                           -- null
                deriving (Show, Functor, Typeable, Data, Eq)

             -- implicit none or no implicit 
data Implicit p = ImplicitNone p | ImplicitNull p
                deriving (Show, Functor, Typeable, Data, Eq, Generic1)
				
             --       use's     implicit  decls  stmts
data Block    p = Block p [String]  (Implicit p)  (Decl p) (Fortran p)
                deriving (Show, Functor, Typeable, Data, Eq)

data Decl     p = Decl           p [(Expr p, Expr p)] (Type p)              -- declaration stmt
                | Namelist       p [(Expr p, [Expr p])]                     -- namelist declaration
                | Data           p [(Expr p, Expr p)]                       -- data declaration
                | AccessStmt     p (Attr p) [GSpec p]                       -- access stmt
                | ExternalStmt   p [String]                                 -- external stmt
                | Interface      p (Maybe (GSpec p)) [InterfaceSpec p]      -- interface declaration
                | Common         p (Maybe String) [Expr p]
                | DerivedTypeDef p (SubName p) [Attr p] [Attr p] [Decl p]  -- derivified
                | Include        p (Expr p)                                -- include stmt
                | DSeq           p (Decl p) (Decl p)                       -- list of decls
                | TextDecl       p String                                  -- cpp switches to carry over
                | NullDecl       p                                         -- null
                  deriving (Show, Functor, Typeable, Data, Eq)

             -- BaseType  dimensions     type        Attributes   kind   len 
data Type     p = BaseType p                    (BaseType p) [Attr p] (Expr p) (Expr p)
                | ArrayT   p [(Expr p, Expr p)] (BaseType p) [Attr p] (Expr p) (Expr p)
                  deriving (Show, Functor, Typeable, Data, Eq)

data BaseType p = Integer p | Real p | Character p | SomeType p | DerivedType p (SubName p)
                | Recursive p | Pure p | Elemental p | Logical p | Complex p
                  deriving (Show, Functor, Typeable, Data, Eq, Generic1)

data Attr     p = Parameter p
                | Allocatable p
                | External p
                | Intent p (IntentAttr p) 
                | Intrinsic p
                | Optional p
                | Pointer p
                | Save p
                | Target p
                | Volatile p
                | Public p
                | Private p
                | Sequence p
--              | Dimension [(Expr,Expr)] -- in Type: ArrayT
              deriving (Show, Functor, Typeable, Data, Eq, Generic1)
			  
data GSpec   p = GName p (Expr p) | GOper p (BinOp p) | GAssg p
                 deriving (Show, Functor, Typeable, Data, Eq)
			  
data InterfaceSpec p = FunctionInterface   p (SubName p) (Arg p) [String] (Implicit p) (Decl p)
                     | SubroutineInterface p (SubName p) (Arg p) [String] (Implicit p) (Decl p)
                     | ModuleProcedure     p [(SubName p)]
                       deriving (Show, Functor, Typeable, Data, Eq)
				   
data IntentAttr p = In p
                  | Out p
                  | InOut p
                    deriving (Show, Functor, Typeable, Data, Eq, Generic1)
				
data Fortran  p = Assg p (Expr p) (Expr p) 
                | For  p (VarName p) (Expr p) (Expr p) (Expr p) (Fortran p)
                | FSeq p (Fortran p) (Fortran p)
                | If   p (Expr p) (Fortran p) [((Expr p),(Fortran p))] (Maybe (Fortran p))
                | Allocate p (Expr p) (Expr p)
                | Backspace p [Spec p]
                | Call p (Expr p) (ArgList p)
                | Equivalence p [(Expr p)]
                | Open p [Spec p]
                | Close p [Spec p]
                | Continue p
                | Cycle p String
                | Deallocate p [(Expr p)] (Expr p)
                | Endfile p [Spec p]
                | Exit p String
                | Forall p ([(String,(Expr p),(Expr p),(Expr p))],(Expr p)) (Fortran p)
                | Goto p String
                | Nullify p [(Expr p)]
                | Inquire p [Spec p] [(Expr p)]
                | Rewind p [Spec p]
                | Stop p (Expr p)
                | Where p (Expr p) (Fortran p)
                | Write p [Spec p] [(Expr p)]
                | PointerAssg p (Expr p) (Expr p)
                | Return p (Expr p)
                | Label p String (Fortran p)
                | Print p (Expr p) [(Expr p)]
                | ReadS p [Spec p] [(Expr p)]
                | TextStmt p String     -- cpp switches to carry over
                | NullStmt p
                  deriving (Show, Functor, Typeable, Data, Eq)

-- type Bound    = ((Expr p),(Expr p))

data Expr  p = Con p String
             | ConS p String  -- String constant
             | Var p [((VarName p),[(Expr p)])]
             | Bin p (BinOp p) (Expr p) (Expr p)
             | Unary p (UnaryOp p) (Expr p)
             | CallExpr p (Expr p) (ArgList p)
             | NullExpr p
             | Null p
             | ESeq p (Expr p) (Expr p)
             | Bound p (Expr p) (Expr p)
             | Sqrt p (Expr p)
             | ArrayCon p [(Expr p)]
             | AssgExpr p String (Expr p)
               deriving (Show, Functor, Typeable ,Data, Eq)

data BinOp   p = Plus p
               | Minus p
               | Mul p
               | Div p
               | Or p
               | And p
               | Concat p
               | Power p
               | RelEQ p
               | RelNE p
               | RelLT p
               | RelLE p
               | RelGT p
               | RelGE p
                deriving (Show, Functor, Typeable, Data, Eq, Generic1)

data UnaryOp  p = UMinus p | Not p
                deriving (Show, Functor,Typeable,Data, Eq, Generic1)

data Spec     p = Access   p (Expr p)
              | Action     p (Expr p)
              | Advance    p (Expr p)
              | Blank      p (Expr p)
              | Delim      p (Expr p)
              | Direct     p (Expr p)
              | End        p (Expr p)
              | Err        p (Expr p)
              | ExFile     p (Expr p)
              | Exist      p (Expr p)
              | Eor        p (Expr p)
              | File       p (Expr p)  
              | FMT        p (Expr p)
              | Form       p (Expr p)
              | Formatted  p (Expr p)
              | Unformatted  p (Expr p)
              | IOLength   p (Expr p)
              | IOStat     p (Expr p)
              | Name       p (Expr p)
              | Named      p (Expr p)
              | NoSpec     p (Expr p)
              | Number     p (Expr p)
              | NextRec    p (Expr p)
              | NML        p (Expr p)
              | Opened     p (Expr p) 
              | Pad        p (Expr p)
              | Position   p (Expr p)
              | Read       p (Expr p)
              | ReadWrite  p (Expr p)
              | Rec        p (Expr p) 
              | Recl       p (Expr p) 
              | Sequential p (Expr p)
              | Size       p (Expr p)
              | Status     p (Expr p)
              | Unit       p (Expr p)
              | WriteSp    p (Expr p)
                deriving (Show, Functor,Typeable,Data, Eq)


-- smart constructors for language 'constants', that is, expressions
-- 

ne = NullExpr 

class Copointed d where
    copoint :: d a -> a 

instance Copointed Attr where copoint = gcopoint
instance Copointed BaseType where copoint = gcopoint
instance Copointed SubName where copoint = gcopoint
instance Copointed VarName where copoint = gcopoint
instance Copointed ArgName where copoint = gcopoint
instance Copointed Arg     where copoint = gcopoint
instance Copointed Implicit where copoint = gcopoint

instance Copointed ArgList where 
    copoint (ArgList x _) = x

instance Copointed Program where
    copoint (Main x _ _ _ _)      = x
    copoint (Sub x _ _ _ _)       = x
    copoint (Function x _ _ _ _)  = x
    copoint (Module x _ _ _ _ _ ) = x
    copoint (BlockData x _ _ _ _) = x
    copoint (PSeq x _ _)          = x
    copoint (Prog x _)            = x
    copoint (NullProg x)          = x

instance Copointed Decl where
    copoint (Decl x _ _)          = x
    copoint (Namelist x _)        = x
    copoint (Data x _)            = x
    copoint (AccessStmt x _ _)    = x
    copoint (ExternalStmt x _)    = x
    copoint (Interface x _ _)     = x
    copoint (Common x _ _)        = x
    copoint (DerivedTypeDef x _ _ _ _) = x
    copoint (Include x _)         = x
    copoint (DSeq x _ _)          = x
    copoint (TextDecl x _)        = x
    copoint (NullDecl x)        = x

instance Copointed Fortran where
    copoint (Assg x e1 e2)        = x
    copoint (For x v e1 e2 e3 fs) = x
    copoint (FSeq x f1 f2)        = x
    copoint (If x e f1 fes f3)    = x
    copoint (Allocate x e1 e2)    = x
    copoint (Backspace x sp)      = x
    copoint (Call x e as)         = x
    copoint (Open x s)            = x
    copoint (Close x s)           = x 
    copoint (Continue x)          = x
    copoint (Cycle x s)           = x
    copoint (Deallocate x es e)   = x
    copoint (Equivalence x _)     = x
    copoint (Endfile x s)         = x
    copoint (Exit x s)            = x
    copoint (Forall x es f)       = x
    copoint (Goto x s)            = x
    copoint (Nullify x e)         = x
    copoint (Inquire x s e)       = x
    copoint (Rewind x s)          = x 
    copoint (Stop x e)            = x
    copoint (Where x e f)         = x 
    copoint (Write x s e)         = x
    copoint (PointerAssg x e1 e2) = x
    copoint (Return x e)          = x
    copoint (Label x s f)         = x
    copoint (Print x e es)        = x
    copoint (ReadS x s e)         = x
    copoint (TextStmt x s)        = x
    copoint (NullStmt x)          = x

instance Copointed Expr where
    copoint (Con x _) = x
    copoint (ConS x _) = x
    copoint (Var x _ ) = x
    copoint (Bin x _ _ _) = x
    copoint (Unary x _ _) = x
    copoint (CallExpr x _ _) = x
    copoint (NullExpr x) = x
    copoint (Null x) = x
    copoint (ESeq x _ _) = x
    copoint (Bound x _ _) = x
    copoint (Sqrt x _) = x
    copoint (ArrayCon x _) = x
    copoint (AssgExpr x _ _) = x

instance Copointed GSpec where
    copoint (GName x _) = x
    copoint (GOper x _) = x
    copoint (GAssg x)   = x