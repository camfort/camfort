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


module Language.Fortran where

---------------------------------------------------------------------------
-- IMPORTS
---------------------------------------------------------------------------

import Data.Generics -- Typeable class and boilerplate generic functions

import Data.Maybe
import Data.List


import Language.Haskell.Syntax (SrcLoc)

---------------------------------------------------------------------------
-- Language definition for parametric Fortran
---------------------------------------------------------------------------


-- Definition of data types
-- 

-- All kinds of names ...
-- 

type SrcSpan = (SrcLoc, SrcLoc)

type Variable = String

type ProgName = String               -- Fortran program names

type MeasureUnit = String

data SubName p  = SubName p String   -- Fortran subroutine names
                 | NullSubName p
                 deriving (Show, Functor, Typeable, Data, Eq)
 
data VarName  p = VarName p Variable 
                  deriving (Show, Functor, Typeable, Data, Eq, Read)

data ArgName  p = ArgName p String
                | ASeq p (ArgName p) (ArgName p)
                | NullArg p
                 deriving (Show, Functor, Typeable, Data, Eq)

-- Syntax defintions
--

data Arg      p = Arg p (ArgName p) SrcSpan -- the src span denotes the end of the arg list before ')'
                  deriving (Show, Functor, Typeable, Data, Eq)

data ArgList  p = ArgList p (Expr p)
                  deriving (Show, Functor, Typeable, Data, Eq)

type Program p = [ProgUnit p]

             -- Prog type   (type of result)   name      args  (result)  body    use's
data ProgUnit  p = Main      p SrcSpan                      (SubName p)  (Arg p)  (Block p) [ProgUnit p]
                | Sub        p SrcSpan (Maybe (BaseType p)) (SubName p)  (Arg p)  (Block p)
                | Function   p SrcSpan (Maybe (BaseType p)) (SubName p)  (Arg p)  (Maybe (VarName p)) (Block p)
                | Module     p SrcSpan                      (SubName p)  (Uses p) (Implicit p) (Decl p) [ProgUnit p]
                | BlockData  p SrcSpan                      (SubName p)  (Uses p) (Implicit p) (Decl p)
                | PSeq       p SrcSpan (ProgUnit p) (ProgUnit p)   -- sequence of programs
                | Prog       p SrcSpan (ProgUnit p)               -- useful for {#p: #q : program ... }
                | NullProg   p SrcSpan                           -- null
                deriving (Show, Functor, Typeable, Data, Eq)

             -- implicit none or no implicit 
data Implicit p = ImplicitNone p | ImplicitNull p
                deriving (Show, Functor, Typeable, Data, Eq)

type Renames = [(Variable, Variable)] -- renames for "use"s 

data Uses p     = Use p (String, Renames) (Uses p) p  -- (second 'p' let's you annotate the 'cons' part of the cell)
                | UseNil p deriving (Show, Functor, Typeable, Data, Eq)

             --       use's     implicit  decls  stmts
data Block    p = Block p  (Uses p) (Implicit p) SrcSpan (Decl p) (Fortran p)
                deriving (Show, Functor, Typeable, Data, Eq)

data Decl     p = Decl           p SrcSpan [(Expr p, Expr p)] (Type p)              -- declaration stmt
                | Namelist       p [(Expr p, [Expr p])]                     -- namelist declaration
                | Data           p [(Expr p, Expr p)]                       -- data declaration
                | Equivalence    p SrcSpan [(Expr p)]
                | AccessStmt     p (Attr p) [GSpec p]                       -- access stmt
                | ExternalStmt   p [String]                                 -- external stmt
                | Interface      p (Maybe (GSpec p)) [InterfaceSpec p]      -- interface declaration
                | Common         p SrcSpan (Maybe String) [Expr p]
                | DerivedTypeDef p SrcSpan (SubName p) [Attr p] [Attr p] [Decl p]  -- derivified
                | MeasureUnitDef p SrcSpan [(MeasureUnit, MeasureUnitSpec p)]
                | Include        p (Expr p)                                -- include stmt
                | DSeq           p (Decl p) (Decl p)                       -- list of decls
                | TextDecl       p String                                  -- cpp switches to carry over
                | NullDecl       p SrcSpan                                       -- null
                  deriving (Show, Functor, Typeable, Data, Eq)

             -- BaseType  dimensions     type        Attributes   kind   len 
data Type     p = BaseType p                    (BaseType p) [Attr p] (Expr p) (Expr p)
                | ArrayT   p [(Expr p, Expr p)] (BaseType p) [Attr p] (Expr p) (Expr p)
                  deriving (Show, Functor, Typeable, Data, Eq)

data BaseType p = Integer p | Real p | Character p | SomeType p | DerivedType p (SubName p)
                | Recursive p | Pure p | Elemental p | Logical p | Complex p
                  deriving (Show, Functor, Typeable, Data, Eq)

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
                | MeasureUnit p (MeasureUnitSpec p)
--              | Dimension [(Expr,Expr)] -- in Type: ArrayT
              deriving (Show, Functor, Typeable, Data, Eq)
			  
data GSpec   p = GName p (Expr p) | GOper p (BinOp p) | GAssg p
                 deriving (Show, Functor, Typeable, Data, Eq)
			  
data InterfaceSpec p = FunctionInterface   p (SubName p) (Arg p) (Uses p) (Implicit p) (Decl p)
                     | SubroutineInterface p (SubName p) (Arg p) (Uses p) (Implicit p) (Decl p)
                     | ModuleProcedure     p [(SubName p)]
                       deriving (Show, Functor, Typeable, Data, Eq)
				   
data IntentAttr p = In p
                  | Out p
                  | InOut p
                    deriving (Show, Functor, Typeable, Data, Eq)
				
data MeasureUnitSpec p = UnitProduct p [(MeasureUnit, Fraction p)]
                       | UnitQuotient p [(MeasureUnit, Fraction p)] [(MeasureUnit, Fraction p)]
                       | UnitNone p
                         deriving (Show, Functor, Typeable, Data, Eq)

data Fraction p = IntegerConst p String
                | FractionConst p String String
                | NullFraction p
                  deriving (Show, Functor, Typeable, Data, Eq)

data Fortran  p = Assg p SrcSpan (Expr p) (Expr p) 
                | For  p SrcSpan (VarName p) (Expr p) (Expr p) (Expr p) (Fortran p)
                | FSeq p SrcSpan (Fortran p) (Fortran p)
                | If   p SrcSpan (Expr p) (Fortran p) [((Expr p),(Fortran p))] (Maybe (Fortran p))
                | Allocate p SrcSpan (Expr p) (Expr p)
                | Backspace p SrcSpan [Spec p]
                | Call p SrcSpan (Expr p) (ArgList p)
                | Open p SrcSpan [Spec p]
                | Close p SrcSpan [Spec p]
                | Continue p SrcSpan 
                | Cycle p SrcSpan String
                | Deallocate p SrcSpan [(Expr p)] (Expr p)
                | Endfile p SrcSpan [Spec p]
                | Exit p SrcSpan String
                | Forall p SrcSpan ([(String,(Expr p),(Expr p),(Expr p))],(Expr p)) (Fortran p)
                | Goto p SrcSpan String
                | Nullify p SrcSpan [(Expr p)]
                | Inquire p SrcSpan [Spec p] [(Expr p)]
                | Rewind p SrcSpan [Spec p]
                | Stop p SrcSpan (Expr p)
                | Where p SrcSpan (Expr p) (Fortran p)
                | Write p SrcSpan [Spec p] [(Expr p)]
                | PointerAssg p SrcSpan  (Expr p) (Expr p)
                | Return p SrcSpan  (Expr p)
                | Label p SrcSpan String (Fortran p)
                | Print p SrcSpan (Expr p) [(Expr p)]
                | ReadS p SrcSpan [Spec p] [(Expr p)]
                | TextStmt p SrcSpan String     -- cpp switches to carry over
                | NullStmt p SrcSpan
                  deriving (Show, Functor, Typeable, Data, Eq)

-- type Bound    = ((Expr p),(Expr p))

data Expr  p = Con p SrcSpan String
             | ConL p SrcSpan Char String
             | ConS p SrcSpan String  -- String representing a constant
             | Var p SrcSpan  [((VarName p),[(Expr p)])]
             | Bin p SrcSpan  (BinOp p) (Expr p) (Expr p)
             | Unary p SrcSpan (UnaryOp p) (Expr p)
             | CallExpr p SrcSpan (Expr p) (ArgList p)
             | NullExpr p SrcSpan
             | Null p SrcSpan 
             | ESeq p SrcSpan (Expr p) (Expr p)
             | Bound p SrcSpan (Expr p) (Expr p)
             | Sqrt p SrcSpan (Expr p)
             | ArrayCon p SrcSpan [(Expr p)]
             | AssgExpr p SrcSpan String (Expr p)
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
                deriving (Show, Functor, Typeable, Data, Eq)

data UnaryOp  p = UMinus p | Not p
                deriving (Show, Functor,Typeable,Data, Eq)

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

class GetSpan t where
    getSpan :: t -> (SrcLoc, SrcLoc)

instance GetSpan (Block a) where
    getSpan (Block _ _ _ sp _ _) = sp

instance GetSpan (Decl a) where
    getSpan (Decl _ sp _ _)               = sp
    getSpan (NullDecl _ sp)               = sp
    getSpan (Common _ sp _ _)             = sp
    getSpan (Equivalence x sp _)          = sp
    getSpan (DerivedTypeDef x sp _ _ _ _) = sp
    getSpan (MeasureUnitDef x sp _)       = sp
    getSpan _ = error "No span for non common/equiv/type/ null declarations"

instance GetSpan (ProgUnit a) where
    getSpan (Main x sp _ _ _ _)       = sp
    getSpan (Sub x sp _ _ _ _)        = sp
    getSpan (Function x sp _ _ _ _ _) = sp
    getSpan (Module x sp _ _ _ _ _ )  = sp
    getSpan (BlockData x sp _ _ _ _)  = sp
    getSpan (PSeq x sp _ _)           = sp
    getSpan (Prog x sp _)             = sp
    getSpan (NullProg x sp)           = sp

instance GetSpan (Expr a) where
    getSpan (Con x sp _)        = sp
    getSpan (ConS x sp _)       = sp
    getSpan (Var x sp _ )       = sp
    getSpan (Bin x sp _ _ _)    = sp
    getSpan (Unary x sp _ _)    = sp
    getSpan (CallExpr x sp _ _) = sp
    getSpan (NullExpr x sp)     = sp
    getSpan (Null x sp)         = sp
    getSpan (ESeq x sp _ _)     = sp
    getSpan (Bound x sp _ _)    = sp
    getSpan (Sqrt x sp _)       = sp
    getSpan (ArrayCon x sp _)   = sp
    getSpan (AssgExpr x sp _ _) = sp

instance GetSpan (Fortran a) where
    getSpan (Assg x sp e1 e2)        = sp
    getSpan (For x sp v e1 e2 e3 fs) = sp
    getSpan (FSeq x sp f1 f2)        = sp
    getSpan (If x sp e f1 fes f3)    = sp
    getSpan (Allocate x sp e1 e2)    = sp
    getSpan (Backspace x sp _)       = sp
    getSpan (Call x sp e as)         = sp
    getSpan (Open x sp s)            = sp
    getSpan (Close x sp s)           = sp 
    getSpan (Continue x sp)          = sp
    getSpan (Cycle x sp s)           = sp
    getSpan (Deallocate x sp es e)   = sp
    getSpan (Endfile x sp s)         = sp
    getSpan (Exit x sp s)            = sp
    getSpan (Forall x sp es f)       = sp
    getSpan (Goto x sp s)            = sp
    getSpan (Nullify x sp e)         = sp
    getSpan (Inquire x sp s e)       = sp
    getSpan (Rewind x sp s)          = sp 
    getSpan (Stop x sp e)            = sp
    getSpan (Where x sp e f)         = sp 
    getSpan (Write x sp s e)         = sp
    getSpan (PointerAssg x sp e1 e2) = sp
    getSpan (Return x sp e)          = sp
    getSpan (Label x sp s f)         = sp
    getSpan (Print x sp e es)        = sp
    getSpan (ReadS x sp s e)         = sp
    getSpan (TextStmt x sp s)        = sp
    getSpan (NullStmt x sp)          = sp