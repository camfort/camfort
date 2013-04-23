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

import Data.Generics.Annotate
import Generics.Deriving.Base
import GHC.Generics

---------------------------------------------------------------------------
-- Language definition for parametric Fortran
---------------------------------------------------------------------------


-- Definition of data types
-- 

-- All kinds of names ...
-- 

data Alt1 = Alt1
data Alt2 = Alt2

class Alts a 
instance Alts Alt1
instance Alts Alt2

--instance (OutputF (Program p) Alt1) => Show (Program p) where
--    show p = let ?variant = Alt1 in outputF p

class OutputF t v where
    outputF :: (?variant :: v) => t -> String

class OutputG t v where
    outputG :: (?variant :: v) => t -> String

-- Default alt1 instance
instance (OutputF t Alt1) => OutputG t Alt1 where
    outputG = outputF

instance Alts v => OutputG Char v where
    outputG = show

instance Alts v => OutputG String v where
    outputG = id

instance (Alts v, OutputG a v, OutputG b v) => OutputG (a, b) v where
     outputG (a, b) = "(" ++ outputG a ++ ", " ++ outputG b ++ ")"

instance (Alts v, OutputG a v) => OutputG [a] v where
    outputG xs = "[" ++ go xs ++ "]" where go [] = "" 
                                           go [x] = outputG x
                                           go (x:xs) = outputG x ++ ", " ++ (go xs)

instance (Alts v, OutputG a v) => OutputF [a] v where
    outputF xs = "[" ++ go xs ++ "]" where go [] = "" 
                                           go [x] = outputG x
                                           go (x:xs) = outputG x ++ ", " ++ (go xs)

class OutputIndF t v where
    outputIndF :: (?variant :: v) => Int -> t -> String

class OutputIndG t v where
    outputIndG :: (?variant :: v) => Int -> t -> String

instance (OutputIndF t Alt1) => OutputIndG t Alt1 where
    outputIndG = outputIndF

type Variable = String

[annotate|

type ProgName  = String           -- Fortran program names

data  SubName  = SubName String   -- Fortran subroutine names
               | NullSubName
                 deriving (Show, Functor, Typeable, Data, Eq)
 
data VarName   = VarName String 
                 deriving (Show, Functor, Typeable, Data, Eq, Read)

data ArgName   = ArgName String
               | ASeq ArgName ArgName
               | NullArg
                 deriving (Show, Functor, Typeable, Data, Eq)

-- Syntax defintions
--

data Arg      = Arg ArgName
                deriving (Show, Functor,Typeable,Data, Eq)

data ArgList  = ArgList Expr
                deriving (Show, Functor,Typeable,Data, Eq)

             -- Prog type   (type of result)   name      args  body    use's  
data Program  = Main                           SubName  Arg  Block [Program]
              | Sub        (Maybe BaseType)    SubName  Arg  Block
              | Function   (Maybe BaseType)    SubName  Arg  Block
              | Module                         SubName              [String] Implicit Decl [Program]
              | BlockData                      SubName              [String] Implicit Decl
             -- 
              | PSeq Program Program   -- sequence of programs
              | Prog Program            -- useful for {#p: #q : program ... }
              | NullProg                 -- null
                deriving (Show, Functor,Typeable,Data, Eq)

             -- implicit none or no implicit 
data Implicit = ImplicitNone | ImplicitNull
                deriving (Show, Functor,Typeable,Data, Eq)
				
             --       use's     implicit  decls  stmts
data Block    = Block [String]  Implicit  Decl  Fortran
                deriving (Show, Functor,Typeable,Data, Eq)

data Decl     = Decl [(Expr,Expr)] Type                     -- declaration stmt
              | Namelist [(Expr,[Expr])]                     -- namelist declaration
              | Data [(Expr,Expr)]                           -- data declaration
              | AccessStmt Attr [GSpec]                        -- access stmt
              | ExternalStmt [String]                          -- external stmt
              | Interface (Maybe GSpec) [InterfaceSpec]        -- interface declaration
              | DerivedTypeDef SubName [Attr] [Attr] [Decl]  -- derivified
              | Include Expr                                  -- include stmt
              | DSeq Decl Decl                               -- list of decls
              | TextDecl String                                -- cpp switches to carry over
              | NullDecl                                       -- null
                deriving (Show, Functor,Typeable,Data, Eq)

             -- BaseType  dimensions     type        Attributes   kind   len 
data Type     = BaseType                 BaseType   [Attr]       Expr  Expr 
              | ArrayT   [(Expr,Expr)] BaseType   [Attr]       Expr  Expr
                deriving (Show, Functor,Typeable,Data, Eq)

data BaseType = Integer | Real | Character | SomeType | DerivedType SubName
              | Recursive | Pure | Elemental | Logical | Complex
                deriving (Show, Functor,Typeable,Data, Eq)

data Attr     = Parameter
              | Allocatable
              | External
              | Intent IntentAttr
              | Intrinsic
              | Optional
              | Pointer
              | Save
              | Target
              | Volatile
              | Public
              | Private
              | Sequence
--              | Dimension [(Expr,Expr)] -- in Type: ArrayT
              deriving (Show, Functor,Typeable,Data, Eq)
			  
data GSpec    = GName Expr | GOper BinOp | GAssg
              deriving (Show, Functor,Typeable,Data, Eq)
			  
data InterfaceSpec = FunctionInterface SubName Arg [String] Implicit Decl
                   | SubroutineInterface SubName Arg [String] Implicit Decl
                   | ModuleProcedure [SubName]
                   deriving (Show, Functor,Typeable, Data, Eq)
				   
data IntentAttr = In
                | Out
                | InOut
                deriving (Show, Functor,Typeable,Data, Eq)
				
data Fortran  = Assg Expr Expr
              | For  VarName Expr Expr Expr Fortran
              | FSeq  Fortran Fortran
              | If Expr Fortran [(Expr,Fortran)] (Maybe Fortran)
              | Allocate Expr Expr
              | Backspace [Spec]
              | Call Expr ArgList
              | Open [Spec]
              | Close [Spec]
              | Continue
              | Cycle String
              | Deallocate [Expr] Expr
              | Endfile [Spec]
              | Exit String
              | Forall ([(String,Expr,Expr,Expr)],Expr) Fortran
              | Goto String
              | Nullify [Expr]
              | Inquire [Spec] [Expr]
              | Rewind [Spec]
              | Stop Expr
              | Where Expr Fortran
              | Write [Spec] [Expr]
              | PointerAssg Expr Expr
              | Return Expr
              | Label String Fortran
              | Print Expr [Expr]
              | ReadS [Spec] [Expr]
              | TextStmt String     -- cpp switches to carry over
              | NullStmt
                deriving (Show, Functor,Typeable,Data, Eq)

-- type Bound    = (Expr,Expr)

data Expr     = Con String
              | ConS String  -- String constant
              | Var [(VarName,[Expr])]
              | Bin BinOp Expr Expr
              | Unary UnaryOp Expr
              | CallExpr Expr ArgList
              | NullExpr
              | Null
              | ESeq Expr Expr
              | Bound Expr Expr
              | Sqrt Expr
              | ArrayCon [Expr]
              | AssgExpr String Expr
                deriving (Show, Functor,Typeable,Data, Eq)

data BinOp    = Plus   | Minus | Mul | Div
              | Or     | And  
              | Concat | Power
              | RelEQ | RelNE | RelLT | RelLE | RelGT | RelGE
                deriving (Show, Functor,Typeable,Data,Eq)

data UnaryOp  = UMinus | Not 
                deriving (Show, Functor,Typeable,Data, Eq)

data Spec     = Access Expr
              | Action Expr
              | Advance Expr
              | Blank Expr
              | Delim Expr
              | Direct Expr
              | End Expr
              | Err Expr
              | ExFile Expr
              | Exist Expr
              | Eor Expr
              | File Expr  
              | FMT Expr
              | Form Expr
              | Formatted Expr
              | Unformatted Expr
              | IOLength Expr
              | IOStat Expr
              | Name Expr
              | Named Expr
              | NoSpec Expr
              | Number Expr
              | NextRec Expr
              | NML Expr
              | Opened Expr 
              | Pad Expr
              | Position Expr
              | Read Expr
              | ReadWrite Expr
              | Rec Expr 
              | Recl Expr 
              | Sequential Expr
              | Size Expr
              | Status Expr
              | Unit Expr
              | WriteSp Expr
                deriving (Show, Functor,Typeable,Data, Eq)

-- Fortran pretty printer 

--showAllocate ((e,b):[]) = outputG e++"("++showRanges b++")" --new
--showAllocate ((e,b):as) = outputG e++"("++showRanges b++")"++", "++showAllocate as	--new


-- showElseIf :: Int -> (Expr,Fortran) -> String

showElseIf i (e,f) = (ind i)++"else if ("++outputG e++") then\n"++(ind (i+1))++outputG f++"\n"

showForall [] = "error"
showForall ((s,e,e',NullExpr):[]) = s++"="++outputG e++":"++outputG e'
showForall ((s,e,e',e''):[]) = s++"="++outputG e++":"++outputG e'++"; "++outputG e''
showForall ((s,e,e',NullExpr):is) = s++"="++outputG e++":"++outputG e'++", "++showForall is
showForall ((s,e,e',e''):is) = s++"="++outputG e++":"++outputG e'++"; "++outputG e''++", "++showForall is

showUse :: [String] -> String
showUse ss = concat ( map (\s -> ((ind 1)++"use "++s++"\n")) ss)

-- Printing declarations
-- 
instance (OutputG Arg v, 
          OutputG BaseType v,
          OutputG Block v,
          OutputG Decl v,
          OutputG Implicit v,
          OutputG SubName v,
          OutputG Program v,
          Alts v) => OutputF Program v where
  outputF (Sub (Just p) n a b)  = outputG p ++ " subroutine "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend subroutine "++(outputG n)++"\n"
  outputF (Sub Nothing n a b)  = "subroutine "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend subroutine "++(outputG n)++"\n"
  outputF (Function (Just p) n a b)  = outputG p ++ " function "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend function "++(outputG n)++"\n"
  outputF (Function Nothing n a b) = "function "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend function "++(outputG n)++"\n"
  outputF (Main n a b [])     = "program "++(outputG n) ++ 
                                (if not (isEmptyArg a) then (outputG a) else ""++"\n") ++
                                outputG b ++
                                "\nend program "++ (outputG n) ++"\n"
  outputF (Main n a b ps)     = "program "++(outputG n) ++ 
                                (if not (isEmptyArg a) then (outputG a) else ""++"\n") ++
                                outputG b ++
                                "contains\n" ++
                                (concatMap outputG ps) ++
                                "\nend program "++(outputG n)++"\n"

  outputF (Module n us i ds []) = "module "++(outputG n)++"\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
                          "end module " ++ (outputG n)++"\n"
  outputF (Module n us i ds ps) = "module "++(outputG n)++"\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
			     "\ncontains\n" ++
                             concatMap outputG ps ++
                          "end module " ++ (outputG n)++"\n"
  outputF (BlockData n us i ds) = "block data " ++ (outputG n) ++ "\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
                          "end block data " ++ (outputG n)++"\n"
  outputF (PSeq p p')  = outputG p++outputG p'
  outputF (Prog p)     = outputG p
  outputF NullProg     = ""

instance (OutputG Fortran v, OutputG Decl v, OutputG Implicit v, Alts v) => OutputF Block v where
  outputF (Block us i ds f) = showUse us++outputG i++(outputG ds)++outputG f

instance (OutputG ArgList v,
          OutputG Attr v,
          OutputG BinOp v,
          OutputG Decl v,
          OutputG Expr v, 
          OutputG GSpec v, 
          OutputG InterfaceSpec v, 
          OutputG (SubName) v,
          OutputG UnaryOp v, 
          OutputG VarName v,
          OutputG Type v,
           Alts v) => OutputF Decl v where
  outputF (Decl vs t)  = ind 1++outputG t++" :: "++asSeq id (map showDV vs)++"\n"
  outputF (Namelist ns) = ind 1++"namelist "++show_namelist ns++"\n"
  outputF (Data ds) = ind 1++"data "++(concat (intersperse "\n" (map show_data ds)))  ++"\n"
  outputF (AccessStmt p []) = ind 1++outputG p ++ "\n"
  outputF (AccessStmt p gs) = ind 1++outputG p ++ " :: " ++ (concat . intersperse ", " . map outputG) gs++"\n"
  outputF (ExternalStmt xs)  = ind 1++"external :: " ++ (concat (intersperse "," xs)) ++ "\n"
  outputF (Interface (Just g) is) = ind 1 ++ "interface " ++ outputG g ++ outputG is ++ ind 1 ++ "end interface" ++ outputG g ++ "\n"
  outputF (Interface Nothing  is) = ind 1 ++ "interface " ++ outputG is ++ ind 1 ++ "end interface\n"
  outputF (DerivedTypeDef n as ps ds) = ind 1 ++ "type " ++ showAttrs as ++  " :: " ++ outputG n ++ "\n" ++ ind 2 ++ (concat (intersperse "\n" (map (outputG) ps))) ++ "\n" ++ outputG ds ++ "end type " ++ outputG n ++ "\n"
  outputF (Include i)  = "include "++outputG i
  outputF (DSeq d d')  = outputG d++outputG d'
  outputF NullDecl     = ""
  
show_namelist ((x,xs):[]) = "/" ++ outputG x ++ "/" ++ (concat (intersperse ", " (map outputG xs)))
show_namelist ((x,xs):ys) = "/" ++ outputG x ++ "/" ++ (concat (intersperse ", " (map outputG xs))) ++ "," ++ show_namelist ys
show_data     ((xs,ys)) = "/" ++  outputG xs ++ "/" ++ outputG ys

-- showDV :: (Expr,Expr) -> String

showDV (v, NullExpr) = outputF v
showDV (v,e) = outputF v++" = "++outputF e

instance (OutputG ArgList v, 
          OutputG BinOp v, 
          OutputG UnaryOp v,
          OutputG BaseType v,
          OutputG Expr v,
          OutputG VarName v,
          Alts v) => OutputF Type v where
  outputF (BaseType bt as NullExpr  NullExpr)   = outputG bt++showAttrs as
  outputF (BaseType bt as NullExpr e')          = outputG bt++" (len="++outputG e'++")"++showAttrs as
  outputF (BaseType bt as e NullExpr)           = outputG bt++" (kind="++outputG e++")"++showAttrs as
  outputF (BaseType bt as e               e')                = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++showAttrs as
  outputF (ArrayT [] bt as NullExpr NullExpr)   = outputG bt++showAttrs as
  outputF (ArrayT [] bt as NullExpr e')         = outputG bt++" (len="++outputG e'++")"++showAttrs as
  outputF (ArrayT [] bt as e NullExpr)          = outputG bt++" (kind="++outputG e++")"++showAttrs as
  outputF (ArrayT [] bt as e                e')              = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++showAttrs as
  outputF (ArrayT rs bt as NullExpr  NullExpr)  = outputG bt++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT rs bt as NullExpr e')         = outputG bt++" (len="++outputG e'++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT rs bt as e NullExpr)          = outputG bt++" (kind="++outputG e++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT rs bt as e               e')               = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++" , dimension ("++showRanges rs++")"++showAttrs as


instance Alts v => OutputF Attr v where --new
  outputF Allocatable    = "allocatable "
  outputF Parameter      = "parameter "
  outputF External       = "external "
  outputF (Intent In)    = "intent(in) "
  outputF (Intent Out)   = "intent(out) "
  outputF (Intent InOut) = "intent(inout) "
  outputF Intrinsic      = "intrinsic "
  outputF Optional       = "optional "
  outputF Pointer        = "pointer "
  outputF Save           = "save "
  outputF Target         = "target "
  outputF Volatile       = "volatile "
  outputF Public         = "public "
  outputF Private        = "private "
  outputF Sequence       = "sequence "

instance (OutputG Arg v, OutputG BinOp v, OutputG Expr v, Alts v) => OutputF GSpec v where
  outputF (GName s)  = outputG s
  outputF (GOper op) = "operator("++outputG op++")"
  outputF (GAssg)    = "assignment(=)"

instance (OutputG Arg v, OutputG Decl v, OutputG Implicit v,
          OutputG SubName v, Alts v) => OutputF InterfaceSpec v where
  outputF (FunctionInterface s as us i ds)   = (ind 1)++ "function " ++ outputG s ++ outputG as ++ showUse us ++ outputG i ++ outputG ds ++ "\nend function " ++ outputG s
  outputF (SubroutineInterface s as us i ds) = (ind 1)++ "subroutine " ++ outputG s ++ outputG as ++ showUse us ++ outputG i ++ outputG ds ++ "\nend subroutine " ++ outputG s
  outputF (ModuleProcedure ss) = (ind 2) ++ "module procedure " ++ concat (intersperse ", " (map (outputG) ss))


instance (OutputG SubName v, Alts v) => OutputF BaseType v where
  outputF Integer   = "integer"
  outputF Real      = "real"
  outputF Character = "character"
  outputF (DerivedType s) = "type ("++outputG s++")"
  outputF SomeType  = error "sometype not valid in output source file"

-- Printing statements and expressions
-- 
instance (OutputG ArgList v,
          OutputG BinOp v,
          OutputG Expr v,
          OutputG UnaryOp v,
          OutputG VarName v,
          Alts v) => OutputF Expr v where
  outputF (Con i)         = i
  outputF (ConS s)        = s
  outputF (Var vs)        = showPartRefList vs
  outputF (Bin bop e@(Bin op _ _) e'@(Bin op' _ _)) = checkPrec bop op (paren) (outputG e)++outputG bop++ checkPrec bop op' (paren) (outputG e')
  outputF (Bin bop e@(Bin op _ _) e')                      = checkPrec bop op (paren) (outputG e)++outputG bop++outputG e'
  outputF (Bin bop e                    e'@(Bin op' _ _))  = outputG e++outputG bop++checkPrec bop op' (paren) (outputG e')
  outputF (Bin bop e                    e')                      = outputG e++outputG bop++outputG e'
  outputF (Unary uop e)   = "("++outputG uop++outputG e++")"
  outputF (CallExpr s as) = outputG s ++ outputG as
  outputF (Null)          = "NULL()"
  outputF (NullExpr)      = ""
  outputF (ESeq e e')     = outputG e++","++outputG e'
  outputF (Bound e e')    = outputG e++":"++outputG e'
  outputF (Sqrt e)        = "sqrt("++outputG e++")"
  outputF (ArrayCon es)   = "(\\" ++ concat (intersperse ", " (map (outputG) es)) ++ "\\)"
  outputF (AssgExpr v e)  = v ++ "=" ++ outputG e

instance (OutputIndF Fortran v, Alts v) => OutputF Fortran v where
  outputF = outputIndF 1

instance (OutputG ArgName v, Alts v) => OutputF Arg v where
  outputF (Arg vs) = "("++ outputG vs ++")"
  
instance (OutputG Expr v, Alts v) => OutputF ArgList v where
  outputF (ArgList es) = "("++outputG es++")" -- asTuple outputG es
  
instance Alts v => OutputF BinOp v where
  outputF Plus   = "+"
  outputF Minus  = "-" 
  outputF Mul    = "*"
  outputF Div    = "/"
  outputF Or     = ".or."
  outputF And    = ".and."
  outputF Concat = "//"
  outputF Power  = "**"
  outputF RelEQ  = "=="
  outputF RelNE  = "/="
  outputF RelLT  = "<"
  outputF RelLE  = "<="
  outputF RelGT  = ">"
  outputF RelGE  = ">="

instance Alts v => OutputF UnaryOp v where
  outputF UMinus = "-"
  outputF Not    = ".not."
  
instance Alts v => OutputF VarName v where
  outputF (VarName v) = v  

instance (OutputG VarName v, OutputG ArgName v, Alts v) => OutputF ArgName v where
  outputF (ArgName a)            = a  
  outputF (ASeq NullArg NullArg) = ""
  outputF (ASeq NullArg  a')     = outputG a'
  outputF (ASeq a NullArg)       = outputG a
  outputF (ASeq a a')            = outputG a++","++outputG a'
  outputF NullArg                            = ""

instance Alts v => OutputF SubName v where
  outputF (SubName n) = n
  outputF (NullSubName) = error "subroutine needs a name"

instance Alts v => OutputF Implicit v where
  outputF ImplicitNone = "   implicit none\n"
  outputF ImplicitNull = ""
  
instance (OutputG Expr v, Alts v) => OutputF Spec v where
  outputF (Access        s) = "access = " ++ outputG s
  outputF (Action        s) = "action = "++outputG s
  outputF (Advance       s) = "advance = "++outputG s
  outputF (Blank         s) = "blank = "++outputG s
  outputF (Delim         s) = "delim = "++outputG s
  outputF (Direct        s) = "direct = "++outputG s
  outputF (End           s) = "end = "++outputG s
  outputF (Eor           s) = "eor = "++outputG s
  outputF (Err           s) = "err = "++outputG s
  outputF (Exist         s) = "exist = "++outputG s
  outputF (File          s) = "file = "++outputG s
  outputF (FMT           s) = "fmt = "++outputG s
  outputF (Form          s) = "form = "++outputG s
  outputF (Formatted     s) = "formatted = "++outputG s
  outputF (Unformatted   s) = "unformatted = "++outputG s
  outputF (IOLength      s) = "iolength = "++outputG s
  outputF (IOStat        s) = "iostat = "++outputG s
  outputF (Opened        s) = "opened = "++outputG s
  outputF (Name          s) = "name = "++outputG s
  outputF (Named         s) = "named = "++outputG s
  outputF (NextRec       s) = "nextrec = "++outputG s
  outputF (NML           s) = "nml = "++outputG s
  outputF (NoSpec        s) = outputG s
  outputF (Number        s) = "number = "++outputG s
  outputF (Pad           s) = "pad = "++outputG s
  outputF (Position      s) = "position = "++outputG s
  outputF (Read          s) = "read = "++outputG s
  outputF (ReadWrite     s) = "readwrite = "++outputG s
  outputF (WriteSp       s) = "write = "++outputG s
  outputF (Rec           s) = "rec = "++outputG s
  outputF (Recl          s) = "recl = "++outputG s
  outputF (Sequential    s) = "sequential = "++outputG s
  outputF (Size          s) = "size = "++outputG s
  outputF (Status        s) = "status = "++outputG s
  outputF (Unit s)          = "unit = "++outputG s



-- smart constructors for language 'constants', that is, expressions
-- 

con  = Con
arr v es = Var [(v,es)]

var :: String -> Expr
var s = Var [(VarName s,[])]

v :: String -> Expr
v s = Var [(VarName s,[])]

var2 :: VarName -> Expr
var2 x = Var [(x,[])]

c :: String -> Expr 
c = con 

c2 (VarName v) = ConS (outputG v)

agn :: String -> ArgName
agn s = ArgName s

agv :: VarName -> ArgName
agv (VarName s) = agn s

($+), ($-), ($*), ($/) :: Expr -> Expr -> Expr
($+) e1 e2 = Bin Plus  e1 e2
($-) e1 e2 = Bin Minus e1 e2
($*) e1 e2 = Bin Mul   e1 e2
($/) e1 e2 = Bin Div   e1 e2

assg v  e          = Assg v  e
for  v  e1 e2 e3 f = For  v  e1 e2 e3 f
fseq f1 f2         = FSeq f1 f2
call s  es         = Call s  es

block us p ds f = Block us ImplicitNull ds f

ne = NullExpr

isEmptyArg (Arg as) = and (isEmptyArgName as)
isEmptyArgName (ASeq a a') = isEmptyArgName a ++ isEmptyArgName a'
isEmptyArgName (ArgName a) = [False]
isEmptyArgName (NullArg)   = [True]

paren :: String -> String
paren s = "(" ++ s ++ ")"

checkPrec :: BinOp -> BinOp -> (a -> a) -> a -> a
checkPrec pop cop f s = if opPrec pop >= opPrec cop then f s else s

opPrec :: BinOp -> Int
opPrec Or     = 0
opPrec And    = 1
opPrec RelEQ  = 2
opPrec RelNE  = 2
opPrec RelLT  = 2
opPrec RelLE  = 2 
opPrec RelGT  = 2
opPrec RelGE  = 2
opPrec Concat = 3
opPrec Plus   = 4
opPrec Minus  = 4
opPrec Mul    = 5
opPrec Div    = 5
opPrec Power  = 6



instance (OutputG VarName v,
          OutputG Expr v,
          OutputG ArgList v,
          OutputIndG Fortran v,
          OutputG Fortran v, OutputG Spec v, Alts v) => OutputIndF Fortran v where
    outputIndF i (Assg v e)               = (ind i)++outputG v++" = "++outputG e
    outputIndF i (For v e e' e'' f)       = (ind i)++"do"++" "++outputG v++" = "++outputG e++", "++
                                         outputG e'++", "++outputG e''++"\n"++
                                         (outputIndG (i+1) f)++"\n"++(ind i)++"end do"
    outputIndF i (FSeq f f')              = outputIndG i f++"\n"++outputIndG i f'
    outputIndF i (If e f [] Nothing)      = (ind i)++"if ("++outputG e++") then\n"
                                         ++(outputIndG (i+1) f)++"\n"
                                         ++(ind i)++"end if"
    outputIndF i (If e f [] (Just f'))    = (ind i)++"if ("++outputG e++") then\n"
                                         ++(outputIndG (i+1) f)++"\n"
                                         ++(ind i)++"else\n"
                                         ++(outputIndG (i+1) f')++"\n"
                                         ++(ind i)++"end if"
    outputIndF i (If e f elsif Nothing)    = (ind i)++"if ("++outputG e++") then\n"
                                          ++(outputIndG (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"end if"
    outputIndF i (If e f elsif (Just f')) = (ind i)++"if ("++outputG e++") then\n"
                                          ++(outputIndG (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"else\n"
                                          ++(outputIndG (i+1) f')++"\n"
                                          ++(ind i)++"end if"
    outputIndF i (Allocate a NullExpr)    = (ind i)++"allocate (" ++ outputG a ++ ")"
    outputIndF i (Allocate a s)              = (ind i)++"allocate ("++ outputG a ++ ", STAT = "++outputG s++ ")"
    outputIndF i (Backspace ss)               = (ind i)++"backspace "++asTuple outputG ss++"\n"
    outputIndF i (Call sub al)                = ind i++"call "++outputG sub++outputG al
    outputIndF i (Open s)                     = (ind i)++"open "++asTuple outputG s++"\n"
    outputIndF i (Close ss)                   = (ind i)++"close "++asTuple outputG ss++"\n"
    outputIndF i (Continue)                   = (ind i)++"continue"++"\n"
    outputIndF i (Cycle s)                    = (ind i)++"cycle "++outputG s++"\n"
    outputIndF i (Deallocate es e)            = (ind i)++"deallocate "++asTuple outputG es++outputG e++"\n"
    outputIndF i (Endfile ss)                 = (ind i)++"endfile "++asTuple outputG ss++"\n"
    outputIndF i (Exit s)                     = (ind i)++"exit "++outputG s
    outputIndF i (Forall (is, NullExpr) f)    = (ind i)++"forall ("++showForall is++") "++outputG f
    outputIndF i (Forall (is,e)            f) = (ind i)++"forall ("++showForall is++","++outputG e++") "++outputG f
    outputIndF i (Goto s)                     = (ind i)++"goto "++outputG s
    outputIndF i (Nullify es)                 = (ind i)++"nullify "++asTuple outputG es++"\n"
    outputIndF i (Inquire ss es)              = (ind i)++"inquire "++asTuple outputG ss++" "++(concat (intersperse "," (map outputG es)))++"\n"
    outputIndF i (Rewind ss)                  = (ind i)++"rewind "++asTuple outputG ss++"\n"
    outputIndF i (Stop e)                     = (ind i)++"stop "++outputG e++"\n"
    outputIndF i (Where e f)                  = (ind i)++"where ("++outputG e++") "++outputG f
    outputIndF i (Write ss es)                = (ind i)++"write "++asTuple outputG ss++" "++(concat (intersperse "," (map outputG es)))++"\n"
    outputIndF i (PointerAssg e e')           = (ind i)++outputG e++" => "++outputG e'++"\n"
    outputIndF i (Return e)                   = (ind i)++"return "++outputG e++"\n"
    outputIndF i (Label s f)                  = s++" "++outputG f
    outputIndF i (Print e [])                 = (ind i)++("print ")++outputG e++("\n")
    outputIndF i (Print e es)                 = (ind i)++("print ")++outputG e++", "++(concat (intersperse "," (map outputG es)))++("\n")
    outputIndF i (ReadS ss es)                = (ind i)++("read ")++(asTuple outputG ss)++" "++(concat (intersperse "," (map outputG es)))++("\n")
    outputIndF i (NullStmt)		       = ""



|]

infix 7 $+
infix 7 $-
infix 8 $*
infix 9 $/

----------------------------------------------------------------------
-- PRINT UTILITIES
----------------------------------------------------------------------

showNQ :: Show a => a -> String
showNQ = filter ('"'/=) . show

indent i l = take (i*l) (repeat ' ')

printList sep f xs = sep!!0++concat (intersperse (sep!!1) (map f xs))++sep!!2

asTuple = printList ["(",",",")"]
asSeq   = printList ["",",",""]
asList  = printList ["[",",","]"]
asSet   = printList ["{",",","}"]
asLisp  = printList ["("," ",")"]
asPlain f xs = if null xs then "" else printList [" "," ",""] f xs
asPlain' f xs = if null xs then "" else printList [""," ",""] f xs
asCases l = printList ["\n"++ind++"   ","\n"++ind++" | ",""] where ind = indent 4 l
asDefs n = printList ["\n"++n,"\n"++n,"\n"]
asParagraphs = printList ["\n","\n\n","\n"]

-- Auxiliary functions
-- 
optTuple :: (?variant :: v, Alts v, OutputG (UnaryOp p) v, OutputF (Expr p) v) => [Expr p] -> String
optTuple [] = ""
optTuple xs = asTuple outputF xs
-- *optTuple xs = ""
-- indent and showInd enable indented printing
-- 
ind = indent 3 

showAttrs :: (Alts v, ?variant :: v, OutputF (Attr p) v) => [Attr p] -> String
showAttrs  = concat . map (", "++) . map (outputF)



showBounds :: (Alts v, ?variant :: v, OutputF (Expr p) v) => (Expr p,Expr p) -> String
showBounds (NullExpr _, NullExpr _) = ":"
showBounds (NullExpr _, e) = outputF e
showBounds (e1,e2) = outputF e1++":"++outputF e2

showRanges :: (Alts v, ?variant :: v, OutputF (Expr p) v) => [(Expr p, Expr p)] -> String
showRanges = asSeq showBounds

showPartRefList :: (Alts v, ?variant :: v, OutputG (VarName p) v, 
                    OutputG (UnaryOp p) v, OutputF (Expr p) v) => [(VarName p,[Expr p])] -> String
showPartRefList []           = ""
showPartRefList ((v,es):[]) = outputG v ++ optTuple es 
showPartRefList ((v,es):xs) = outputG v ++ optTuple es ++ "%" ++ showPartRefList xs