-- 
-- Pretty.hs  - 
-- Based on code by Martin Erwid from Parameterized Fortran
--

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

module Language.Fortran.Pretty where

import Language.Fortran

import Data.List

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


-- Fortran pretty printer 

--showAllocate ((e,b):[]) = outputG e++"("++showRanges b++")" --new
--showAllocate ((e,b):as) = outputG e++"("++showRanges b++")"++", "++showAllocate as	--new


-- showElseIf :: Int -> (Expr,Fortran) -> String

showElseIf i (e,f) = (ind i)++"else if ("++outputG e++") then\n"++(ind (i+1))++outputG f++"\n"

showForall [] = "error"
showForall ((s,e,e',NullExpr _ _):[]) = s++"="++outputG e++":"++outputG e'
showForall ((s,e,e',e''):[]) = s++"="++outputG e++":"++outputG e'++"; "++outputG e''
showForall ((s,e,e',NullExpr _ _):is) = s++"="++outputG e++":"++outputG e'++", "++showForall is
showForall ((s,e,e',e''):is) = s++"="++outputG e++":"++outputG e'++"; "++outputG e''++", "++showForall is

showUse :: [String] -> String
showUse ss = concat ( map (\s -> ((ind 1)++"use "++s++"\n")) ss)

-- Printing declarations
-- 
instance (OutputG (Arg p) v, 
          OutputG (BaseType p) v,
          OutputG (Block p) v,
          OutputG (Decl p) v,
          OutputG (Implicit p) v,
          OutputG (SubName p) v,
          OutputG (Program p) v,
          Alts v) => OutputF (Program p) v where
  outputF (Sub _ _ (Just p) n a b)  = outputG p ++ " subroutine "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend subroutine "++(outputG n)++"\n"
  outputF (Sub _ _ Nothing n a b)  = "subroutine "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend subroutine "++(outputG n)++"\n"
  outputF (Function _ _ (Just p) n a b)  = outputG p ++ " function "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend function "++(outputG n)++"\n"
  outputF (Function _ _ Nothing n a b) = "function "++(outputG n)++outputG a++"\n"++
                             outputG b++
                          "\nend function "++(outputG n)++"\n"
  outputF (Main _ _ n a b [])     = "program "++(outputG n) ++ 
                                (if not (isEmptyArg a) then (outputG a) else ""++"\n") ++
                                outputG b ++
                                "\nend program "++ (outputG n) ++"\n"
  outputF (Main _ _ n a b ps)     = "program "++(outputG n) ++ 
                                (if not (isEmptyArg a) then (outputG a) else ""++"\n") ++
                                outputG b ++
                                "\ncontains\n" ++
                                (concatMap outputG ps) ++
                                "\nend program "++(outputG n)++"\n"

  outputF (Module _ _ n us i ds []) = "module "++(outputG n)++"\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
                          "end module " ++ (outputG n)++"\n"
  outputF (Module _ _ n us i ds ps) = "module "++(outputG n)++"\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
			     "\ncontains\n" ++
                             concatMap outputG ps ++
                          "end module " ++ (outputG n)++"\n"
  outputF (BlockData _ _ n us i ds) = "block data " ++ (outputG n) ++ "\n" ++
                             showUse us ++
                             outputG i ++
                             outputG ds ++
                          "end block data " ++ (outputG n)++"\n"
  outputF (PSeq _ _ p p')  = outputG p++outputG p'
  outputF (Prog _ _ p)     = outputG p
  outputF (NullProg _ _)    = ""

instance (OutputG (Fortran p) v, OutputG (Decl p) v, OutputG (Implicit p) v, Alts v) =>
            OutputF (Block p) v where
  outputF (Block _ us i ds f) = showUse us++outputG i++(outputG ds)++outputG f

instance (OutputG (ArgList p) v,
          OutputG (Attr p) v,
          OutputG (BinOp p) v,
          OutputG (Decl p) v,
          OutputG (Expr p) v, 
          OutputG (GSpec p) v, 
          OutputG (InterfaceSpec p) v, 
          OutputG (SubName p) v,
          OutputG (UnaryOp p) v, 
          OutputG (VarName p) v,
          OutputG (Type p) v,
           Alts v) => OutputF (Decl p) v where
  outputF (Decl _ vs t)  = ind 1++outputG t++" :: "++asSeq id (map showDV vs)++"\n"
  outputF (Namelist _ ns) = ind 1++"namelist "++show_namelist ns++"\n"
  outputF (Data _ ds) = ind 1++"data "++(concat (intersperse "\n" (map show_data ds)))  ++"\n"
  outputF (AccessStmt _ p []) = ind 1++outputG p ++ "\n"
  outputF (AccessStmt _ p gs) = ind 1++outputG p ++ " :: " ++ (concat . intersperse ", " . map outputG) gs++"\n"
  outputF (ExternalStmt _ xs)  = ind 1++"external :: " ++ (concat (intersperse "," xs)) ++ "\n"
  outputF (Interface _ (Just g) is) = ind 1 ++ "interface " ++ outputG g ++ outputG is ++ ind 1 ++ "end interface" ++ outputG g ++ "\n"
  outputF (Common _ name exps) = ind 1++"common " ++ (case name of 
                                                     Just n -> "/" ++ n ++ "/ "
                                                     Nothing -> "") ++ (concat (intersperse "," (map outputF exps))) ++ "\n"
  outputF (Interface _ Nothing  is) = ind 1 ++ "interface " ++ outputG is ++ ind 1 ++ "end interface\n"
  outputF (DerivedTypeDef _ n as ps ds) = ind 1 ++ "type " ++ showAttrs as ++  " :: " ++ outputG n ++ "\n" ++ ind 2 ++ (concat (intersperse "\n" (map (outputG) ps))) ++ "\n" ++ outputG ds ++ "end type " ++ outputG n ++ "\n"
  outputF (Include _ i)  = "include "++outputG i
  outputF (DSeq _ d d')  = outputG d++outputG d'
  outputF (NullDecl _)    = ""
  
show_namelist ((x,xs):[]) = "/" ++ outputG x ++ "/" ++ (concat (intersperse ", " (map outputG xs)))
show_namelist ((x,xs):ys) = "/" ++ outputG x ++ "/" ++ (concat (intersperse ", " (map outputG xs))) ++ "," ++ show_namelist ys
show_data     ((xs,ys)) = "/" ++  outputG xs ++ "/" ++ outputG ys

-- showDV :: (Expr,Expr) -> String

showDV (v, NullExpr _ _) = outputF v
showDV (v,e) = outputF v++" = "++outputF e

instance (OutputG (ArgList p) v, 
          OutputG (BinOp p) v, 
          OutputG (UnaryOp p) v,
          OutputG (BaseType p) v,
          OutputG (Expr p) v,
          OutputG (VarName p) v,
          Alts v) => OutputF (Type p) v where
  outputF (BaseType _ bt as (NullExpr _ _)  (NullExpr _ _))   = outputG bt++showAttrs as
  outputF (BaseType _ bt as (NullExpr _ _) e')          = outputG bt++" (len="++outputG e'++")"++showAttrs as
  outputF (BaseType _ bt as e (NullExpr _ _))           = outputG bt++" (kind="++outputG e++")"++showAttrs as
  outputF (BaseType _ bt as e               e')                = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++showAttrs as
  outputF (ArrayT _ [] bt as (NullExpr _ _) (NullExpr _ _))   = outputG bt++showAttrs as
  outputF (ArrayT _ [] bt as (NullExpr _ _) e')         = outputG bt++" (len="++outputG e'++")"++showAttrs as
  outputF (ArrayT _ [] bt as e (NullExpr _ _))          = outputG bt++" (kind="++outputG e++")"++showAttrs as
  outputF (ArrayT _ [] bt as e                e')              = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++showAttrs as
  outputF (ArrayT _ rs bt as (NullExpr _ _)  (NullExpr _ _))  = outputG bt++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT _ rs bt as (NullExpr _ _) e')         = outputG bt++" (len="++outputG e'++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT _ rs bt as e (NullExpr _ _))          = outputG bt++" (kind="++outputG e++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  outputF (ArrayT _ rs bt as e               e')               = outputG bt++" (len="++outputG e'++"kind="++outputG e++")"++" , dimension ("++showRanges rs++")"++showAttrs as


instance Alts v => OutputF (Attr p) v where --new
    outputF (Allocatable _)      = "allocatable "
    outputF (Parameter _)        = "parameter "
    outputF (External _)         = "external "
    outputF (Intent _  (In _))   = "intent(in) "
    outputF (Intent _ (Out _))   = "intent(out) "
    outputF (Intent _ (InOut _)) = "intent(inout) "
    outputF (Intrinsic _)        = "intrinsic "
    outputF (Optional _)         = "optional "
    outputF (Pointer _)          = "pointer "
    outputF (Save _)             = "save "
    outputF (Target _)           = "target "
    outputF (Volatile _)         = "volatile "
    outputF (Public _)           = "public "
    outputF (Private _)          = "private "
    outputF (Sequence _)         = "sequence "

instance (OutputG (Arg p) v, OutputG (BinOp p) v, OutputG (Expr p) v, Alts v) => OutputF (GSpec p) v where
  outputF (GName _ s)  = outputG s
  outputF (GOper _ op) = "operator("++outputG op++")"
  outputF (GAssg _)    = "assignment(=)"

instance (OutputG (Arg p) v, OutputG (Decl p) v, OutputG (Implicit p) v,
          OutputG (SubName p) v, Alts v) => OutputF (InterfaceSpec p) v where
  outputF (FunctionInterface _ s as us i ds)   = (ind 1)++ "function " ++ outputG s ++ outputG as ++ showUse us ++ outputG i ++ outputG ds ++ "\nend function " ++ outputG s
  outputF (SubroutineInterface _ s as us i ds) = (ind 1)++ "subroutine " ++ outputG s ++ outputG as ++ showUse us ++ outputG i ++ outputG ds ++ "\nend subroutine " ++ outputG s
  outputF (ModuleProcedure _ ss) = (ind 2) ++ "module procedure " ++ concat (intersperse ", " (map (outputG) ss))


instance (OutputG (SubName p) v, Alts v) => OutputF (BaseType p) v where
  outputF (Integer _)       = "integer"
  outputF (Real    _)       = "real"
  outputF (Character  _)    = "character"
  outputF (Logical   _)     = "logical"
  outputF (DerivedType _ s) = "type ("++outputG s++")"
  outputF (SomeType _)      = error "sometype not valid in output source file"

-- Printing statements and expressions
-- 
instance (OutputG (ArgList p) v,
          OutputG (BinOp p) v,
          OutputG (Expr p) v,
          OutputG (UnaryOp p) v,
          OutputG (VarName p) v,
          Alts v) => OutputF (Expr p) v where
  outputF (Con _ _ i)         = i
  outputF (ConS _ _ s)        = s
  outputF (Var _ _ vs)        = showPartRefList vs
  outputF (Bin _ _ bop e@(Bin _ _ op _ _ ) e'@(Bin _ _ op' _ _)) = checkPrec bop op (paren) (outputG e)++outputG bop++ checkPrec bop op' (paren) (outputG e')
  outputF (Bin _ _ bop e@(Bin _ _ op _ _) e')                      = checkPrec bop op (paren) (outputG e)++outputG bop++outputG e'
  outputF (Bin _ _ bop e                    e'@(Bin _ _ op' _ _))  = outputG e++outputG bop++checkPrec bop op' (paren) (outputG e')
  outputF (Bin _ _ bop e                    e')                      = outputG e++outputG bop++outputG e'
  outputF (Unary _ _ uop e)   = "("++outputG uop++outputG e++")"
  outputF (CallExpr  _ _ s as) = outputG s ++ outputG as
  outputF (Null _ _)          = "NULL()"
  outputF (NullExpr _ _)      = ""
  outputF (ESeq _ _ e e')     = outputG e++","++outputG e'
  outputF (Bound _ _ e e')    = outputG e++":"++outputG e'
  outputF (Sqrt _ _ e)        = "sqrt("++outputG e++")"
  outputF (ArrayCon _ _ es)   = "(\\" ++ concat (intersperse ", " (map (outputG) es)) ++ "\\)"
  outputF (AssgExpr _ _ v e)  = v ++ "=" ++ outputG e

instance (OutputIndF (Fortran p) v, Alts v) => OutputF (Fortran p) v where
  outputF = outputIndF 1

instance (OutputG (ArgName p) v, Alts v) => OutputF (Arg p) v where
  outputF (Arg _ vs) = "("++ outputG vs ++")"
  
instance (OutputG (Expr p) v, Alts v) => OutputF (ArgList p) v where
  outputF (ArgList _ es) = "("++outputG es++")" -- asTuple outputG es
  
instance Alts v => OutputF (BinOp p) v where
  outputF (Plus  _) ="+"
  outputF (Minus _) ="-" 
  outputF (Mul   _) ="*"
  outputF (Div   _) ="/"
  outputF (Or    _) =".or."
  outputF (And   _) =".and."
  outputF (Concat _) ="//"
  outputF (Power _) ="**"
  outputF (RelEQ _) ="=="
  outputF (RelNE _) ="/="
  outputF (RelLT _) ="<"
  outputF (RelLE _) ="<="
  outputF (RelGT _) =">"
  outputF (RelGE _) =">="

instance Alts v => OutputF (UnaryOp p) v where
  outputF (UMinus _) = "-"
  outputF (Not    _) = ".not."
  
instance Alts v => OutputF (VarName p) v where
  outputF (VarName _ v) = v  

instance (OutputG (VarName p) v, OutputG (ArgName p) v, Alts v) => OutputF (ArgName p) v where
    outputF (ArgName _ a)                    = a  
    outputF (ASeq _ (NullArg _) (NullArg _)) = ""
    outputF (ASeq _ (NullArg _)  a')         = outputG a'
    outputF (ASeq _ a (NullArg _))           = outputG a
    outputF (ASeq _ a a')                    = outputG a++","++outputG a'
    outputF (NullArg _)                            = ""

instance Alts v => OutputF (SubName p) v where
  outputF (SubName _ n)   = n
  outputF (NullSubName _) = error "subroutine needs a name"

instance Alts v => OutputF ( Implicit p) v where
  outputF (ImplicitNone _) = "   implicit none\n"
  outputF (ImplicitNull _) = ""
  
instance (OutputG (Expr p) v, Alts v) => OutputF (Spec p) v where
  outputF (Access        _ s) = "access = " ++ outputG s
  outputF (Action        _ s) = "action = "++outputG s
  outputF (Advance       _ s) = "advance = "++outputG s
  outputF (Blank         _ s) = "blank = "++outputG s
  outputF (Delim         _ s) = "delim = "++outputG s
  outputF (Direct        _ s) = "direct = "++outputG s
  outputF (End           _ s) = "end = "++outputG s
  outputF (Eor           _ s) = "eor = "++outputG s
  outputF (Err           _ s) = "err = "++outputG s
  outputF (Exist         _ s) = "exist = "++outputG s
  outputF (File          _ s) = "file = "++outputG s
  outputF (FMT           _ s) = "fmt = "++outputG s
  outputF (Form          _ s) = "form = "++outputG s
  outputF (Formatted     _ s) = "formatted = "++outputG s
  outputF (Unformatted   _ s) = "unformatted = "++outputG s
  outputF (IOLength      _ s) = "iolength = "++outputG s
  outputF (IOStat        _ s) = "iostat = "++outputG s
  outputF (Opened        _ s) = "opened = "++outputG s
  outputF (Name          _ s) = "name = "++outputG s
  outputF (Named         _ s) = "named = "++outputG s
  outputF (NextRec       _ s) = "nextrec = "++outputG s
  outputF (NML           _ s) = "nml = "++outputG s
  outputF (NoSpec        _ s) = outputG s
  outputF (Number        _ s) = "number = "++outputG s
  outputF (Pad           _ s) = "pad = "++outputG s
  outputF (Position      _ s) = "position = "++outputG s
  outputF (Read          _ s) = "read = "++outputG s
  outputF (ReadWrite     _ s) = "readwrite = "++outputG s
  outputF (WriteSp       _ s) = "write = "++outputG s
  outputF (Rec           _ s) = "rec = "++outputG s
  outputF (Recl          _ s) = "recl = "++outputG s
  outputF (Sequential    _ s) = "sequential = "++outputG s
  outputF (Size          _ s) = "size = "++outputG s
  outputF (Status        _ s) = "status = "++outputG s
  outputF (Unit _ s)          = "unit = "++outputG s




isEmptyArg (Arg _ as) = and (isEmptyArgName as)
isEmptyArgName (ASeq _ a a') = isEmptyArgName a ++ isEmptyArgName a'
isEmptyArgName (ArgName _ a) = [False]
isEmptyArgName (NullArg _)   = [True]

paren :: String -> String
paren s = "(" ++ s ++ ")"

checkPrec :: BinOp p -> BinOp p -> (a -> a) -> a -> a
checkPrec pop cop f s = if opPrec pop >= opPrec cop then f s else s

opPrec :: BinOp p -> Int
opPrec (Or    _) = 0
opPrec (And   _) = 1
opPrec (RelEQ _) = 2
opPrec (RelNE _) = 2
opPrec (RelLT _) = 2
opPrec (RelLE _) = 2 
opPrec (RelGT _) = 2
opPrec (RelGE _) = 2
opPrec (Concat _) = 3
opPrec (Plus  _) = 4
opPrec (Minus _) = 4
opPrec (Mul   _) = 5
opPrec (Div   _) = 5
opPrec (Power _) = 6


instance (OutputG (VarName p) v,
          OutputG (Expr p) v,
          OutputG (UnaryOp p) v,
          OutputG (BinOp p) v, 
          OutputG (ArgList p) v,
          OutputIndG (Fortran p) v,
          OutputG (Fortran p) v, OutputG (Spec p) v, Alts v) => OutputIndF (Fortran p) v where

    outputIndF i (Assg _ _ v e)               = (ind i)++outputG v++" = "++outputG e
    outputIndF i (For _ _  v e e' e'' f)       = (ind i)++"do"++" "++outputG v++" = "++outputG e++", "++
                                         outputG e'++", "++outputG e''++"\n"++
                                         (outputIndG (i+1) f)++"\n"++(ind i)++"end do"
    outputIndF i (FSeq _ _  f f')              = outputIndG i f++"\n"++outputIndG i f'
    outputIndF i (If _ _  e f [] Nothing)      = (ind i)++"if ("++outputG e++") then\n"
                                         ++(outputIndG (i+1) f)++"\n"
                                         ++(ind i)++"end if"
    outputIndF i (If _ _  e f [] (Just f'))    = (ind i)++"if ("++outputG e++") then\n"
                                         ++(outputIndG (i+1) f)++"\n"
                                         ++(ind i)++"else\n"
                                         ++(outputIndG (i+1) f')++"\n"
                                         ++(ind i)++"end if"
    outputIndF i (If _ _  e f elsif Nothing)    = (ind i)++"if ("++outputG e++") then\n"
                                          ++(outputIndG (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"end if"
    outputIndF i (If _ _  e f elsif (Just f')) = (ind i)++"if ("++outputG e++") then\n"
                                          ++(outputIndG (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"else\n"
                                          ++(outputIndG (i+1) f')++"\n"
                                          ++(ind i)++"end if"
    outputIndF i (Allocate _ _  a (NullExpr _ _))    = (ind i)++"allocate (" ++ outputG a ++ ")"
    outputIndF i (Allocate _ _  a s)              = (ind i)++"allocate ("++ outputG a ++ ", STAT = "++outputG s++ ")"
    outputIndF i (Backspace _ _  ss)               = (ind i)++"backspace "++asTuple outputG ss++"\n"
    outputIndF i (Call  _ _ sub al)                = ind i++"call "++outputG sub++outputG al
    outputIndF i (Open  _ _ s)                     = (ind i)++"open "++asTuple outputG s++"\n"
    outputIndF i (Equivalence  _ _ vs)             = ind i++"equivlance ("++(concat (intersperse "," (map outputF vs))) ++ ")\n"
    outputIndF i (Close  _ _ ss)                   = (ind i)++"close "++asTuple outputG ss++"\n"
    outputIndF i (Continue _ _)                   = (ind i)++"continue"++"\n"
    outputIndF i (Cycle _ _ s)                    = (ind i)++"cycle "++outputG s++"\n"
    outputIndF i (Deallocate _ _ es e)            = (ind i)++"deallocate "++asTuple outputG es++outputG e++"\n"
    outputIndF i (Endfile _ _ ss)                 = (ind i)++"endfile "++asTuple outputG ss++"\n"
    outputIndF i (Exit _ _ s)                     = (ind i)++"exit "++outputG s
    outputIndF i (Forall _ _ (is, (NullExpr _ _)) f)    = (ind i)++"forall ("++showForall is++") "++outputG f
    outputIndF i (Forall _ _ (is,e)            f) = (ind i)++"forall ("++showForall is++","++outputG e++") "++outputG f
    outputIndF i (Goto _ _ s)                     = (ind i)++"goto "++outputG s
    outputIndF i (Nullify _ _ es)                 = (ind i)++"nullify "++asTuple outputG es++"\n"
    outputIndF i (Inquire _ _ ss es)              = (ind i)++"inquire "++asTuple outputG ss++" "++(concat (intersperse "," (map outputG es)))++"\n"
    outputIndF i (Rewind _ _  ss)                  = (ind i)++"rewind "++asTuple outputG ss++"\n"
    outputIndF i (Stop _ _ e)                     = (ind i)++"stop "++outputG e++"\n"
    outputIndF i (Where _ _ e f)                  = (ind i)++"where ("++outputG e++") "++outputG f
    outputIndF i (Write _ _ ss es)                = (ind i)++"write "++asTuple outputG ss++" "++(concat (intersperse "," (map outputG es)))++"\n"
    outputIndF i (PointerAssg _ _ e e')           = (ind i)++outputG e++" => "++outputG e'++"\n"
    outputIndF i (Return _ _ e)                   = (ind i)++"return "++outputG e++"\n"
    outputIndF i (Label _ _ s f)                  = s++" "++outputG f
    outputIndF i (Print _ _ e [])                 = (ind i)++("print ")++outputG e++("\n")
    outputIndF i (Print _ _ e es)                 = (ind i)++("print ")++outputG e++", "++(concat (intersperse "," (map outputG es)))++("\n")
    outputIndF i (ReadS _ _ ss es)                = (ind i)++("read ")++(asTuple outputG ss)++" "++(concat (intersperse "," (map outputG es)))++("\n")
    outputIndF i (NullStmt _ _)		       = ""

-- infix 7 $+
-- infix 7 $-
-- infix 8 $*
-- infix 9 $/

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
showBounds (NullExpr _ _, NullExpr _ _) = ":"
showBounds (NullExpr _ _, e) = outputF e
showBounds (e1,e2) = outputF e1++":"++outputF e2

showRanges :: (Alts v, ?variant :: v, OutputF (Expr p) v) => [(Expr p, Expr p)] -> String
showRanges = asSeq showBounds

showPartRefList :: (Alts v, ?variant :: v, OutputG (VarName p) v, 
                    OutputG (UnaryOp p) v, OutputF (Expr p) v) => [(VarName p,[Expr p])] -> String
showPartRefList []           = ""
showPartRefList ((v,es):[]) = outputG v ++ optTuple es 
showPartRefList ((v,es):xs) = outputG v ++ optTuple es ++ "%" ++ showPartRefList xs