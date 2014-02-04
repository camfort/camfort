{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fortran.Show where

import Language.Fortran
import Data.Generics.Annotate

import Data.List

[annotateFrom| {Language.Fortran} 

-- Auxiliary functions
-- 
optTuple [] = ""
optTuple xs = asTuple show xs
-- *optTuple xs = ""
-- indent and showInd enable indented printing
-- 
ind = indent 3 
-- *ind = indent


--class Output t where
--    output :: t -> String

--instance Output Char

--instance Output a => Output [a] where
--    output xs = "[" ++ go xs ++ "]" where go [] = "" 
 --                                         go [x] = output x
  --                                        go (x:xs) = output x ++ ", " ++ (go xs)


--showAllocate ((e,b):[]) = show e++"("++showRanges b++")" --new
--showAllocate ((e,b):as) = show e++"("++showRanges b++")"++", "++showAllocate as	--new


showElseIf :: Int -> (Expr,Fortran) -> String
showElseIf i (e,f) = (ind i)++"else if ("++show e++") then\n"++(ind (i+1))++show f++"\n"

showForall [] = "error"
showForall ((s,e,e',NullExpr):[]) = s++"="++show e++":"++show e'
showForall ((s,e,e',e''):[]) = s++"="++show e++":"++show e'++"; "++show e''
showForall ((s,e,e',NullExpr):is) = s++"="++show e++":"++show e'++", "++showForall is
showForall ((s,e,e',e''):is) = s++"="++show e++":"++show e'++"; "++show e''++", "++showForall is

showUse :: [String] -> String
showUse ss = concat ( map (\s -> ((ind 1)++"use "++s++"\n")) ss)

-- Printing declarations
-- 
instance Show Program where
  show (Sub (Just p) n a b)  = show p ++ " subroutine "++(show n)++show a++"\n"++
                             show b++
                          "\nend subroutine "++(show n)++"\n"
  show (Sub Nothing n a b)  = "subroutine "++(show n)++show a++"\n"++
                             show b++
                          "\nend subroutine "++(show n)++"\n"
  show (Function (Just p) n a b)  = show p ++ " function "++(show n)++show a++"\n"++
                             show b++
                          "\nend function "++(show n)++"\n"
  show (Function Nothing n a b) = "function "++(show n)++show a++"\n"++
                             show b++
                          "\nend function "++(show n)++"\n"
  show (Main n a b)     = "program "++(show n)++if not (isEmptyArg a) then (show a) else ""++"\n"++
                             show b++
                          "\nend program "++(show n)++"\n"
  show (Module n us i ds []) = "module "++(show n)++"\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
                          "end module " ++ (show n)++"\n"
  show (Module n us i ds ps) = "module "++(show n)++"\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
							 "contains\n" ++
                             concatMap show ps ++
                          "end module " ++ (show n)++"\n"
  show (BlockData n us i ds) = "block data " ++ (show n) ++ "\n" ++
                             showUse us ++
                             show i ++
                             show ds ++
                          "end block data " ++ (show n)++"\n"
  show (PSeq p p')  = show p++show p'
  show (Prog p)     = show p
  show NullProg     = ""

instance Show Block where
  show (Block us i ds f) = showUse us++show i++(show ds)++show f

instance Show Decl where
  show (Decl vs t)  = ind 1++show t++" :: "++asSeq id (map showDV vs)++"\n"
  show (Namelist ns) = ind 1++"namelist "++show_namelist ns++"\n"
  show (Data ds) = ind 1++"data "++(concat (intersperse "\n" (map show_data ds)))  ++"\n"
  show (AccessStmt p []) = ind 1++show p ++ "\n"
  show (AccessStmt p gs) = ind 1++show p ++ " :: " ++ (concat . intersperse ", " . map show) gs++"\n"
  show (ExternalStmt xs)  = ind 1++"external :: " ++ (concat (intersperse "," xs)) ++ "\n"
  show (Interface (Just g) is) = ind 1 ++ "interface " ++ show g ++ show is ++ ind 1 ++ "end interface" ++ show g ++ "\n"
  show (Interface Nothing  is) = ind 1 ++ "interface " ++ show is ++ ind 1 ++ "end interface\n"
  show (DerivedTypeDef n as ps ds) = ind 1 ++ "type " ++ showAttrs as ++  " :: " ++ show n ++ "\n" ++ ind 2 ++ (concat (intersperse "\n" (map (show) ps))) ++ "\n" ++ show ds ++ "end type " ++ show n ++ "\n"
  show (Include i)  = "include "++show i
  show (DSeq d d')  = show d++show d'
  show NullDecl     = ""
  
show_namelist ((x,xs):[]) = "/" ++ show x ++ "/" ++ (concat (intersperse ", " (map show xs)))
show_namelist ((x,xs):ys) = "/" ++ show x ++ "/" ++ (concat (intersperse ", " (map show xs))) ++ "," ++ show_namelist ys
show_data     ((xs,ys)) = "/" ++  show xs ++ "/" ++ show ys

showDV :: (Expr,Expr) -> String
showDV (v, NullExpr) = show v
showDV (v,e) = show v++" = "++show e

instance Show Type where
  show (BaseType bt as NullExpr  NullExpr)   = show bt++showAttrs as
  show (BaseType bt as NullExpr e')          = show bt++" (len="++show e'++")"++showAttrs as
  show (BaseType bt as e NullExpr)           = show bt++" (kind="++show e++")"++showAttrs as
  show (BaseType bt as e               e')                = show bt++" (len="++show e'++"kind="++show e++")"++showAttrs as
  show (ArrayT [] bt as NullExpr NullExpr)   = show bt++showAttrs as
  show (ArrayT [] bt as NullExpr e')         = show bt++" (len="++show e'++")"++showAttrs as
  show (ArrayT [] bt as e NullExpr)          = show bt++" (kind="++show e++")"++showAttrs as
  show (ArrayT [] bt as e                e')              = show bt++" (len="++show e'++"kind="++show e++")"++showAttrs as
  show (ArrayT rs bt as NullExpr  NullExpr)  = show bt++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as NullExpr e')         = show bt++" (len="++show e'++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as e NullExpr)          = show bt++" (kind="++show e++")"++" , dimension ("++showRanges rs++")"++showAttrs as
  show (ArrayT rs bt as e               e')               = show bt++" (len="++show e'++"kind="++show e++")"++" , dimension ("++showRanges rs++")"++showAttrs as

showAttrs :: [Attr] -> String
showAttrs  = concat . map (", "++) . map (show)

instance Show Attr where --new
  show Allocatable    = "allocatable "
  show Parameter      = "parameter "
  show External       = "external "
  show (Intent In)    = "intent(in) "
  show (Intent Out)   = "intent(out) "
  show (Intent InOut) = "intent(inout) "
  show Intrinsic      = "intrinsic "
  show Optional       = "optional "
  show Pointer        = "pointer "
  show Save           = "save "
  show Target         = "target "
  show Volatile       = "volatile "
  show Public         = "public "
  show Private        = "private "
  show Sequence       = "sequence "

instance Show GSpec where
  show (GName s)  = show s
  show (GOper op) = "operator("++show op++")"
  show (GAssg)    = "assignment(=)"

instance Show InterfaceSpec where
  show (FunctionInterface s as us i ds)   = (ind 1)++ "function " ++ show s ++ show as ++ showUse us ++ show i ++ show ds ++ "\nend function " ++ show s
  show (SubroutineInterface s as us i ds) = (ind 1)++ "subroutine " ++ show s ++ show as ++ showUse us ++ show i ++ show ds ++ "\nend subroutine " ++ show s
  show (ModuleProcedure ss) = (ind 2) ++ "module procedure " ++ concat (intersperse ", " (map (show) ss))


showBounds :: (Expr,Expr) -> String
showBounds (NullExpr, NullExpr) = ":"
showBounds (NullExpr, e) = show e
showBounds (e1,e2) = show e1++":"++show e2

showRanges :: [(Expr,Expr)] -> String
showRanges = asSeq showBounds

showPartRefList :: [(VarName,[Expr])] -> String
showPartRefList []           = ""
showPartRefList ((v,es):[]) = show v ++ optTuple es 
showPartRefList ((v,es):xs) = show v ++ optTuple es ++ "%" ++ showPartRefList xs

instance Show BaseType where
  show Integer   = "integer"
  show Real      = "real"
  show Character = "character"
  show (DerivedType s) = "type ("++show s++")"
  show SomeType  = error "sometype not valid in output source file"

-- Printing statements and expressions
-- 
instance Show Expr where
  show (Con i)         = i
  show (ConS s)        = s
  show (Var vs)        = showPartRefList vs
  show (Bin bop e@(Bin op _ _) e'@(Bin op' _ _)) = checkPrec bop op (paren) (show e)++show bop++ checkPrec bop op' (paren) (show e')
  show (Bin bop e@(Bin op _ _) e')                      = checkPrec bop op (paren) (show e)++show bop++show e'
  show (Bin bop e                    e'@(Bin op' _ _))  = show e++show bop++checkPrec bop op' (paren) (show e')
  show (Bin bop e                    e')                      = show e++show bop++show e'
  show (Unary uop e)   = "("++show uop++show e++")"
  show (CallExpr s as) = show s ++ show as
  show (Null)          = "NULL()"
  show (NullExpr)      = ""
  show (ESeq e e')     = show e++","++show e'
  show (Bound e e')    = show e++":"++show e'
  show (Sqrt e)        = "sqrt("++show e++")"
  show (ArrayCon es)   = "(\\" ++ concat (intersperse ", " (map (show) es)) ++ "\\)"
  show (AssgExpr v e)  = v ++ "=" ++ show e

instance Show Fortran where
  show = showInd 1

instance Show Arg where
  show (Arg vs) = "("++ show vs ++")"
  
instance Show ArgList where
  show (ArgList es) = "("++show es++")" -- asTuple show es
  
instance Show BinOp where
  show Plus   = "+"
  show Minus  = "-" 
  show Mul    = "*"
  show Div    = "/"
  show Or     = ".or."
  show And    = ".and."
  show Concat = "//"
  show Power  = "**"
  show RelEQ  = "=="
  show RelNE  = "/="
  show RelLT  = "<"
  show RelLE  = "<="
  show RelGT  = ">"
  show RelGE  = ">="

instance Show UnaryOp where
  show UMinus = "-"
  show Not    = ".not."
  
instance Show VarName where
  show (VarName v) = v  

instance Show ArgName where
  show (ArgName a)            = a  
  show (ASeq NullArg NullArg) = ""
  show (ASeq NullArg  a')     = show a'
  show (ASeq a NullArg)       = show a
  show (ASeq a a')            = show a++","++show a'
  show NullArg                            = ""

instance Show SubName where
  show (SubName n) = n
  show (NullSubName) = error "subroutine needs a name"

instance Show Implicit where
  show ImplicitNone = "   implicit none\n"
  show ImplicitNull = ""
  
instance Show Spec where
  show (Access        s) = "access = " ++ show s
  show (Action        s) = "action = "++show s
  show (Advance       s) = "advance = "++show s
  show (Blank         s) = "blank = "++show s
  show (Delim         s) = "delim = "++show s
  show (Direct        s) = "direct = "++show s
  show (End           s) = "end = "++show s
  show (Eor           s) = "eor = "++show s
  show (Err           s) = "err = "++show s
  show (Exist         s) = "exist = "++show s
  show (File          s) = "file = "++show s
  show (FMT           s) = "fmt = "++show s
  show (Form          s) = "form = "++show s
  show (Formatted     s) = "formatted = "++show s
  show (Unformatted   s) = "unformatted = "++show s
  show (IOLength      s) = "iolength = "++show s
  show (IOStat        s) = "iostat = "++show s
  show (Opened        s) = "opened = "++show s
  show (Name          s) = "name = "++show s
  show (Named         s) = "named = "++show s
  show (NextRec       s) = "nextrec = "++show s
  show (NML           s) = "nml = "++show s
  show (NoSpec        s) = show s
  show (Number        s) = "number = "++show s
  show (Pad           s) = "pad = "++show s
  show (Position      s) = "position = "++show s
  show (Read          s) = "read = "++show s
  show (ReadWrite     s) = "readwrite = "++show s
  show (WriteSp       s) = "write = "++show s
  show (Rec           s) = "rec = "++show s
  show (Recl          s) = "recl = "++show s
  show (Sequential    s) = "sequential = "++show s
  show (Size          s) = "size = "++show s
  show (Status        s) = "status = "++show s
  show (Unit s)          = "unit = "++show s



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

c2 (VarName v) = ConS (show v)

agn :: String -> ArgName
agn s = ArgName s

agv :: VarName -> ArgName
agv (VarName s) = agn s

($+), ($-), ($*), ($/) :: Expr -> Expr -> Expr
($+) e1 e2 = Bin Plus  e1 e2
($-) e1 e2 = Bin Minus e1 e2
($*) e1 e2 = Bin Mul   e1 e2
($/) e1 e2 = Bin Div   e1 e2

infix 7 $+
infix 7 $-
infix 8 $*
infix 9 $/

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

showInd :: Int -> Fortran -> String
showInd i (Assg v e)               = (ind i)++show v++" = "++show e
showInd i (For v e e' e'' f)       = (ind i)++"do"++" "++show v++" = "++show e++", "++
                                         show e'++", "++show e''++"\n"++
                                         (showInd (i+1) f)++"\n"++(ind i)++"end do"
showInd i (FSeq f f')              = showInd i f++"\n"++showInd i f'
showInd i (If e f [] Nothing)      = (ind i)++"if ("++show e++") then\n"
                                         ++(showInd (i+1) f)++"\n"
                                         ++(ind i)++"end if"
showInd i (If e f [] (Just f'))    = (ind i)++"if ("++show e++") then\n"
                                         ++(showInd (i+1) f)++"\n"
                                         ++(ind i)++"else\n"
                                         ++(showInd (i+1) f')++"\n"
                                         ++(ind i)++"end if"
showInd i (If e f elsif Nothing)    = (ind i)++"if ("++show e++") then\n"
                                          ++(showInd (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"end if"
showInd i (If e f elsif (Just f')) = (ind i)++"if ("++show e++") then\n"
                                          ++(showInd (i+1) f)++"\n"
                                          ++concat (map (showElseIf i) elsif)
                                          ++(ind i)++"else\n"
                                          ++(showInd (i+1) f')++"\n"
                                          ++(ind i)++"end if"
showInd i (Allocate a NullExpr)    = (ind i)++"allocate (" ++ show a ++ ")"
showInd i (Allocate a s)              = (ind i)++"allocate ("++ show a ++ ", STAT = "++show s++ ")"
showInd i (Backspace ss)               = (ind i)++"backspace "++asTuple show ss++"\n"
showInd i (Call sub al)                = ind i++"call "++show sub++show al
showInd i (Open s)                     = (ind i)++"open "++asTuple show s++"\n"
showInd i (Close ss)                   = (ind i)++"close "++asTuple show ss++"\n"
showInd i (Continue)                   = (ind i)++"continue"++"\n"
showInd i (Cycle s)                    = (ind i)++"cycle "++show s++"\n"
showInd i (Deallocate es e)            = (ind i)++"deallocate "++asTuple show es++show e++"\n"
showInd i (Endfile ss)                 = (ind i)++"endfile "++asTuple show ss++"\n"
showInd i (Exit s)                     = (ind i)++"exit "++show s
showInd i (Forall (is, NullExpr) f)    = (ind i)++"forall ("++showForall is++") "++show f
showInd i (Forall (is,e)            f) = (ind i)++"forall ("++showForall is++","++show e++") "++show f
showInd i (Goto s)                     = (ind i)++"goto "++show s
showInd i (IfStmt e f)                 = (ind i)++"if ("++show e++") "++show f
showInd i (Nullify es)                 = (ind i)++"nullify "++asTuple show es++"\n"
showInd i (Inquire ss es)              = (ind i)++"inquire "++asTuple show ss++" "++(concat (intersperse "," (map show es)))++"\n"
showInd i (Rewind ss)                  = (ind i)++"rewind "++asTuple show ss++"\n"
showInd i (Stop e)                     = (ind i)++"stop "++show e++"\n"
showInd i (Where e f)                  = (ind i)++"where ("++show e++") "++show f
showInd i (Write ss es)                = (ind i)++"write "++asTuple show ss++" "++(concat (intersperse "," (map show es)))++"\n"
showInd i (PointerAssg e e')           = (ind i)++show e++" => "++show e'++"\n"
showInd i (Return e)                   = (ind i)++"return "++show e++"\n"
showInd i (Label s f)                  = s++" "++show f
showInd i (Print e [])                 = (ind i)++("print ")++show e++("\n")
showInd i (Print e es)                 = (ind i)++("print ")++show e++", "++(concat (intersperse "," (map show es)))++("\n")
showInd i (ReadS ss es)                = (ind i)++("read ")++(asTuple show ss)++" "++(concat (intersperse "," (map show es)))++("\n")
showInd i (NullStmt)		       = ""


|]