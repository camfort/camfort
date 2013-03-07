> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> module Output where

> import Language.Fortran as Fortran

> import Data.Text hiding (foldl,map)
> import qualified Data.Text as Text

> import Data.Generics.Annotate

> import Data.Generics
> import Data.Generics.Uniplate.Data

> purple = "#800080"
> green = "#008000"
> blue = "#000080"

> keyword = map pack
>           ["end","subroutine","function","program","module","block","data",
>            "namelist", "external", "interface", "type", "include",
>            "len", "kind", "dimension", "allocatable", "parameter", "external",
>            "intent", "intrinsic", "optional", "pointer", "save", "target",
>            "volatile", "public", "private", "sequence", "operator", "assignment",
>            "procedure", "do", "if", "else", "then", "allocate", "backspace", 
>            "call", "open", "close", "continue", "cycle", "deallocate", "endfile",
>            "exit", "forall", "goto", "nullify", "inquire", "rewind", "stop", "where",
>            "write", "reurn", "print", "read", "write", "implicit", "use"]

> addColor c k = "<span style='color:" ++ c ++ "'>" ++ k ++ "</span>"
> toColor c t k = replace k (Text.concat [pack ("<span style='color:" ++ c ++ "'>"), k, pack "</span>"]) t

> types = map pack ["real", "integer", "character", "type", "logical"]

> pre l = Text.concat [pack "<pre>", l, pack "</pre>"]

> outputHTML :: Fortran.Program String -> String
> outputHTML prog = unpack html
>                 where
>                   t :: SubName String -> SubName String
>                   t (SubName p n) = SubName p (addColor blue n)
>                   t x = x
                  

>                   html = let ?variant = Alt2
>                          in 
>                            (Text.append (pack "<style>pre {margin:1px;}</style>"))
>                          . (Text.concat . (map pre) . Text.lines)
>                          . (\t -> foldl (toColor green) t types)
>                          . (\t -> foldl (toColor purple) t keyword)
>                          . (pack . outputF)
>                          -- . (pack . output) 
>                          -- . (pack . paraBi (\p -> \ss -> (showPara p) ++ ss) "")
>                          -- . (pack . (para (\p -> \ss -> showPara p ++ (Prelude.concat ss))))
>                          . (transformBi t) $ prog
>                   
> instance OutputG p Alt2 => OutputG (Program p) Alt2 where
>     outputG = outputF

> instance OutputG (SubName p) Alt2 where
>     outputG = outputF

> instance OutputG (Implicit p) Alt2 where
>     outputG = outputF

> instance OutputG (Decl p) Alt2 where
>     outputG = outputF

> instance OutputG (Type p) Alt2 where
>     outputG = outputF

> instance OutputG (VarName p) Alt2 where
>     outputG = outputF

> instance OutputG (Expr p) Alt2 where
>     outputG = outputF

> instance OutputG (UnaryOp p) Alt2 where
>     outputG = outputF

> instance OutputG (BinOp p) Alt2 where
>     outputG = outputF

> instance OutputG (ArgList p) Alt2 where
>     outputG = outputF

> instance OutputG (BaseType p) Alt2 where
>     outputG = outputF

> instance OutputG (InterfaceSpec p) Alt2 where
>     outputG = outputF

> instance OutputG (Arg p) Alt2 where
>     outputG = outputF

> instance OutputG (ArgName p) Alt2 where
>     outputG = outputF

> instance OutputG (GSpec p) Alt2 where
>     outputG = outputF

> instance OutputG (Attr p) Alt2 where
>     outputG = outputF

> instance OutputG p Alt2 => OutputG (Block p) Alt2 where
>     outputG = outputF

> instance OutputG p Alt2 => OutputG (Fortran p) Alt2 where
>     outputG (For p v e e' e'' f) = (ind 1)++"["++outputG p++"]do"++" "++outputG v++" = "++outputG e++", "++
>                                    outputG e'++", "++outputG e''++"\n"++
>                                    (outputIndG 1 f)++"\n"++(ind 1)++"end do"
>     outputG t = outputF t

> instance OutputG (Spec p) Alt2 where
>     outputG = outputF

> instance OutputG p Alt2 => OutputIndG (Fortran p) Alt2 where
>     outputIndG i (For p v e e' e'' f) = (ind i)++"["++outputG p++"]do"++" "++outputG v++" = "++outputG e++", "++
>                                         outputG e'++", "++outputG e''++"\n"++
>                                         (outputIndG (i+1) f)++"\n"++(ind i)++"end do"
>     outputIndG i t = outputIndF i t



 paraBi :: Biplate from to => (to -> r -> r) -> r -> from -> r
 paraBi op r x = Prelude.foldr op r (childrenBi x)

 (For p v e e' e'' f) = (ind 1)++"["++p++"]do"++" "++show v++" = "++show e++", "++
                                            show e'++", "++show e''++"\n"++
                                       (showInd (1) f)++"\n"++(ind 1)++"end do"

 transformBi 

 showPara :: Fortran.Fortran String -> String
 showPara (For p v e e' e'' f)       = (ind 1)++"["++p++"] do"++" "++show v++" = "++show e++", "++
                                            show e'++", "++show e''++"\n"++
                                        (showInd (1) f)++"\n"++(ind 1)++"end do"
 showPara t = show t

 class Show t => Output t where
     output :: t -> String
               
 instance Output Program  where
   output (Sub (Just p) n a b)  = output p ++ " subroutine "++(output n)++output a++"\n"++
                                  output b++
                                 "\nend subroutine "++(output n)++"\n"
   output (Sub Nothing n a b)  = "subroutine "++(output n)++output a++"\n"++
                                  output b++
                                 "\nend subroutine "++(output n)++"\n"
   output (Function (Just p) n a b)  = output p ++ " function "++(output n)++output a++"\n"++
                                       output b++
                                       "\nend function "++(output n)++"\n"
   output (Function Nothing n a b) = "function "++(output n)++output a++"\n"++
                                     output b++
                                     "\nend function "++(output n)++"\n"
   output (Main n a b)     = "program "++(output n)++if not (isEmptyArg a) then (output a) else ""++"\n"++
                             output b++
                           "\nend program "++(output n)++"\n"
   output (Module n us i ds []) = "module "++(output n)++"\n" ++
                                  showUse us ++
                                  output i ++
                                  output ds ++
                                  "end module " ++ (output n)++"\n"

   output (Module n us i ds ps) = "module "++(output n)++"\n" ++
                                 showUse us ++
                                 output i ++
                                 output ds ++
							 "contains\n" ++
                                 Prelude.concatMap output ps ++
                                 "end module " ++ (output n)++"\n"
   output (BlockData n us i ds) = "block data " ++ (output n) ++ "\n" ++
                                 showUse us ++
                                 output i ++
                                 output ds ++
                                 "end block data " ++ (output n)++"\n"
   output (PSeq p p')  = output p++output p'
   output (Prog p)     = output p
   output NullProg     = ""


 instance Output (Fortran.Fortran String) where
     output t@(For p _ _ _ _ _) = p ++ "<div style='border:1px'>" ++ show t ++ "</div>"
     output t = show t

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
    showInd i (NullStmt)		           = ""

