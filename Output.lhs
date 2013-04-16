> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE ScopedTypeVariables #-}

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

> outputHTML :: forall p . (Data p, Typeable p, OutputG p Alt2) => Fortran.Program p -> String
> outputHTML prog = unpack html
>                 where
>                   t :: SubName p -> SubName p
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

Output routines specialised to the analysis.
                   


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
>     outputG (For p v e e' e'' f) = "do"++" "++outputG v++" = "++outputG e++", "++
>                                    outputG e'++", "++outputG e''++"\n"++
>                                    "<span style='color:#707d8f'>"++"{"++outputG p++"}</span>\n" ++ 
>                                    (outputIndG 1 f)++"\n"++(ind 1)++"end do"
>     outputG t = outputF t

> instance OutputG (Spec p) Alt2 where
>     outputG = outputF

> instance OutputG p Alt2 => OutputIndG (Fortran p) Alt2 where
>     outputIndG i (For p v e e' e'' f) = (ind i) ++ "do"++" "++outputG v++" = "++outputG e++", "++
>                                         outputG e'++", "++outputG e''++"\n"++
>                                         "<span style='color:#707d8f'>"++(ind i)++"{"++outputG p++"}</span>\n" ++ 
>                                         (outputIndG (i+1) f)++"\n"++(ind i)++"end do"
>     outputIndG i t = outputIndF i t

