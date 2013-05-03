> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE OverlappingInstances #-}

> {-# LANGUAGE ScopedTypeVariables #-}

> module Output where

> import Annotations
> import Traverse
> import Language.Fortran as Fortran

> import Data.Text hiding (foldl,map, concatMap)
> import qualified Data.Text as Text 
> import Data.Map.Lazy hiding (map, foldl)
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

> outputHTML :: forall p . (Data p, Typeable p, OutputG p Alt2, OutputIndG (Fortran p) Alt2) => Fortran.Program p -> String
> outputHTML prog = unpack html
>                 where
>                   t :: SubName p -> SubName p
>                   t (SubName p n) = SubName p (addColor blue n)
>                   t x = x
                  

>                   html = let ?variant = Alt2
>                          in 
>                            (Text.append (pack "<head><script type='text/javascript' src='source.js'></script><link href='source.css' type='text/css' rel='stylesheet' /></head>"))
>                          . (Text.concat . (map pre) . Text.lines)
>                          . (\t -> foldl (toColor green) t types)
>                          . (\t -> foldl (toColor purple) t keyword)
>                          . (pack . outputF)
>                          -- . (pack . output) 
>                          -- . (pack . paraBi (\p -> \ss -> (showPara p) ++ ss) "")
>                          -- . (pack . (para (\p -> \ss -> showPara p ++ (Prelude.concat ss))))
>                          . (transformBi t) $ prog

Output routines specialised to the analysis.
                   


> instance (OutputIndG (Fortran p) Alt2, OutputG p Alt2) => OutputG (Program p) Alt2 where
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

> instance (OutputIndG (Fortran p) Alt2, OutputG p Alt2) => OutputG (Block p) Alt2 where
>     outputG = outputF

> instance (OutputIndG (Fortran p) Alt2, OutputG p Alt2) => OutputG (Fortran p) Alt2 where
>     outputG (For p v e e' e'' f) = "do"++" "++outputG v++" = "++outputG e++", "++
>                                    outputG e'++", "++outputG e''++"\n"++
>                                    "<span style='color:#707d8f'>"++"{"++outputG p++"}</span>\n" ++ 
>                                    (outputIndG 1 f)++"\n"++(ind 1)++"end do"
>     outputG t = outputF t

> instance OutputG (Spec p) Alt2 where
>     outputG = outputF

> instance OutputIndG (Fortran Annotation) Alt2 where
>     outputIndG i t@(For p v e e' e'' f) = (outputAnn p True i) ++ 
>                                           annotationMark i t
>                                           ((ind i) ++ "do"++" "++outputG v++" = "++
>                                            outputG e++", "++
>                                            outputG e'++", "++outputG e''++"\n"++
>                                            (outputIndG (i+1) f)++"\n"++(ind i)++"end do")
>                                         
>     outputIndG i t@(FSeq p f1 f2) = outputIndG i f1 ++ outputIndG i f2
>     outputIndG i t =  (annotationMark i t (outputIndF i t)) ++ (outputAnn (rextract t) False i)

> annotationMark i t x = "<div class='clickable' onClick='toggle(" ++  
>                        (show $ number (rextract t)) ++ ");'>" ++
>                        x ++ "</div>"


> row xs = "<tr>" ++ (concatMap (\x -> "<td>" ++ x ++ "</td>") xs) ++ "</tr>"

> instance OutputG Annotation Alt2 where
>     outputG t = outputAnn t False 0

> outputAnn t visible i =
>      "<div id='a" ++ (show $ number t) ++ "' style='" ++
>      (if visible then "" else "display:none;") ++
>      "' class'outer'><div class='spacer'><pre>" ++ (indent 3 i) ++ "</pre></div>" ++ 
>      "<div class='annotation'><div class='number'>" ++ (show $ number t) ++ 
>      "</div><p><table>" ++
>      row ["lives:",    showList $ lives t] ++ 
>      row ["indices:",  showList $ indices t] ++ 
>      row ["arrays R:", showExps (assocs $ arrsRead t)] ++ 
>      row ["arrays W:", showExps (assocs $ arrsWrite t)] ++
>      "</table></p></div><br />\n\r\n" 
>          where
>            listToPair x       = "(" ++ listToPair' x ++ ")"
>            listToPair' []     = ""
>            listToPair' [x]    = outputF x
>            listToPair' (x:xs) = outputF x ++ ", " ++ listToPair' xs

>            showList x    = "" ++ showList' x ++ ""
>            showList' []  = ""
>            showList' [x] = x
>            showList' (x:xs) = x ++ ", " ++ showList' xs

>            showExps []           = ""
>            showExps [(v, es)]    = "[" ++ v ++ ": " ++ (showList $ map listToPair es) ++ "]"
>            showExps ((v, es):ys) = (showExps [(v, es)]) ++ ", " ++ (showExps ys)

