> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE KindSignatures #-}

> {-# LANGUAGE ScopedTypeVariables #-}

> module Output where

> import Annotations
> import Traverse
> import Language.Fortran as Fortran
> import Language.Fortran.Pretty

> import Data.Text hiding (foldl,map, concatMap)
> import qualified Data.Text as Text 
> import Data.Map.Lazy hiding (map, foldl)
> import Data.Generics.Annotate
> import Data.Generics
> import Data.Generics.Uniplate.Data

> import Language.Haskell.Syntax (SrcLoc(..), srcLine, srcColumn)
> import Data.Generics.Zipper

> import Data.Maybe

> import Debug.Trace

> purple = "#800080"
> green = "#008000"
> blue = "#000080"

> keyword = map pack
>           ["end","subroutine","function","program","module","block","data", "common",
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
                   
> instance OutputG Bool Alt2 where
>     outputG = show

> instance OutputG SrcLoc Alt2 where
>     outputG _ = "" -- not sure if I want this to shown

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
>                               

>     outputG (For p v e e' e'' f) = "do"++" "++outputG v++" = "++outputG e++", "++
>                                    outputG e'++", "++outputG e''++"\n"++
>                                    "<span style='color:#707d8f'>"++"{"++outputG p++"}</span>\n" ++ 
>                                    (outputIndG 1 f)++"\n"++(ind 1)++"end do"
>     outputG t = outputF t

> instance OutputG (Spec p) Alt2 where
>     outputG = outputF

> instance OutputIndG (Fortran A1) Alt2 where
>     outputIndG = outputIndF

> instance OutputIndG (Fortran Annotation) Alt2 where

>     outputIndG i t@(For p v e e' e'' f) = (outputAnn p False i) ++ 
>                                           annotationMark i t
>                                           ((ind i) ++ "do"++" "++outputG v++" = "++
>                                            outputG e++", "++
>                                            outputG e'++", "++outputG e''++"\n"++
>                                            (outputIndG (i+1) f)++"\n"++(ind i)++"end do")

                                         
>     -- outputIndG i t@(FSeq p f1 f2) =  (outputAnn p False i) ++ outputIndG i f1 ++ outputIndG i f2
>     outputIndG i t = "<div style=''>" ++ (outputAnn (rextract t) False i) ++  (annotationMark i t (outputIndF i t)) ++ "</div>"

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


> type A1 =  ((SrcLoc, SrcLoc), Bool)

> lineCol :: SrcLoc -> (Int, Int)
> lineCol x = (srcLine x, srcColumn x)

> inBounds :: SrcLoc -> (SrcLoc, SrcLoc) -> Bool
> inBounds x (l,u) = (lineCol x) >= (lineCol l) && (lineCol x) < (lineCol u)

> takeBounds (l, u) inp = takeBounds' (lineCol l, lineCol u) [] inp

> takeBounds' ((ll, lc), (ul, uc)) tk inp =
>     if (ll == ul && lc == uc) then (Prelude.reverse tk, inp)
>     else case inp of []             -> (Prelude.reverse tk, inp)
>                      ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
>                      ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)


> reprint :: String -> String -> Program A1 -> String
> reprint input f z = let input' = Prelude.lines input
>                    in reprintA (SrcLoc f 1 1) (SrcLoc f (Prelude.length input') (1 + (Prelude.length $ Prelude.last input'))) input' (toZipper z)

> doHole :: SrcLoc -> [String] -> Zipper (d A1) -> (String, SrcLoc)
> doHole cursor inp z = case (getHole z)::(Maybe (Fortran A1)) of
>                           Just e  -> let ((lb, ub), flag) = copoint e
>                                          (p1, rest1) = takeBounds (cursor, lb) inp
>                                      in  if flag then let ?variant = Alt2 in (p1 ++ outputF e, ub)
>                                          else case (down' z) of
>                                                    Just cz -> (p1 ++ reprintA lb ub rest1 cz, ub)
>                                                    Nothing -> let (p2, _) = takeBounds (lb, ub) rest1
>                                                               in (p1, ub)
>                           Nothing -> case (down' z) of 
>                                        Just cz -> (reprintA cursor cursor inp cz, cursor)
>                                        Nothing -> ("", cursor)

> reprintA :: SrcLoc -> SrcLoc -> [String] -> Zipper (d A1) -> String
> reprintA cursor end inp z = let (p1, cursor') = doHole cursor inp z
>                                 (p2, inp')    = takeBounds (cursor, cursor') inp
>                             in p1 ++ case (right z) of 
>                                         Just rz -> reprintA cursor' end inp' rz
>                                         Nothing -> fst $ takeBounds (cursor', end) inp'
