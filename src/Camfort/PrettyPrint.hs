{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances, ImplicitParams, DoAndIfThenElse,
             MultiParamTypeClasses, FlexibleContexts, KindSignatures, ScopedTypeVariables,
             DeriveGeneric, DeriveDataTypeable #-}

{-

 Provides pretty printing related code

-}

module Camfort.PrettyPrint where

import Camfort.Analysis.Syntax
import Camfort.Analysis.Annotations
import Camfort.Helpers
import Camfort.Traverse

import qualified Language.Fortran as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

import Data.Map.Lazy hiding (map, foldl)
import qualified Data.ByteString.Char8 as B
import Data.Text hiding (zip,foldl,map,concatMap,take,drop,length,last,head,tail,replicate,concat)
import qualified Data.Text as Text
import Data.List
import Data.Generics.Uniplate.Data
import Data.Generics
import GHC.Generics
import Data.Char
import Data.Maybe
import Control.Monad.Trans.State.Lazy
import Text.Printf

class PrettyPrint p where
  prettyPrint :: p -> SourceText

instance (PrintMaster (Program Annotation) DefaultPP) => PrettyPrint (Program Annotation) where
  prettyPrint p = let ?variant = DefaultPP in B.pack $ printMaster p

-- Define new pretty printing version for HTML output
data HTMLPP = HTMLPP
instance PPVersion HTMLPP

{-| Convert source code to a pretty-printed HTML format -}
outputHTMLA :: Fortran.ProgUnit Annotation -> String
outputHTMLA x = outputHTML x

outputHTML :: forall p . (Data p, Typeable p, PrintSlave p HTMLPP, PrintSlave (Decl p) HTMLPP, PrintIndSlave (Fortran p) HTMLPP, Indentor (Decl p), Indentor (Fortran p)) =>
              Fortran.ProgUnit p -> String
outputHTML prog = unpack html
                where
                  t :: SubName p -> SubName p
                  t (SubName p n) = SubName p (addColor blue n)
                  t x = x

                  purple = "#800080"
                  green = "#008000"
                  blue = "#000080"

                  toColor c t k = replace k (Text.concat [pack ("<span style='color:" ++ c ++ "'>"), k, pack "</span>"]) t
                  addColor c k = "<span style='color:" ++ c ++ "'>" ++ k ++ "</span>"
                  pre l = Text.concat [pack "<pre>", l, pack "</pre>"]
                  types = map pack ["real", "integer", "character", "type", "logical"]

                  html = let ?variant = HTMLPP
                         in
                           (Text.append (pack $ "<head><script type='text/javascript' src='../source.js'></script>"
                                             ++ "<link href='../source.css' type='text/css' rel='stylesheet' /></head>"))
                         . (\t -> replace (pack "newline") (pack "\n") t)
                         . (Text.concat . (map pre) . Text.lines)
                         . (\t -> foldl (toColor green) t types)
                         . (\t -> foldl (toColor purple) t keyword)
                         . (pack . printMaster)
                         -- . (pack . output)
                         -- . (pack . paraBi (\p -> \ss -> (showPara p) ++ ss) "")
                         -- . (pack . (para (\p -> \ss -> showPara p ++ (Prelude.concat ss))))
                         . (transformBi t) $ prog

{- | Pretty printer for HTML, specialised to the analysis of CamFort, which mostly uses the default master
     behaviour, but with a few special cases -}

instance PrintSlave Bool HTMLPP where
    printSlave = show

instance PrintSlave SrcLoc HTMLPP where
    printSlave _ = "" -- not sure if I want this to shown

instance (PrintSlave (Decl p) HTMLPP, PrintIndSlave (Fortran p) HTMLPP, PrintSlave p HTMLPP, Indentor (Decl p), Indentor (Fortran p)) => PrintSlave (ProgUnit p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (DataForm p) HTMLPP where
    printSlave = printMaster

instance (PrintSlave (DataForm p) HTMLPP) => PrintSlave (SubName p) HTMLPP where
    printSlave = printMaster

instance (PrintSlave (Decl p) HTMLPP) => PrintSlave (Implicit p) HTMLPP where
    printSlave = printMaster

instance {-# OVERLAPPABLE #-} (Indentor (Decl p), PrintSlave (DataForm p) HTMLPP) => PrintSlave (Decl p) HTMLPP where
    printSlave = printMaster

instance {-# OVERLAPS #-} PrintSlave (Decl Annotation) HTMLPP where
    printSlave t = let i = 0
                   in "<div style=''>" ++ (outputAnn (tag t) False i showt) ++  (annotationMark i t (printMaster t)) ++ "</div>"
                    where showt = prettyp (show (setCompactSrcLocs $ fmap (\x -> ()) t))


instance PrintSlave (Type p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (VarName p) HTMLPP where
    printSlave = printMaster

instance (PrintSlave (DataForm p) HTMLPP) => PrintSlave (Expr p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (UnaryOp p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (BinOp p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (ArgList p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (BaseType p) HTMLPP where
    printSlave = printMaster

instance (PrintSlave (Decl p) HTMLPP, Indentor (Decl p)) => PrintSlave (InterfaceSpec p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (Arg p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (ArgName p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (GSpec p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (Attr p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (Fraction p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (MeasureUnitSpec p) HTMLPP where
    printSlave = printMaster

instance (PrintSlave (Decl p) HTMLPP, PrintSlave (DataForm p) HTMLPP, PrintIndSlave (Fortran p) HTMLPP, PrintSlave p HTMLPP, Indentor (Fortran p), Indentor (Decl p)) => PrintSlave (Block p) HTMLPP where
    printSlave = printMaster

instance PrintSlave (Uses p) HTMLPP where
    printSlave u = showUse' u

showUse' :: Uses p -> String
showUse' (UseNil _) = ""
showUse' (Uses _ (Use n []) us _) = ("use "++n++"\n") ++ (showUse' us)
showUse' (Uses _ (Use n renames) us _) = ("use "++n++", " ++ (Prelude.concat $ Data.List.intersperse ", " (map (\(a, b) -> a ++ " => " ++ b) renames)) ++ "\n") ++ (showUse' us)

instance (PrintIndSlave (Fortran p) HTMLPP, PrintSlave p HTMLPP, Indentor (Fortran p)) => PrintSlave (Fortran p) HTMLPP where
    printSlave (For p _ v e e' e'' f) = "do"++" "++printSlave v++" = "++printSlave e++", "++
                                   printSlave e'++", "++printSlave e''++"\n"++
                                   "<span style='color:#707d8f'>"++"{"++printSlave p++"}</span>\n" ++
                                   (printIndSlave 1 f)++"\n"++(ind 1)++"end do"
    printSlave t = printMaster t

instance PrintSlave (Spec p) HTMLPP where
    printSlave = printMaster

instance Indentor (Fortran Bool) where
    indR t i = if (tag t) then
                   let (s, SrcLoc f l c) = srcSpan t
                   in Prelude.take c (repeat ' ')
               else ind i

instance PrintIndSlave (Fortran A1) HTMLPP where
    printIndSlave = printIndMaster

instance PrintIndSlave (Fortran Annotation) HTMLPP where

    printIndSlave i t@(For p _ v e e' e'' f) = (outputAnn p False i (show t)) ++
                                          annotationMark i t
                                          ((ind i) ++ "do"++" "++printSlave v++" = "++
                                           printSlave e++", "++
                                           printSlave e'++", "++printSlave e''++"\n"++
                                           (printIndSlave (i+1) f)++"\n"++(ind i)++"end do")


    -- printIndSlave i t@(FSeq p f1 f2) =  (outputAnn p False i) ++ printIndSlave i f1 ++ printIndSlave i f2
    printIndSlave i t = "<div style=''>" ++ (outputAnn (rextract t) False i showt) ++  (annotationMark i t (printIndMaster i t)) ++ "</div>"
                          where showt = prettyp (show (setCompactSrcLocs $ fmap (\x -> ()) t))

{-
instance PrintIndSlave (Decl p) HTMLPP where
    outputPrintSlave i t = "<div style=''>" ++ (outputAnn (rextract t) False i showt) ++  (annotationMark i t (printIndMaster i t)) ++ "</div>"
                        where showt = prettyp (show (setCompactSrcLocs $ fmap (\x -> ()) t))x
-}

countToColor n = colors !! (n `mod` (length colors)) --  printf "#%06x" ((256*256*256 - (n * 40)) :: Int)

colors = ["#ffeeee", "#eeffee", "#eeeeff", "#ffffee",
          "#eeffff", "#eeffee", "#ffdddd", "#ddffdd",
          "#ddddff", "#ffffdd", "#ffddff", "#ddffff",
          "#eecccc", "#cceecc", "#eeeecc", "#ddeeee"]

prettyp xs = prettyp' xs 0 []
prettyp' [] n f       = []
prettyp' ('(':xs) n f = let k = "<span style='background-color:" ++ (countToColor n) ++ ";'>"
                 in  if (nearbyClose xs 10) then
                         k ++ ('(':(prettyp' xs n (False:f)))
                     else
                         ("<br>" ++ (concat $ replicate (2 * (n+1)) "&nbsp;")) ++ k ++ ('(' : (prettyp' xs (n+1) (True:f)))
prettyp' (')':xs) n (False:f) = ')' : ("</span>" ++ prettyp' xs n f)
prettyp' (')':xs) n (True:f)  = ')' : ("</span>" ++ prettyp' xs (n - 1) f)
prettyp' (x:xs) n f = x : prettyp' xs n f

nearbyClose []       n = False
nearbyClose _        0 = False
nearbyClose ('(':(')':xs)) n = nearbyClose xs (n - 2)
nearbyClose (')':xs) n = True
nearbyClose (x:xs)   n = nearbyClose xs (n - 1)


annotationMark i t x = "<div class='clickable' onClick='toggle(" ++
                       (show $ number (tag t)) ++ ");'>" ++
                       x ++ "</div>"


row xs = "<tr>" ++ (concatMap (\x -> "<td>" ++ x ++ "</td>") xs) ++ "</tr>"

instance PrintSlave Annotation HTMLPP where
    printSlave t = outputAnn t False 0 (show t)

breakUp xs = breakup' xs 0 False
              where breakup' [] _ _ = []
                    breakup' (x:xs) c mode | x == '<' = x : (breakup' xs c True)
                                           | x == '>' = x : (breakup' xs c False)
                                           | c >= 80 && (not mode) = x : ("newline" ++ breakup' xs 0 False)
                                           | mode                  = x : (breakup' xs c mode)
                                           | otherwise             = x : (breakup' xs (c+1) mode)

 --  (take 80 xs) ++ "newline" ++ (if (drop 80 xs) == [] then [] else breakUp (drop 80 xs))

outputAnn t visible i astString =
     "<div id='a" ++ (show $ number t) ++ "' style='" ++
     (if visible then "" else "display:none;") ++
     "' class'outer'><div class='spacer'><pre>" ++ (indent 3 i) ++ "</pre></div>" ++
     "<div class='annotation'><div class='number'>" ++ (show $ number t) ++ "</div>" ++
     "<div><div class='clickable' onClick=\"toggle('" ++ (show $ number t) ++  "src');\">" ++
     "<u>show ast</u></div><div id='a" ++ (show $ number t) ++ "src' " ++
     "style='background:#fff;display:none;width:600px;overflow:wrap;'>" ++ (astString) ++ "</div></div>" ++ "<p><table>" ++
     row ["lives: (in) ",    showList $ (map show) $ fst $ lives t, "(out)", showList $ (map show) $ snd $ lives t] ++
     row ["successors:", showList $ (map show) (successorStmts t)] ++
     "</table></p></div><br />\n\r\n"
         where
           listToPair x       = "(" ++ listToPair' x ++ ")"
           listToPair' []     = ""
           listToPair' [x]    = printMaster x
           listToPair' (x:xs) = printMaster x ++ ", " ++ listToPair' xs

           showExps []           = ""
           showExps [(v, es)]    = "[" ++ v ++ ": " ++ (showList $ map listToPair es) ++ "]"
           showExps ((v, es):ys) = (showExps [(v, es)]) ++ ", " ++ (showExps ys)


           showList []  = ""
           showList [x] = x
           showList (x:xs) = x ++ ", " ++ showList xs


type A1 =  Bool


-- inBounds :: SrcLoc -> (SrcLoc, SrcLoc) -> Bool
-- inBounds x (l,u) = (lineCol x) >= (lineCol l) && (lineCol x) < (lineCol u)


{- Indenting for refactored code -}

instance Tagged p => Indentor (p Annotation) where
    indR t i = case (refactored . tag $ t) of
                 Just (SrcLoc f _ c) -> Prelude.take c (repeat ' ')
                 Nothing             -> ind i

keyword = map pack
          ["end","subroutine","function","program","module","data", "common",
           "namelist", "external", "interface", "type", "include", "format",
           "len", "kind", "dimension", "allocatable", "parameter", "external",
           "intent", "intrinsic", "optional", "pointer", "save", "target",
           "volatile", "public", "private", "sequence", "operator", "assignment",
           "procedure", "do", "if", "else", "then", "allocate", "backspace",
           "call", "open", "close", "continue", "cycle", "deallocate", "endfile",
           "exit", "forall", "goto", "nullify", "inquire", "rewind", "stop", "where",
           "write", "rerun", "print", "read", "write", "implicit", "use"]

