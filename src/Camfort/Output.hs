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

 Provides support for outputting source files and analysis information

-}

module Camfort.Output where

import Camfort.Helpers
import Camfort.Traverse

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Parser as Fortran
import Language.Fortran.PreProcess
import Language.Fortran

import Camfort.Analysis.Annotations
import Camfort.Analysis.Syntax
import Language.Fortran as Fortran
import Language.Fortran.Pretty
import Camfort.Transformation.Syntax

import System.FilePath
import System.Directory

-- FIXME: Did enough to get this module to compile, it's not optimised to use ByteString.
import qualified Data.ByteString.Char8 as B
import Data.Text hiding (zip,foldl,map,concatMap,take,drop,length,last,head,tail,replicate,concat)
import qualified Data.Text as Text
import Data.Map.Lazy hiding (map, foldl)
import Data.Functor.Identity
import Data.Generics
import GHC.Generics
import Data.List hiding (zip)
import Data.Generics.Uniplate.Data
import Generics.Deriving.Copoint
import Data.Char
import Data.Generics.Zipper
import Data.Maybe
import Debug.Trace
import Control.Monad.Trans.State.Lazy
import Text.Printf

import Camfort.Specification.Units.Environment

-- Custom 'Show' which on strings is the identity
class Show' s where
      show' :: s -> String
instance {-# OVERLAPS #-} Show' String where
      show' = id
instance {-# OVERLAPS #-} (Show' a, Show' b) => Show' (a, b) where
      show' (a, b) = "(" ++ show' a ++ "," ++ show' b ++")"
instance {-# OVERLAPPABLE #-} (Show a) => Show' a where
      show' = show

class OutputFiles t where
   {-| Given a directory and list of triples of filenames, with their source
       text (if it exists) and their AST, write these to the directory -}
  mkOutputText :: FileOrDir -> t -> SourceText
  outputFile   :: t -> Filename

  outputFiles :: FileOrDir -> FileOrDir -> [t] -> IO ()
  outputFiles inp outp pdata = do
      outIsDir <- isDirectory outp
      inIsDir  <- isDirectory inp
      inIsFile <- doesFileExist inp
      if outIsDir then do
          createDirectoryIfMissing True outp
          putStrLn $ "Writing refactored files to directory: " ++ outp ++ "/"
          isdir <- isDirectory inp
          let inSrc = if isdir then inp else getDir inp
          mapM_ (\x -> let f' = changeDir outp inSrc (outputFile x)
                       in do checkDir f'
                             putStrLn $ "Writing " ++ f'
                             B.writeFile f' (mkOutputText outp x)) pdata
       else
         if inIsDir || length pdata > 1
         then  error $ "Error: attempting to output multiple files, but the \
                         \given output destination is a single file. \n\
                         \Please specify an output directory"
         else
           if inIsFile -- Input was just a file, then output just a file
           then do
             putStrLn $ "Writing refactored file to: " ++ outp
             putStrLn $ "Writing " ++ outp
             B.writeFile outp (mkOutputText outp (head pdata))

            else let outSrc = getDir outp
               in do createDirectoryIfMissing True outSrc
                     putStrLn $ "Writing refactored file to: " ++ outp
                     putStrLn $ "Writing " ++ outp
                     B.writeFile outp (mkOutputText outp (head pdata))

-- When the new source text is already provided
instance OutputFiles (Filename, SourceText) where
  mkOutputText _ (_, output) = output
  outputFile (f, _) = f

data PR a = PR (Program a) deriving Data

-- When there is a file to be reprinted (for refactoring)
instance OutputFiles (Filename, SourceText, Program Annotation) where
  mkOutputText f' (f, input, ast') = B.pack $ reprint input f' (PR ast')
    where
  outputFile (f, _, _) = f

-- When there is a file to be reprinted (for refactoring)
instance OutputFiles (Filename, SourceText, F.ProgramFile Annotation) where
  mkOutputText f' (f, input, ast') = B.pack $ reprint input f' ast'
  outputFile (f, _, _) = f


{-| changeDir is used to change the directory of a filename string.
    If the filename string has no directory then this is an identity  -}
changeDir newDir oldDir oldFilename = newDir ++ (listDiffL oldDir oldFilename)
                                      where listDiffL []     ys = ys
                                            listDiffL xs     [] = []
                                            listDiffL (x:xs) (y:ys) | x==y      = listDiffL xs ys
                                                                    | otherwise = ys

{-| output pre-analysis ASTs into the directory with the given file names (the list of ASTs should match the
    list of filenames) -}
outputAnalysisFiles :: FileOrDir -> [Program Annotation] -> [Filename] -> IO ()
outputAnalysisFiles src asts files = do
  isdir <- isDirectory src
  let src' = if isdir then src else dropFileName src
  putStrLn $ "Writing analysis files to directory: " ++ src'
  mapM (\(ast', f) -> writeFile (f ++ ".html") ((concatMap outputHTML) ast')) (zip asts files)
  return ()

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
     row ["indices:",  showList $ indices t] ++
     row ["successors:", showList $ (map show) (successorStmts t)] ++
     row ["arrays R:", showExps (assocs $ arrsRead t)] ++
     row ["arrays W:", showExps (assocs $ arrsWrite t)] ++
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


takeBounds (l, u) inp = takeBounds' (lineCol l, lineCol u) [] inp

takeBounds' ((ll, lc), (ul, uc)) tk inp  =
    if (ll == ul && lc == uc) || (ll > ul) then (Prelude.reverse tk, inp)
    else case inp of []             -> (Prelude.reverse tk, inp)
                     ([]:[])        -> (Prelude.reverse tk, inp)
                     ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
                     ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)

{- Indenting for refactored code -}

instance Tagged p => Indentor (p Annotation) where
    indR t i = case (refactored . tag $ t) of
                 Just (SrcLoc f _ c) -> Prelude.take c (repeat ' ')
                 Nothing             -> ind i


-- Start of GLORIOUS REFACTORING ALGORITHM!

-- FIXME: Use ByteString! (Or Data.Text, at least)

{-| -}
reprint :: (Data (p Annotation)) -- (PrintMaster (p Annotation) DefaultPP, 
        => SourceText -> Filename -> p Annotation -> String
reprint input f p =
  -- If the inupt is null then switch into pretty printer
  -- | B.null input = let ?variant = DefaultPP in printMaster p
  -- | otherwise = 
    pn ++ pe
  where input' = map B.unpack $ B.lines input
        len = Prelude.length input'
        start = SrcLoc f 1 0
        end = SrcLoc f len (1 + (Prelude.length $ Prelude.last input'))
        (pn, cursorn) = runIdentity $ evalStateT (reprintC start input' (toZipper p)) 0
        (_, inpn) = takeBounds (start, cursorn) input'
        (pe, _) = takeBounds (cursorn, end) inpn

reprintC :: Monad m => SrcLoc -> [String] -> Zipper a -> StateT Int m (String, SrcLoc)
reprintC cursor inp z = do
  (p1, cursor', flag) <- query (refactoring inp cursor) z

  (_, inp')       <- return $ takeBounds (cursor, cursor') inp
  (p2, cursor'')  <- if flag then return ("", cursor')
                             else enterDown cursor' inp' z

  (_, inp'')      <- return $ takeBounds (cursor', cursor'') inp'
  (p3, cursor''') <- enterRight cursor'' inp'' z

  return (p1 ++ p2 ++ p3, cursor''')

enterDown cursor inp z = case (down' z) of
                             Just dz -> reprintC cursor inp dz
                             Nothing -> return $ ("", cursor)

enterRight cursor inp z = case (right z) of
                             Just rz -> reprintC cursor inp rz
                             Nothing -> return $ ("", cursor)

-- End of GLORIOUS REFACTORING ALGORITHM


{- Specifies how to do specific refactorings
  (uses generic query extension - remember extQ is non-symmetric)
-}

refactoring :: (Typeable a, Monad m) => [String] -> SrcLoc -> a -> StateT Int m (String, SrcLoc, Bool)
refactoring inp cursor = ((((\_ -> return ("", cursor, False))
                              `extQ` (refactorUses inp cursor))
                                 `extQ` (refactorDecl inp cursor))
                                    `extQ` (refactorArgName inp cursor))
                                       `extQ` (refactorFortran inp cursor)


refactorFortran :: Monad m => [String] -> SrcLoc -> Fortran Annotation -> StateT Int m (String, SrcLoc, Bool)
refactorFortran inp cursor e =  return $
       if (pRefactored $ tag e) then
          let (lb, ub) = srcSpan e
              (p0, _) = takeBounds (cursor, lb) inp
              outE = pprint e
              lnl = case e of (NullStmt _ _) -> (if ((p0 /= []) && (Prelude.last p0 /= '\n')) then "\n" else "")
                              _              -> ""
              lnl2 = if ((p0 /= []) && (Prelude.last p0 /= '\n')) then "\n" else ""
              textOut = if p0 == "\n" then outE else (p0 ++ lnl2 ++ outE ++ lnl)
          in (textOut, ub, True)
       else ("", cursor, False)


refactorDecl :: Monad m => [String] -> SrcLoc -> Decl Annotation -> StateT Int m (String, SrcLoc, Bool)
refactorDecl inp cursor d =
    if (pRefactored $ tag d) then
       let (lb, ub) = srcSpan d
           (p0, _) = takeBounds (cursor, lb) inp
           textOut = p0 ++ (pprint d)
       in do textOut' <- -- The following compensates new lines with removed lines
                         case d of
                           (NullDecl _ _) ->
                              do added <- get
                                 let diff = linesCovered ub lb
                                 -- remove empty newlines here if extra lines have been added
                                 let (text, removed) = if added <= diff
                                                         then removeNewLines textOut added
                                                         else removeNewLines textOut diff
                                 put (added - removed)
                                 return text
                           otherwise -> return textOut
             return (textOut', ub, True)
    else return ("", cursor, False)

refactorArgName :: Monad m => [String] -> SrcLoc -> ArgName Annotation -> m (String, SrcLoc, Bool)
refactorArgName inp cursor a = return $
        case (refactored $ tag a) of
            Just lb -> let (p0, _) = takeBounds (cursor, lb) inp
                       in (p0 ++ pprint a, lb, True)
            Nothing -> ("", cursor, False)

refactorUses :: Monad m => [String] -> SrcLoc -> Uses Annotation -> StateT Int m (String, SrcLoc, Bool)
refactorUses inp cursor u =
    let ?variant = HTMLPP in
        case (refactored $ tag u) of
           Just lb -> let (p0, _) = takeBounds (cursor, lb) inp
                          syntax  = printSlave u
                       in do added <- get
                             if (newNode $ tag u) then put (added + (countLines syntax))
                                                  else return ()
                             return (p0 ++ syntax, toCol0 lb, True)
           Nothing -> return ("", cursor, False)

countLines []        = 0
countLines ('\n':xs) = 1 + countLines xs
countLines (x:xs)    = countLines xs

{- 'removeNewLines xs n' removes at most 'n' new lines characters from the input string
    xs, returning the new string and the number of new lines that were removed. Note
    that the number of new lines removed might actually be less than 'n'- but in principle
    this should not happen with the usaage in 'refactorDecl' -}

removeNewLines [] n = ([], 0)

removeNewLines xs 0 = (xs, 0)

-- Deal with CR LF in the same way as just LF
removeNewLines ('\r':('\n':('\r':('\n':xs)))) n = let (xs', n') = removeNewLines ('\r':'\n':xs) (n - 1)
                                                   in (xs', n' + 1)

removeNewLines ('\n':('\n':xs)) n = let (xs', n') = removeNewLines ('\n':xs) (n - 1)
                                     in (xs', n' + 1)
removeNewLines (x:xs) n = let (xs', n') = removeNewLines xs n
                          in (x:xs', n)

--removeNewLines ('\n':xs) 0 = let (xs', n') = removeNewLines xs 0
--                             in ('\n':xs', 0)
