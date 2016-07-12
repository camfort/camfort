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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Camfort.Specification.Units.SyntaxConversion ( convertSyntax
                                               , convertSyntaxBack, toUnitInfo ) where

import Prelude hiding (getLine)
import Data.List (isPrefixOf, isInfixOf, find, (\\), intercalate)
import Data.Char (isSpace, toLower)
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe (fromJust)
import Data.Ratio

import GHC.Exts (sortWith)

import qualified Language.Fortran as LF

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Units.Environment hiding (Unitless)
import Camfort.Specification.Units.Parser

import Debug.Trace

convertSyntax :: String -> String -> LF.Program () -> LF.Program ()
convertSyntax fn src ast =
  let (attrs, decls) = convert . cluster fn $ src
  in addAliasDecls decls (addUnitAttrs attrs ast)

convertSyntaxBack :: String -> LF.Program () -> String
convertSyntaxBack src ast = unlines (go info processedSource)
  where
    info = gatherUnitInformation ast
    processedSource = processSource src
    averageSpace = round $ (fromIntegral $ foldr ((+) . length . takeWhile isSpace) 0 processedSource) / (fromIntegral $ length processedSource)
    go [] zs = zs
    go  inf@((var,uom):xs) (z:zs)
      | isInfixOf var z = ((replicate averageSpace ' ')
                           ++ show' (UnitAssignment (Just var) uom)) : (go xs (z:zs))
      | otherwise = z : go inf zs
    go qs _ = error "Not all unit annotations are correctly declared."
    show' uom = "!" ++ show uom

-- Back to the source

processSource :: String -> [ String ]
processSource src =
    let ls = lines . map toLower $ src
    in map snd . filter p . zip (map parse $ ls) $ ls
  where
    parse s =
      case dropWhile isSpace s of
        '!':xs -> unitParser xs
        _ -> Left NotAnnotation
    p (Right UnitAssignment{}, _) = False
    p _ = True

gatherUnitInformation :: LF.Program () -> [ (String, UnitOfMeasure) ]
gatherUnitInformation ast =
    let decls = [ d | d@(LF.Decl () _ _ _) <- universeBi ast ]
    in foldr update' [ ] decls
  where
    update' decl mapping =
      let names = [ name | LF.VarName () name <- universeBi decl ]
          units = [ fromOldUnits u | LF.MeasureUnit () u <- universeBi decl ]
      in foldr (:) mapping [ (map toLower name, unit) | name <- names, unit <- units ]

-- Source to new units of measure

type TargetIndex = Int
type Line = Int

type SrcSpan = (LF.SrcLoc, LF.SrcLoc)

type ElaborateAttr = (LF.Attr (), Maybe String)

containsSpan :: (LF.Span a, LF.Span b) => a -> b -> Bool
containsSpan a b
 | (LF.SrcLoc _ l1 _, LF.SrcLoc _ l2 _)  <- LF.srcSpan a
 , (LF.SrcLoc _ l1' _, LF.SrcLoc _ l2' _) <- LF.srcSpan b
 = l1 <= l1' && l2' <= l2

addUnitAttrs :: [ ([ ElaborateAttr ], TargetIndex) ]
             -> LF.Program ()
             -> LF.Program ()
addUnitAttrs attrStrIs ast = foldr addUnitAttr ast attrStrIs
  where
    addUnitAttr (attrStrs, ti) ast'
      | null attrStrs = error "Attributes should not be empty."
      {-
      | length attrs' == 1 =
        transformBi (breakUp ti attrs') ast'
-}
      | Just names <- mapM snd attrStrs = transformBi (breakUp ti attrStrs) ast'
      | otherwise = error "Unit annotations needs to include names."
    breakUp ti attrStrs (b :: LF.Block ()) = descendBi (breakUp' ti attrStrs) b
    -- If there is a single declaration, then we have Decl inside Block
    breakUp' ti attrStrs d@(LF.Decl () s ls t)
      | getLine s == ti =
        let newDecls = genNewDecls attrStrs d
        in append newDecls (LF.NullDecl () s)
      | otherwise = d
    -- If there are multiple declarations we have DSeq inside Block
    breakUp' ti attrStrs d@(LF.DSeq () decl@(LF.Decl () s ls t) drest)
      | getLine s == ti =
        let newDecls = genNewDecls attrStrs decl
        in append newDecls drest
      | (LF.DSeq () decl drest) <- d = LF.DSeq () decl (breakUp' ti attrStrs drest)
    breakUp' _ _ x = x
    genNewDecls :: [ (LF.Attr (), Maybe String) ] -> LF.Decl () ->  [ LF.Decl () ]
    genNewDecls attrStrs (LF.Decl () s ls t) =
        let decls = map (\l -> LF.Decl () s [ l ] t) ls
            dNames = extractNames ls
            aNames = map (\s -> fromJust . snd $ s) attrStrs
            diff = aNames \\ dNames
        in if null diff
           then let (pairs, rest) = pair aNames decls attrStrs
                    newDecls = map (\(a,b) -> transformBi (trans (getLine s) b) a) pairs
                in newDecls ++ rest
           else error $ "Annotated variables " ++ (intercalate ", " diff) ++ " do not appear in the following declaration."
    pair intersection decls attrStrs =
      let pickedDecls = filter (\d -> extractName d `elem` intersection) decls
          sDecls = sortWith extractName pickedDecls
          sAttrs = map fst $ sortWith (fromJust . snd) attrStrs
      in (zip sDecls sAttrs, decls \\ pickedDecls)
    append [ ] drest = drest
    append (d:ds) drest = LF.DSeq () d (append ds drest)
    trans ti attr d@(LF.Decl () s@((LF.SrcLoc _ l _),_) ls t)
      | l == ti =
        LF.Decl () s ls $
          case t of
            LF.BaseType () a as b c ->
              LF.BaseType () a (attr : as) b c
            LF.ArrayT () z a as b c ->
              LF.ArrayT () z a (attr : as) b c
      | otherwise = d
    trans _ _ x = x


class Named a where
  extractNames :: a -> [ String ]
  extractName :: a -> String

extractName' :: (LF.Expr (), b, c) -> String
extractName' ((LF.Var _ _ [(LF.VarName _ name, _)]),_,_) = name
instance Named [ (LF.Expr (), b, c) ] where
  extractNames = map extractName'
  extractName = intercalate ", " . map extractName'

instance Named (LF.Decl ()) where
  extractNames (LF.Decl _ _ ls _) = extractNames ls
  extractName (LF.Decl _ _ [ l ] _) = extractName' l

addAliasDecls :: [ LF.Decl () ] -> LF.Program () -> LF.Program ()
addAliasDecls decls ast = foldr addAliasDecl ast decls
  where
    addAliasDecl decl ast' =
      let p = smallest [ p | (p :: LF.ProgUnit ()) <- universeBi ast', p `containsSpan` decl ]
      in transformBi (trans decl p) ast'
    trans decl p1 p2
      | p1 == p2 = insertDecl decl p2
      | otherwise = p2
    smallest ps =
      let m = minimum $ map (getLen . LF.srcSpan) ps
      in case find (\a -> (getLen . LF.srcSpan) a == m) ps of
           Just a -> a
           Nothing -> error "Couldn't find ProgUnit"
    getLen (LF.SrcLoc _ l1 _, LF.SrcLoc _ l2 _) = l2 - l1

insertDecl :: LF.Decl () -> LF.ProgUnit () -> LF.ProgUnit ()
insertDecl decl (LF.Main () a b c block d) =
  LF.Main () a b c (insertDecl' decl block) d
insertDecl decl (LF.Sub () a b c d block) =
  LF.Sub () a b c d (insertDecl' decl block)
insertDecl decl (LF.Function () a b c d e block) =
  LF.Function () a b c d e (insertDecl' decl block)
insertDecl decl (LF.Module () a b c d decls e) =
  LF.Module () a b c d (LF.DSeq () decl decls) e
insertDecl decl (LF.BlockData () a b c d decls) =
  LF.BlockData () a b c d (LF.DSeq () decl decls)

insertDecl' :: LF.Decl () -> LF.Block () -> LF.Block ()
insertDecl' decl (LF.Block () a b c decls d) =
  (LF.Block () a b c (LF.DSeq () decl decls) d)

getLine :: SrcSpan -> Line
getLine (LF.SrcLoc _ l _, _) = l

convert :: ( [ ([ UnitStatement ], TargetIndex) ]
           , [ (UnitStatement, SrcSpan) ] )
        -> ( [ ([ElaborateAttr], TargetIndex) ]
           , [ LF.Decl () ] )
convert (attrs, decls) = (map l attrs, map r decls)
  where
    l (uss, targetIndex) = (map l' uss, targetIndex)
    l' us =
      case fromNewUnitStatements us of
        Left attr -> attr
    r (us, s) =
      case fromNewUnitStatements us of
        Right declF -> declF s

cluster :: String
        -> String
        -> ( [ ([ UnitStatement ], TargetIndex) ]
           , [ (UnitStatement, SrcSpan) ] )
cluster f = filterCluster . cluster' . addIndex . parse f

filterCluster (annotations, aliases) =
  (filter (not . null . fst) annotations, aliases)

cluster' :: [ (Either AnnotationParseError UnitStatement, SrcSpan) ]
         -> ( [ ([ UnitStatement ], TargetIndex) ],
              [ (UnitStatement, SrcSpan) ])
cluster' [] = ([], [])
cluster' ((Left _,s):xs) =
  let (fs,sn) = cluster' xs
  in (([],getLine s):fs, sn)
cluster' ((Right x@UnitAssignment{}, s):xs) =
  case cluster' xs of
    ([], as) -> ([ ([ x ], (getLine s) + 1) ], as)
    ((h:t), as) -> ((x:fst h, snd h):t, as)
cluster' ((Right x@UnitAlias{}, s):xs) =
  case cluster' xs of
    (as, []) -> (([],getLine s):as, [ (x,s) ])
    (as, xs) -> (([],getLine s):as, (x,s):xs)

addIndex :: [ (Either AnnotationParseError UnitStatement, Line -> SrcSpan) ]
         -> [ (Either AnnotationParseError UnitStatement, SrcSpan) ]
addIndex xs = map f (zip xs [1..(length xs)])
  where
    f ((us,toSrcLoc),i) = (us, toSrcLoc i)

parse :: String
      -> String
      -> [ (Either AnnotationParseError UnitStatement, Line -> SrcSpan) ]
parse fn src = map (t 0) $ lines src
  where
    t col line
      | isPrefixOf " " line = t (col + 1) (tail line)
      | isPrefixOf "!" line =
        (unitParser $ tail line, genSrcLoc col (length line))
      | otherwise = (Left NotAnnotation, genSrcLoc 0 0)
    genSrcLoc c len l = (LF.SrcLoc fn l c, LF.SrcLoc fn c (c + len))

-- Convert parser units to UnitInfo

toUnitInfo :: UnitOfMeasure -> UnitInfo
toUnitInfo (UnitProduct u1 u2) =
    UnitMul (toUnitInfo u1) (toUnitInfo u2)
toUnitInfo (UnitQuotient u1 u2) =
    UnitMul (toUnitInfo u1) (UnitPow (toUnitInfo u2) (-1))
toUnitInfo (UnitExponentiation u1 p) =
    UnitPow (toUnitInfo u1) (toDouble p)
  where
    toDouble :: UnitPower -> Double
    toDouble (UnitPowerInteger i) = fromInteger i
    toDouble (UnitPowerRational x y) = fromRational (x % y)
toUnitInfo (UnitBasic str) =
    UnitName str
toUnitInfo (Unitless) =
    UnitlessI

-- Convert new units of measure to old units of measure
fromNewUnitStatements :: UnitStatement
                      -> (Either ElaborateAttr)
                                 (SrcSpan -> LF.Decl ())
fromNewUnitStatements (UnitAssignment ms uom) =
  Left (LF.MeasureUnit () (fromNewUnits uom), ms)
fromNewUnitStatements (UnitAlias s uom) =
  Right $ flip (LF.MeasureUnitDef ()) [ (s, fromNewUnits uom) ]

fromNewUnits :: UnitOfMeasure -> LF.MeasureUnitSpec ()
fromNewUnits Unitless = LF.UnitNone ()
fromNewUnits uom@(UnitBasic s) = LF.UnitProduct () (fromNewUnits' uom)
fromNewUnits (UnitProduct uom1 uom2) =
  LF.UnitProduct () $ fromNewUnits' uom1 ++ fromNewUnits' uom2
fromNewUnits (UnitQuotient uom1 uom2) =
  LF.UnitQuotient () (fromNewUnits' uom1) (fromNewUnits' uom2)

fromNewUnits' :: UnitOfMeasure -> [ (LF.MeasureUnit, LF.Fraction ()) ]
fromNewUnits' (UnitProduct uom1 uom2) =
  fromNewUnits' uom1 ++ fromNewUnits' uom2
fromNewUnits' (UnitBasic s) = [(s, LF.NullFraction ())]
fromNewUnits' (UnitExponentiation uom power) =
  case uom of
    UnitBasic s ->
      case power of
        UnitPowerInteger i -> [ (s, LF.IntegerConst () $ show i) ]
        UnitPowerRational i1 i2 -> [ (s, LF.FractionConst () (show i1) (show i2)) ]
    _ -> error "Only base units can be exponentiated."
fromNewUnits' (UnitQuotient _ _) =
  error "Unit should have only one division."
fromNewUnits' Unitless =
  error "Unit should not have unitless component inside."

-- Convert old units of measure to new units of measure
fromOldUnitAttribute :: String -> LF.Attr () -> Maybe UnitStatement
fromOldUnitAttribute s (LF.MeasureUnit () oldUnits) =
  Just $ UnitAssignment (Just s) (fromOldUnits oldUnits)
fromOldUnitAttribute _ _ = Nothing

fromOldUnitAlias :: LF.Decl () -> UnitStatement
fromOldUnitAlias (LF.MeasureUnitDef () _ [ (name, oldUnits) ] ) =
  UnitAlias name (fromOldUnits oldUnits)

fromOldUnits :: LF.MeasureUnitSpec () -> UnitOfMeasure
fromOldUnits (LF.UnitNone ()) = Unitless
fromOldUnits (LF.UnitProduct () [ (s, LF.NullFraction ()) ]) = UnitBasic s
fromOldUnits (LF.UnitProduct () xs) = fromOldUnits' xs
fromOldUnits (LF.UnitQuotient () l r) = UnitQuotient (fromOldUnits' l) (fromOldUnits' r)

fromOldUnits' :: [ (LF.MeasureUnit, LF.Fraction ()) ] -> UnitOfMeasure
fromOldUnits' [ x ] = fromOldUnits'' x
fromOldUnits' (x:xs) = UnitProduct (fromOldUnits'' x) (fromOldUnits' xs)

fromOldUnits'' :: (LF.MeasureUnit, LF.Fraction ()) -> UnitOfMeasure
fromOldUnits'' (s, LF.NullFraction ()) = UnitBasic s
fromOldUnits'' (s, f) = UnitExponentiation (UnitBasic s) $ fromOldUnits''' f

fromOldUnits''' :: LF.Fraction () -> UnitPower
fromOldUnits''' (LF.FractionConst () i1 i2) = UnitPowerRational (read i1) (read i2)
fromOldUnits''' (LF.IntegerConst () i) = UnitPowerInteger (read i)
