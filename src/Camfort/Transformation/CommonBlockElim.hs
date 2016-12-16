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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Camfort.Transformation.CommonBlockElim where

import Control.Monad
import Control.Monad.State.Lazy

import Debug.Trace
import Data.Data
import Data.List
import Data.Ord
import qualified Data.Map as M
import Data.Generics.Uniplate.Operations

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Util.Position as FU
import qualified Language.Fortran.ParserMonad as PM
import qualified Language.Fortran.PrettyPrint as PP

import Camfort.Helpers
import Camfort.Helpers.Syntax
import Camfort.Analysis.Annotations

-- Typed common-block representation
-- Tuple of:
--     * a (possible) common block name
--     * map from names to their types
type TCommon p = (Maybe F.Name, [(F.Name, F.BaseType)])

-- Typed and "located" common block representation
-- Right associated pairs tuple of:
--     * current filename
--     * current program unit name
--     * Typed common-block representation
-- TODO: include column + line information
type TLCommon p = (Filename, (F.Name, TCommon p))

type A1 = FA.Analysis Annotation
type CommonState = State (Report, [TLCommon A])

-- Top-level functions for eliminating common blocks in a set of files
commonElimToModules ::
       Directory
    -> [(Filename, F.ProgramFile A)]
    -> (Report, [(Filename, F.ProgramFile A)], [(Filename, F.ProgramFile A)])

-- Eliminates common blocks in a program directory (and convert to modules)
commonElimToModules d pfs =
    (r ++ r', pfs'', pfM)
  where
    (pfs', (r, cg)) = runState (analyseAndRmCommons pfs) ("", [])
    meta = F.MetaInfo PM.Fortran90 ""
    (r', pfM) = introduceModules meta d cg
    pfs'' = updateUseDecls pfs' cg

analyseAndRmCommons :: [(Filename, F.ProgramFile A)]
               -> CommonState [(Filename, F.ProgramFile A)]
analyseAndRmCommons = mapM analysePerPF

analysePerPF ::
   (Filename, F.ProgramFile A) -> CommonState (Filename, F.ProgramFile A)
analysePerPF (fname, pf) = do
   let pf' = FA.initAnalysis pf
   let (pf'', tenv) = FAT.analyseTypes pf'
   pf''' <- transformBiM (analysePerPU tenv fname) pf''
   return (fname, fmap FA.prevAnnotation pf''')

analysePerPU ::
    FAT.TypeEnv -> Filename -> F.ProgramUnit A1 -> CommonState (F.ProgramUnit A1)
analysePerPU tenv fname p =
    transformBiM (collectAndRmCommons tenv fname (F.getName p)) p

collectAndRmCommons :: FAT.TypeEnv -> Filename -> F.ProgramUnitName
               -> F.Block A1 -> CommonState (F.Block A1)
collectAndRmCommons tenv fname pname = transformBiM commons
  where
    commons :: F.Statement A1 -> CommonState (F.Statement A1)
    commons f@(F.StCommon a s@(FU.SrcSpan p1 _) cgrps) = do
        mapM_ commonGroups (F.aStrip cgrps)
        let a' = onPrev (\ap -> ap {refactored = Just p1, deleteNode = True}) a
        return $ F.StCommon a' (deleteLine s) (F.AList a s [])
    commons f = return f

    punitName (F.Named s) = s
    punitName _ = ""

    -- Process a common group, adding blocks to the common state
    commonGroups :: F.CommonGroup A1 -> CommonState ()
    commonGroups (F.CommonGroup a (FU.SrcSpan p1 _) cname exprs) = do
      let r' = show p1 ++ ": removed common declaration\n"
      let tcommon = map typeCommonExprs (F.aStrip exprs)
      let info = (fname, (punitName pname, (commonNameFromAST cname, tcommon)))
      modify (\(r, infos) -> (r ++ r', info : infos))

    typeCommonExprs :: F.Expression A1 -> (F.Name, F.BaseType)
    typeCommonExprs (F.ExpValue _ sp (F.ValVariable v)) =
      case M.lookup v tenv of
        Just (FA.IDType (Just t) (Just FA.CTVariable)) -> (v, t)
        Just (FA.IDType (Just t) (Just FA.CTArray))    -> (v, t)
        _ -> error $ "Variable '" ++ show v
                  ++ "' is of an unknown or higher-order type at: " ++ show sp
                  ++ show (M.lookup v tenv)

    typeCommonExprs e = error $ "Not expecting a non-variable expression \
                                \in expression at: " ++ show (FU.getSpan e)


{- Comparison functions for common block names and variables -}
cmpTLConFName :: TLCommon a -> TLCommon a -> Ordering
cmpTLConFName (f1, (_, _)) (f2, (_, _)) = compare f1 f2

cmpTLConPName :: TLCommon a -> TLCommon a -> Ordering
cmpTLConPName (_, (p1, _)) (_, (p2, _)) = compare p1 p2

cmpTLConBNames :: TLCommon a -> TLCommon a -> Ordering
cmpTLConBNames (_, (_, c1)) (_, (_, c2)) = cmpTConBNames c1 c2

cmpTConBNames :: TCommon a -> TCommon a -> Ordering
cmpTConBNames (Nothing, _) (Nothing, _) = EQ
cmpTConBNames (Nothing, _) (Just _, _)  = LT
cmpTConBNames (Just _, _) (Nothing, _)  = GT
cmpTConBNames (Just n, _) (Just n', _)
    | n < n' = LT
    | n > n' = GT
    | otherwise = EQ

cmpVarName :: TLCommon a -> TLCommon a -> Ordering
cmpVarName (_, (_, (_, vtys1))) (_, (_, (_, vtys2))) =
  map fst vtys1 `compare` map fst vtys2

-- Fold [TLCommon p] to get a list of ([(TLCommon p, Renamer p)],
-- [(Filename, F.ProgramFile A)]) How to decide which gets to be the
-- "head" perhaps the one which triggers the *least* renaming (ooh!)
-- (this is calculated by looking for the mode of the TLCommon (for a
-- particular Common) (need to do gorouping, but sortBy is used
-- already so... (IS THIS STABLE- does this matter?))

commonName Nothing  = "Common"
commonName (Just x) = x

commonNameFromAST (Just (F.ExpValue _ _ (F.ValVariable v))) = Just v
commonNameFromAST _ = Nothing

-- Freshen the names for a common block and generate a renamer from
-- the old block to this
freshenCommonNames :: TLCommon A -> (TLCommon A, RenamerCoercer)
freshenCommonNames (fname, (pname, (cname, fields))) =
        let mkRenamerAndCommon (r, tc) (v, t) =
                           let v' = caml (commonName cname) ++ "_" ++ v
                           in (M.insert v (Just v', Nothing) r, (v', t) : tc)
            (r, fields') = foldl mkRenamerAndCommon (M.empty, []) fields
        in ((fname, (pname, (cname, fields'))), Just r)

-- From a list of typed and located common blocks group by the common
-- block name, and then group/sort within such that the "mode" block
-- is first
groupSortCommonBlock :: [TLCommon A] -> [[[TLCommon A]]]
groupSortCommonBlock commons = gccs
  where
    -- Group by names of the common blocks
    gcs = groupBy (\x y -> cmpEq $ cmpTLConBNames x y) commons
    -- Group within by the different common block variable-type fields
    gccs = map (sortBy (\y x -> length x `compare` length y) . group . sortBy cmpVarName) gcs

mkTLCommonRenamers :: [TLCommon A] -> [(TLCommon A, RenamerCoercer)]
mkTLCommonRenamers commons =
    case allCoherentCommons commons of
      (r, False) -> error $ "Common blocks are incoherent!\n" ++ r
      (_, True) -> commons'
  where
    gccs = groupSortCommonBlock commons
    -- Find the "mode" common block and freshen the names for
    -- this, creating a renamer between this and every module
    gcrcs = map (\grp -> -- grp are block decls all for the same block
             let (com, r) = freshenCommonNames (head (head grp))
             in  map (\c -> (c, r)) (head grp) ++
                  map (\c -> (c, mkRenamerCoercerTLC c com)) (concat $ tail grp)) gccs
    -- Now re-sort based on the file and program unit
    commons' = sortBy (cmpFst cmpTLConFName) (sortBy (cmpFst cmpTLConPName) (concat gcrcs))

type NameMap = M.Map F.Name F.Name

-- Nothing represents an overall identity renamer/coercer for efficiency
-- a Nothing for a variable represent a variable-level (renamer) identity
-- a Nothing for a type represents a type-level (coercer) identity
type RenamerCoercer =
    Maybe (M.Map F.Name (Maybe F.Name, Maybe (F.BaseType, F.BaseType)))

applyRenaming :: (Typeable (t A), Data (t A)) => NameMap -> t A -> t A
applyRenaming r = transformBi rename
  where
    rename :: F.Value A -> F.Value A
    rename vn@(F.ValVariable v) =
        case M.lookup v r of
           Nothing -> vn
           Just v' -> F.ValVariable v'

class Renaming r where
    hasRenaming :: F.Name -> r -> Bool

instance Renaming RenamerCoercer where
    hasRenaming _ Nothing   = False
    hasRenaming v (Just rc) = M.member v rc

-- sometimes we have a number of renamer coercers together
instance Renaming [RenamerCoercer] where
    hasRenaming v = any (hasRenaming v)

updateUseDecls ::
  [(Filename, F.ProgramFile A)] -> [TLCommon A] -> [(Filename, F.ProgramFile A)]
updateUseDecls fps tcs = map perPF fps
  where
    perPF (f, p@(F.ProgramFile (F.MetaInfo v _) _ _)) =
      (f, transformBi (importIncludeCommons v) $ transformBi (matchPUnit v f) p)
    tcrs = mkTLCommonRenamers tcs

    inames :: F.Statement A -> Maybe String
    inames (F.StInclude _ _ (F.ExpValue _ _ (F.ValString fname))) = Just fname
    inames _ = Nothing

    importIncludeCommons :: PM.FortranVersion -> F.ProgramUnit A -> F.ProgramUnit A
    importIncludeCommons v p =
        foldl (flip (matchPUnit v)) p (reduceCollect inames p)

    insertUses :: [F.Block A] -> F.ProgramUnit A -> F.ProgramUnit A
    insertUses uses = descendBi insertUses'
      where insertUses' :: [F.Block A] -> [F.Block A]
            insertUses' bs = uses ++ bs

    matchPUnit :: PM.FortranVersion -> Filename -> F.ProgramUnit A -> F.ProgramUnit A
    matchPUnit v fname p =
        removeDecls v (map snd tcrs') p'
      where
        pname = case F.getName p of
                  F.Named pname -> pname
                   -- If no subname is available, use the filename
                  _             -> fname
        tcrs' = lookups' pname (lookups' fname tcrs)
        pos = getUnitStartPosition p
        uses = mkUseStatementBlocks pos tcrs'
        p' = insertUses uses p

    -- Given the list of renamed/coercerd variables form common blocks,
    -- remove any declaration sites
    removeDecls :: PM.FortranVersion -> [RenamerCoercer] -> F.ProgramUnit A -> F.ProgramUnit A
    removeDecls v rcs p = addToProgramUnit v p' remainingAssignments
        where
     (p', remainingAssignments) = runState (transformBiM (removeDecl rcs) p) []

    -- Removes a declaration and collects a list of any default values given at
    -- declaration time (which then need to be turned into separate assignment
    -- statements)
    removeDecl :: [RenamerCoercer]
               -> F.Statement A -> State [F.Statement A] (F.Statement A)
    removeDecl rcs d@(F.StDeclaration a s@(FU.SrcSpan p1 _) typ attr decls) = do
        modify (++ assgns)
        return $ F.StDeclaration a' (deleteLine s) typ attr decls'
      where
        (F.AList al sl declsA) = decls
        decls' = F.AList al' sl declsA'
        (assgns, declsA') = foldl matchVar ([],[]) declsA
        -- Update annotation if declarations are being added
        (a', al') = if length declsA == length declsA'
                     then (a, al)
                     else (a {refactored = Just p1, deleteNode = True}
                         , al {refactored = Just pl1})
                       where (FU.SrcSpan pl1 _ ) = sl

        matchVar :: ([F.Statement A], [F.Declarator A]) -> F.Declarator A
                 -> ([F.Statement A], [F.Declarator A])
        matchVar (assgns, decls)
                     dec@(F.DeclVariable a s
                    lvar@(F.ExpValue _ _ (F.ValVariable v)) len init) =
           if hasRenaming v rcs
           then case init of
                   -- Renaming exists and no default, then remove
                   Nothing -> (assgns, decls)
                   -- Renaming exists but has default, so create an
                   -- assignment for this
                   Just initExpr ->
                     ((F.StExpressionAssign a' s lvar initExpr) : assgns, decls)
            else -- no renaming, preserve declaration
                 (assgns, dec : decls)
        matchVar (assgns, decls) _ = (assgns, decls)
    removeDecl _ d = return d


-- Adds additional statements to the start of the statement block in a program unit
addToProgramUnit ::
   PM.FortranVersion -> F.ProgramUnit A -> [F.Statement A] -> F.ProgramUnit A
addToProgramUnit v pu stmnts = descendBi (addAfterDecls (map toBlock stmnts)) pu
  where
    -- Find the point where blocks are non-executable statements
    -- and become executable statements/blocks
    addAfterDecls :: [F.Block A] -> [F.Block A] -> [F.Block A]
    addAfterDecls []          ys = ys
    addAfterDecls [x]         ys = x : ys
    addAfterDecls (x:(x':xs)) ys
      | F.nonExecutableStatementBlock v x && F.executableStatementBlock v x'
                                 = x : (ys ++ (x' : xs))
      | F.executableStatementBlock v x = ys ++ (x:(x':xs))

    addAfterDecls (x:xs) ys      = x : addAfterDecls xs ys

    -- Convert a statement to a simple 'Statement' block
    toBlock :: F.Statement A -> F.Block A
    toBlock stmnt =
      F.BlStatement (F.getAnnotation stmnt) (FU.getSpan stmnt) Nothing stmnt

getUnitStartPosition :: F.ProgramUnit A -> FU.SrcSpan
getUnitStartPosition (F.PUMain _ s _ [] _) = s
getUnitStartPosition (F.PUMain _ _ _ bs _) = FU.getSpan (head bs)
getUnitStartPosition (F.PUSubroutine _ s _ _ _ [] _) = s
getUnitStartPosition (F.PUSubroutine _ _ _ _ _ bs _) = FU.getSpan (head bs)
getUnitStartPosition (F.PUFunction _ s _ _ _ _ _ [] _) = s
getUnitStartPosition (F.PUFunction _ _ _ _ _ _ _ bs _) = FU.getSpan (head bs)
getUnitStartPosition (F.PUBlockData _ s _ []) = s
getUnitStartPosition (F.PUBlockData _ _ _ bs) = FU.getSpan (head bs)

renamerToUse :: RenamerCoercer -> [(F.Name, F.Name)]
renamerToUse Nothing = []
renamerToUse (Just m) = let entryToPair v (Nothing, _) = []
                            entryToPair v (Just v', _) = [(v, v')]
                        in M.foldlWithKey (\xs v e -> entryToPair v e ++ xs) [] m

-- make the use statements for a particular program unit's common blocks
mkUseStatementBlocks :: FU.SrcSpan -> [(TCommon A, RenamerCoercer)] -> [F.Block A]
mkUseStatementBlocks s = map mkUseStmnt
  where
    a = unitAnnotation { refactored = Just pos, newNode = True }
    (FU.SrcSpan pos pos') = s
    s' = FU.SrcSpan (toCol0 pos) pos'
    mkUseStmnt x@((name, _), r) = F.BlStatement a s' Nothing $
       F.StUse a s' useName F.Permissive useListA
     where useName = F.ExpValue a s' (F.ValVariable (caml (commonName name)))
           useListA = case useList of [] -> Nothing
                                      us -> Just (F.AList a s' (reverse us))
           useList = mkUses pos x

    mkUses :: FU.Position -> (TCommon A, RenamerCoercer) -> [F.Use A]
    mkUses s ((name, _), r) = map useRenamer (renamerToUse r)

    useRenamer (v, vR) = F.UseRename a s' (F.ExpValue a s' (F.ValVariable v))
                                          (F.ExpValue a s' (F.ValVariable vR))

mkRenamerCoercerTLC :: TLCommon A :? source -> TLCommon A :? target -> RenamerCoercer
mkRenamerCoercerTLC x@(fname, (pname, common1)) (_, (_, common2)) =
    mkRenamerCoercer common1 common2

mkRenamerCoercer :: TCommon A :? source -> TCommon A :? target -> RenamerCoercer
mkRenamerCoercer (name1, vtys1) (name2, vtys2)
  | name1 == name2 =
     if vtys1 == vtys2 then Nothing
                         else Just $ generate vtys1 vtys2 M.empty
  | otherwise      =
        error "Can't generate renamer between different common blocks\n"
      where
        generate [] [] theta = theta
        generate ((var1, ty1):vtys1) ((var2, ty2):vtys2) theta =
            generate vtys1 vtys2 (M.insert var1 (varR, typR) theta)
          where
             varR = if var1 == var2 then Nothing else Just var2
             typR = if ty1  ==  ty2 then Nothing else Just (ty1, ty2)
        generate _ _ _ = error "Common blocks of different field length\n"

allCoherentCommons :: [TLCommon A] -> (Report, Bool)
allCoherentCommons commons =
   foldM (\p (c1, c2) -> coherentCommons c1 c2 >>= \p' -> return $ p && p')
     True (pairs commons)

coherentCommons :: TLCommon A -> TLCommon A -> (Report, Bool)
coherentCommons (f1, (p1, (n1, vtys1))) (f2, (p2, (n2, vtys2))) =
    if n1 == n2
    then coherentCommons' vtys1 vtys2
    else error $ "Trying to compare differently named common blocks: "
               ++ show n1 ++ " and " ++ show n2 ++ "\n"

coherentCommons' ::  [(F.Name, F.BaseType)] -> [(F.Name, F.BaseType)] -> (Report, Bool)
coherentCommons' []               []                = ("", True)
coherentCommons' ((var1, ty1):xs) ((var2, ty2):ys)
      | af ty1 == af ty2 = let (r', c) = coherentCommons' xs ys
                                           in (r', c && True)
      | otherwise = let r = var1 ++ ":"
                          ++ PP.pprintAndRender PM.Fortran90 ty1 Nothing
                          ++ "(" ++ show (af ty1) ++ ")"
                          ++ " differs from " ++ var2
                          ++ ":" ++ PP.pprintAndRender PM.Fortran90 ty2 Nothing
                          ++ "(" ++ show (af ty2) ++ ")" ++ "\n"
                        (r', _) = coherentCommons' xs ys
                    in (r ++ r', False)
    -- TODO - give more information in the error
coherentCommons' _ _ = ("Common blocks of different field lengths", False)

introduceModules ::
    F.MetaInfo -> Directory -> [TLCommon A]
                            -> (Report, [(Filename, F.ProgramFile A)])
introduceModules meta dir cenv =
    mapM (mkModuleFile meta dir . head . head) (groupSortCommonBlock cenv)

mkModuleFile ::
  F.MetaInfo -> Directory -> TLCommon A -> (Report, (Filename, F.ProgramFile A))
mkModuleFile meta dir (_, (_, (name, varTys))) =
    (r, (path, F.pfSetFilename path $ F.ProgramFile meta [([], mod)] []))
  where
    modname = commonName name
    path = dir ++ modname ++ ".f90"
    r = "Creating module " ++ modname ++ " at " ++ path ++ "\n"
    mod = mkModule modname varTys modname

mkModule :: String -> [(F.Name, F.BaseType)] -> String -> F.ProgramUnit A
mkModule name vtys fname =
    F.PUModule a sp (caml fname) decls Nothing
  where
    a = unitAnnotation { refactored = Just loc, newNode = True }
    loc = FU.Position 0 0 0
    sp = FU.SrcSpan loc loc
    toDeclBlock (v, t) = F.BlStatement a sp Nothing (toStmt (v, t))
    toStmt (v, t) = F.StDeclaration a sp (toTypeSpec t) Nothing (toDeclarator v)
    toTypeSpec t = F.TypeSpec a sp t Nothing
    toDeclarator v = F.AList a sp
       [F.DeclVariable a sp
          (F.ExpValue a sp (F.ValVariable (caml name ++ "_" ++ v))) Nothing Nothing]
    decls = map toDeclBlock vtys
