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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}

module Camfort.Transformation.CommonBlockElim
  ( commonElimToModules
  ) where

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Helpers
import           Camfort.Helpers.Syntax
import           Control.Monad hiding (ap)
import           Control.Monad.State.Lazy hiding (ap)
import           Control.Monad.Writer.Lazy (execWriter, tell)
import           Data.Data
import           Data.Function (on)
import           Data.Generics.Uniplate.Operations
import           Data.List hiding (init)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Void
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.SemanticTypes as FAS
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Version as F
import qualified Language.Fortran.PrettyPrint as PP
import qualified Language.Fortran.Util.Position as FU
import           Prelude hiding (mod, init)

-- Typed common-block representation
-- Tuple of:
--     * a (possible) common block name
--     * map from names to their types
type TypeInfo = (FAS.SemType, FA.ConstructType)
type TCommon p = (Maybe F.Name, [(F.Name, TypeInfo)])

-- Typed and "located" common block representation
-- Right associated pairs tuple of:
--     * current filename
--     * current program unit name
--     * Typed common-block representation
-- TODO: include column + line information
type TLCommon p = (Filename, (F.Name, TCommon p))

type A1 = FA.Analysis Annotation
type CommonState = State (String, [TLCommon A])

-- | Type for type-level annotations giving documentation
type (:?) a (b :: k) = a

-- Top-level functions for eliminating common blocks in a set of files
commonElimToModules ::
       Directory
    -> [F.ProgramFile A]
    -> PureAnalysis Void Void ([F.ProgramFile A], [F.ProgramFile A])

-- Eliminates common blocks in a program directory (and convert to modules)
commonElimToModules d pfs = do
  let (pfs', (r, cg)) = runState (analyseAndRmCommons pfs) ("", [])
      (r', pfM)       = introduceModules meta d cg
      pfs''           = updateUseDecls pfs' cg
  logDebug' pfs $ describe $ r ++ r'
  pure (pfs'', pfM)
  where
    meta = F.MetaInfo F.Fortran90 ""

analyseAndRmCommons :: [F.ProgramFile A]
               -> CommonState [F.ProgramFile A]
analyseAndRmCommons = mapM analysePerPF

analysePerPF :: F.ProgramFile A -> CommonState (F.ProgramFile A)
analysePerPF pf = do
   let pf' = FAR.analyseRenames . FA.initAnalysis $ pf
   let (pf'', tenv) = FAT.analyseTypes pf'
   pf''' <- transformBiM (analysePerPU tenv (F.pfGetFilename pf)) pf''
   return (fmap FA.prevAnnotation pf''')

analysePerPU ::
    FAT.TypeEnv -> Filename -> F.ProgramUnit A1 -> CommonState (F.ProgramUnit A1)
analysePerPU tenv fname p =
    transformBiM (collectAndRmCommons tenv fname (F.getName p)) p

collectAndRmCommons :: FAT.TypeEnv -> Filename -> F.ProgramUnitName
               -> F.Block A1 -> CommonState (F.Block A1)
collectAndRmCommons tenv fname pname = transformBiM commons
  where
    commons :: F.Statement A1 -> CommonState (F.Statement A1)
    commons (F.StCommon a s@(FU.SrcSpan p1 _) cgrps) = do
        mapM_ commonGroups (F.aStrip cgrps)
        let a' = onPrev (\ap -> ap {refactored = Just p1, deleteNode = True}) a
        return $ F.StCommon a' (deleteLine s) (F.AList a s [])
    commons f = return f

    punitName (F.Named s) = s
    punitName _ = ""

    -- Process a common group, adding blocks to the common state
    commonGroups :: F.CommonGroup A1 -> CommonState ()
    commonGroups (F.CommonGroup _ (FU.SrcSpan p1 _) cname exprs) = do
      let r' = show p1 ++ ": removed common declaration\n"
      let tcommon = map typeCommonExprs (F.aStrip exprs)
      let info = (fname, (punitName pname, (commonNameFromAST cname, tcommon)))
      modify (\(r, infos) -> (r ++ r', info : infos))

    typeCommonExprs :: F.Declarator A1 -> (F.Name, TypeInfo)
    typeCommonExprs (F.Declarator _ sp nameExpr _ _ _) =
        let var = FA.varName nameExpr
            src = FA.srcName nameExpr
         in case M.lookup var tenv of
              Just (FA.IDType (Just t) (Just ct@FA.CTVariable)) -> (src, (t, ct))
              Just (FA.IDType (Just t) (Just ct@FA.CTArray{}))  -> (src, (t, ct))
              _ -> error $ "Variable '" ++ src
                        ++ "' is of an unknown or higher-order type at: "
                        ++ show sp ++ " " ++ show (M.lookup var tenv)

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

commonName :: Maybe String -> String
commonName = fromMaybe "Common"

commonNameFromAST :: Maybe (F.Expression a) -> Maybe F.Name
commonNameFromAST (Just (F.ExpValue _ _ (F.ValVariable v))) = Just v
commonNameFromAST _ = Nothing

-- Freshen the names for a common block and generate a renamer from
-- the old block to this
freshenCommonNames :: TLCommon A -> (TLCommon A, RenamerCoercer)
freshenCommonNames (fname, (pname, (cname, fields))) =
        let mkRenamerAndCommon (r, tc) (v, t) =
                           let v' = caml (commonName cname) ++ "_" ++ v
                           in (M.insert v (Just v', Nothing) r, (v', t) : tc)
            (rmap, fields') = foldl' mkRenamerAndCommon (M.empty, []) fields
        in ((fname, (pname, (cname, fields'))), Just rmap)

-- From a list of typed and located common blocks group by the common
-- block name, and then group/sort within such that the "mode" block
-- is first
groupSortCommonBlock :: [TLCommon A] -> [[[TLCommon A]]]
groupSortCommonBlock commons = gccs
  where
    -- Group by names of the common blocks
    gcs = groupCommonBlocksByName commons
    -- Group within by the different common block variable-type fields
    gccs = map (sortBy (\y x -> length x `compare` length y) . group . sortBy cmpVarName) gcs

groupCommonBlocksByName :: [TLCommon A] -> [[TLCommon A]]
groupCommonBlocksByName commons =
    groupBy (\x y -> cmpEq $ cmpTLConBNames x y) commons
  where
    cmpEq = (== EQ)

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
    cmpFst = (`on` fst)


-- Nothing represents an overall identity renamer/coercer for efficiency
-- a Nothing for a variable represent a variable-level (renamer) identity
-- a Nothing for a type represents a type-level (coercer) identity
type RenamerCoercer =
    Maybe (M.Map F.Name (Maybe F.Name, Maybe (TypeInfo, TypeInfo)))

class Renaming r where
    hasRenaming :: F.Name -> r -> Bool

instance Renaming RenamerCoercer where
    hasRenaming _ Nothing   = False
    hasRenaming v (Just rc) = M.member v rc

-- sometimes we have a number of renamer coercers together
instance Renaming [RenamerCoercer] where
    hasRenaming v = any (hasRenaming v)

updateUseDecls ::
  [F.ProgramFile A] -> [TLCommon A] -> [F.ProgramFile A]
updateUseDecls fps tcs = map perPF fps
  where
    perPF p@(F.ProgramFile (F.MetaInfo v _) _) =
      transformBi (importIncludeCommons v)
      $ transformBi (matchPUnit v (F.pfGetFilename p)) p
    tcrs = mkTLCommonRenamers tcs

    inames :: F.Statement A -> Maybe String
    inames (F.StInclude _ _ (F.ExpValue _ _ (F.ValString fname)) _) = Just fname
    inames _ = Nothing

    importIncludeCommons :: F.FortranVersion -> F.ProgramUnit A -> F.ProgramUnit A
    importIncludeCommons v p =
        foldl' (flip (matchPUnit v)) p (reduceCollect inames p)

    -- Data-type generic reduce traversal
    reduceCollect :: (Data s, Data t, Uniplate t, Biplate t s) => (s -> Maybe a) -> t -> [a]
    reduceCollect k x = execWriter (transformBiM (\y -> do case k y of
                                                            Just x' -> tell [x']
                                                            Nothing -> return ()
                                                           return y) x)


    insertUses :: [F.Block A] -> F.ProgramUnit A -> F.ProgramUnit A
    insertUses uses = descendBi insertUses'
      where insertUses' :: [F.Block A] -> [F.Block A]
            insertUses' bs = uses ++ bs

    matchPUnit :: F.FortranVersion -> Filename -> F.ProgramUnit A -> F.ProgramUnit A
    matchPUnit v fname p =
        removeDecls v (map snd tcrs') p'
      where
        pname = case F.getName p of
                  F.Named n -> n
                   -- If no subname is available, use the filename
                  _         -> fname
        tcrs' = lookups pname (lookups fname tcrs)
        pos = getUnitStartPosition p
        uses = mkUseStatementBlocks pos tcrs'
        p' = insertUses uses p
        -- Lookup functions over relation s

        lookups :: Eq a => a -> [((a, b), c)] -> [(b, c)]
        lookups x = map (\((_,b),c) -> (b, c))
          . filter ((==x) . fst . fst)


    -- Given the list of renamed/coercerd variables form common blocks,
    -- remove any declaration sites
    removeDecls :: F.FortranVersion -> [RenamerCoercer] -> F.ProgramUnit A -> F.ProgramUnit A
    removeDecls v rcs p = addToProgramUnit v p' remainingAssignments
        where
     (p', remainingAssignments) = runState (transformBiM (removeDecl rcs) p) []

    -- Removes a declaration and collects a list of any default values given at
    -- declaration time (which then need to be turned into separate assignment
    -- statements)
    removeDecl :: [RenamerCoercer]
               -> F.Block A -> State [F.Statement A] (F.Block A)
    removeDecl rcs (F.BlStatement a s@(FU.SrcSpan p1 _) mlab (F.StDeclaration stA stS typ attr decls)) = do
        modify (++ assgns)
        return . F.BlStatement a' s' mlab $ F.StDeclaration stA stS typ attr decls'
      where
        (F.AList al sl declsA) = decls
        decls' = F.AList al' sl declsA'
        (assgns, declsA') = foldl' matchVar ([],[]) declsA
        -- Update annotation if declarations are being added
        ((a', s'), al')
          | null declsA'                     = ((a {refactored = Just p1, deleteNode = True}, deleteLine s),
                                                al {refactored = Just pl1})
          | length declsA' /= length declsA  = ((a {refactored = Just p1}, s), al {refactored = Just pl1})
          | otherwise                        = ((a, s), al)
          where FU.SrcSpan pl1 _ = sl

        matchVar :: ([F.Statement A], [F.Declarator A]) -> F.Declarator A
                 -> ([F.Statement A], [F.Declarator A])
        -- match on declaration (care not whether scalar or array)
        matchVar (assgnsNew, declsNew) dec = case dec of
          F.Declarator _ _ lvar@(F.ExpValue _ _ (F.ValVariable v)) _ _ init  -> doMatchVar lvar v init
          _                                                                 -> (assgnsNew, declsNew)
          where
            doMatchVar lvar v init
              | hasRenaming v rcs = case init of
                  -- Renaming exists and no default, then remove
                  Nothing -> (assgnsNew, declsNew)
                    -- Renaming exists but has default, so create an
                    -- assignment for this
                  Just initExpr -> ((F.StExpressionAssign a' (FU.getSpan dec) lvar initExpr) : assgnsNew, declsNew)
              | otherwise = (assgnsNew, dec : declsNew)  -- no renaming, preserve declaration

    removeDecl _ d = return d


-- Adds additional statements to the start of the statement block in a program unit
addToProgramUnit ::
   F.FortranVersion -> F.ProgramUnit A -> [F.Statement A] -> F.ProgramUnit A
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
getUnitStartPosition (F.PUComment _ s _) = s
getUnitStartPosition (F.PUModule _ s _ _ _) = s

renamerToUse :: RenamerCoercer -> [(F.Name, F.Name)]
renamerToUse Nothing = []
renamerToUse (Just m) = let entryToPair _ (Nothing, _) = []
                            entryToPair v (Just v', _) = [(v, v')]
                        in M.foldlWithKey' (\xs v e -> entryToPair v e ++ xs) [] m

-- make the use statements for a particular program unit's common blocks
mkUseStatementBlocks :: FU.SrcSpan -> [(TCommon A, RenamerCoercer)] -> [F.Block A]
mkUseStatementBlocks s = map mkUseStmnt
  where
    a = unitAnnotation { refactored = Just pos, newNode = True }
    (FU.SrcSpan pos pos') = s
    s' = FU.SrcSpan (toCol0 pos) pos'
    mkUseStmnt x@((name, _), _) = F.BlStatement a s' Nothing $
       F.StUse a s' useName Nothing F.Permissive useListA
     where useName = F.ExpValue a s' (F.ValVariable (caml (commonName name)))
           useListA = case useList of [] -> Nothing
                                      us -> Just (F.AList a s' (reverse us))
           useList = mkUses pos x

    mkUses :: FU.Position -> (TCommon A, RenamerCoercer) -> [F.Use A]
    mkUses _ ((_, _), r) = map useRenamer (renamerToUse r)

    useRenamer (v, vR) = F.UseRename a s' (F.ExpValue a s' (F.ValVariable v))
                                          (F.ExpValue a s' (F.ValVariable vR))

mkRenamerCoercerTLC :: TLCommon A :? source -> TLCommon A :? target -> RenamerCoercer
mkRenamerCoercerTLC (_, (_, common1)) (_, (_, common2)) =
    mkRenamerCoercer common1 common2

mkRenamerCoercer :: TCommon A :? source -> TCommon A :? target -> RenamerCoercer
mkRenamerCoercer (name1, vtys1) (name2, vtys2)
  | name1 == name2 = if vtys1 == vtys2 then Nothing
                     else Just $ generate vtys1 vtys2 M.empty
  | otherwise      =
        error "Can't generate renamer between different common blocks\n"
      where
        generate [] [] theta = theta
        generate ((var1, ty1):vtys1') ((var2, ty2):vtys2') theta =
            generate vtys1' vtys2' (M.insert var1 (varR, typR) theta)
          where
             varR = if var1 == var2 then Nothing else Just var2
             typR = if ty1  ==  ty2 then Nothing else Just (ty1, ty2)
        generate _ _ _ = error "Common blocks of different field length\n"

-- Checks whether all commons of the same name (i.e., across program units)
-- are coherent with regards their types, returning a string of errors (if there are any)
-- and a boolean to indicate coherence or not
allCoherentCommons :: [TLCommon A] -> (String, Bool)
allCoherentCommons commons = do
    ps <- mapM checkCoherence (groupCommonBlocksByName commons)
    return (and ps)
   where
      checkCoherence :: [TLCommon A] -> (String, Bool)
      checkCoherence cs =
        foldM (\p (c1, c2) -> coherentCommons c1 c2 >>= \p' -> return $ p && p') True (pairs cs)

      -- Computes all pairwise combinations
      pairs :: [a] -> [(a, a)]
      pairs []     = []
      pairs (x:xs) = zip (repeat x) xs ++ pairs xs

coherentCommons :: TLCommon A -> TLCommon A -> (String, Bool)
coherentCommons (_, (_, (n1, vtys1))) (_, (_, (n2, vtys2))) =
    if n1 == n2
      then
        coherentCommons' vtys1 vtys2
      else error $ "Trying to compare differently named common blocks: "
                 ++ show n1 ++ " and " ++ show n2 ++ "\n"

coherentCommons' ::  [(F.Name, TypeInfo)] -> [(F.Name, TypeInfo)] -> (String, Bool)
coherentCommons' []               []                = ("", True)
coherentCommons' ((var1, ty1):xs) ((var2, ty2):ys)
      | af ty1 == af ty2 = let (r', c) = coherentCommons' xs ys
                                           in (r', c && True)
      | otherwise = let r = var1 ++ ":"
                          ++ PP.pprintAndRender F.Fortran90 (fst ty1) Nothing
                          ++ "(" ++ show (af ty1) ++ ")"
                          ++ " differs from " ++ var2
                          ++ ":" ++ PP.pprintAndRender F.Fortran90 (fst ty2) Nothing
                          ++ "(" ++ show (af ty2) ++ ")" ++ "\n"
                        (r', _) = coherentCommons' xs ys
                    in (r ++ r', False)
    -- TODO - give more information in the error
coherentCommons' _ _ = ("Common blocks of different field lengths", False)

introduceModules :: F.MetaInfo
                 -> Directory
                 -> [TLCommon A]
                 -> (String, [F.ProgramFile A])
introduceModules meta dir cenv =
    mapM (mkModuleFile meta dir . head . head) (groupSortCommonBlock cenv)

mkModuleFile ::
  F.MetaInfo -> Directory -> TLCommon A -> (String, F.ProgramFile A)
mkModuleFile meta dir (_, (_, (name, varTys))) =
    (r, F.pfSetFilename path $ F.ProgramFile meta [mod])
  where
    modname = commonName name
    path = dir ++ modname ++ ".f90"
    r = "Creating module " ++ modname ++ " at " ++ path ++ "\n"
    mod = mkModule (F.miVersion meta) modname varTys modname

mkModule :: F.FortranVersion -> String -> [(F.Name, TypeInfo)] -> String -> F.ProgramUnit A
mkModule v name vtys fname =
    F.PUModule a sp (caml fname) decls Nothing
  where
    a = unitAnnotation { refactored = Just loc, newNode = True }
    loc = FU.Position 0 0 0 "" Nothing
    sp = FU.SrcSpan loc loc
    toDeclBlock (v, t) = F.BlStatement a sp Nothing (toStmt (v, t))
    toStmt (v, (st, ct)) = F.StDeclaration a sp (typespec st) attrs (toDeclarator (v, ct))
    attrs = Just $ F.AList a sp [F.AttrSave a sp]
    typespec = FAS.recoverSemTypeTypeSpec a sp v
    toDeclarator (v, FA.CTVariable) = F.AList a sp
       [F.Declarator a sp
          (F.ExpValue a sp (F.ValVariable (caml name ++ "_" ++ v))) F.ScalarDecl Nothing Nothing]
    toDeclarator (v, FA.CTArray dims) = F.AList a sp
       [F.Declarator a sp
          (F.ExpValue a sp (F.ValVariable (caml name ++ "_" ++ v))) (F.ArrayDecl dimDecls) Nothing Nothing]
       where
         dimDecls = F.AList a sp . flip map dims $ \ (lb, ub) -> F.DimensionDeclarator a sp (fmap expr lb) (fmap expr ub)
         expr x = F.ExpValue a sp $ F.ValInteger (show x) Nothing
    toDeclarator (_, ct) = error $ "mkModule: toDeclarator: bad construct type: " ++ show ct
    decls = map toDeclBlock vtys
