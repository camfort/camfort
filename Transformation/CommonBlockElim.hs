{-# LANGUAGE ImplicitParams, DeriveDataTypeable, TypeOperators #-}

module Transformation.CommonBlockElim where

import Control.Monad
import Control.Monad.State.Lazy

import Debug.Trace
import Data.Data
import Data.List
import Data.Ord
import qualified Data.Map as Data.Map
import Data.Generics.Uniplate.Operations

import Language.Fortran
import Language.Fortran.Pretty

import Helpers
import Traverse
import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types
import Transformation.Syntax

-- Typed common block representation
type TCommon p = (Maybe String, [(Variable, Type p)])

-- Typed and "located" common block representation
-- TODO: include column + line information 
type TLCommon p = (Filename, (String, TCommon p))

-- Top-level functions for eliminating common blocks in a set of files
commonElimToModules :: Directory -> [(Filename, Program A)] -> (Report, [(Filename, Program A)])

-- Eliminates common blocks in a program directory (and convert to modules)
commonElimToModules d ps = let (ps', (r, cg)) = runState (analyseCommons ps) ("", [])
                               (r', ps'') = introduceModules d cg
                               psR = updateUseDecls ps' cg
                           in  (r ++ r', psR ++ ps'') 



analyseCommons :: [(Filename, Program A)] -> State (Report, [TLCommon A]) [(Filename, Program A)] 
analyseCommons pss = let 
                          defs' :: Filename -> ProgUnit A -> State (Report, [TLCommon A]) (ProgUnit A)
                          defs' fname p = case (getSubName p) of
                                            Just pname -> transformBiM (collectCommons fname pname) p
                                            Nothing -> case p of 
                                                         IncludeProg a sp ds f -> 
                                                            -- ("doing an include: " ++ (show fname)) `trace`
                                                            let -- create dummy block
                                                                a0 = unitAnnotation
                                                                b = Block a (UseBlock (UseNil a0) nullLoc) 
                                                                            (ImplicitNull a0) sp ds
                                                                            (NullStmt a0 nullSpan)
                                                             in do (Block _ _ _ _ ds' _) <- transformBiM (collectCommons fname fname) b
                                                                   return $ IncludeProg a sp ds' f
                                                         otherwise -> return p 
                          -- defs' f (Sub _ _ _ (SubName _ n) _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons' b) []))]
                          -- Don't support functions yet
                          -- defs' f (Function _ _ _ (SubName _ n) _ _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons b) []))]
                          -- defs' _ _ rs = concat rs

                      in mapM (\(f, ps) -> do ps' <- mapM (transformBiM (defs' f)) ps
                                              return (f, ps')) pss

collectCommons :: Filename -> String -> Block A -> State (Report, [TLCommon A]) (Block A)
collectCommons fname pname b = 
    let tenv = typeEnv b
                    
        commons' :: Decl A -> State (Report, [TLCommon A]) (Decl A)
        commons' f@(Common a sp cname exprs) = 
            do let r' = fname ++ (show $ srcLineCol $ fst sp) ++ ": removed common declaration\n"
               (r, env) <- get
               put (r ++ r', (fname, (pname, (cname, typeCommonExprs exprs))):env)
               return $ (NullDecl (a { refactored = (Just $ fst sp) }) sp)
        commons' f = return f

        typeCommonExprs :: [Expr Annotation] -> [(Variable, Type Annotation)]
        typeCommonExprs [] = []
        typeCommonExprs ((Var _ sp [(VarName _ v, _)]):es) = 
            case (tenvLookup v tenv) of
                 Just t -> (v, t) : (typeCommonExprs es)
                 Nothing -> error $ "Variable " ++ (show v) ++ " is of an unknown type at: " ++ show sp
        typeCommonExprs (e:_) = error $ "Not expecting a non-variable expression in expression at: " ++ show (srcSpan e)

    in transformBiM commons' b            

{- Comparison functions for common block names and variables -}
cmpTLConFName :: TLCommon A -> TLCommon A -> Ordering
cmpTLConFName (f1, (_, _)) (f2, (_, _)) = compare f1 f2

cmpTLConPName :: TLCommon A -> TLCommon A -> Ordering
cmpTLConPName (_, (p1, _)) (_, (p2, _)) = compare p1 p2

cmpTLConBNames :: TLCommon A -> TLCommon A -> Ordering
cmpTLConBNames (_, (_, c1)) (_, (_, c2)) = cmpTConBNames c1 c2

cmpTConBNames :: TCommon A -> TCommon A -> Ordering
cmpTConBNames (Nothing, _) (Nothing, _) = EQ
cmpTConBNames (Nothing, _) (Just _, _)  = LT
cmpTConBNames (Just _, _) (Nothing, _)  = GT
cmpTConBNames (Just n, _) (Just n', _) = if (n < n') then LT
                                            else if (n > n') then GT else EQ

-- Fold [TLCommon p] to get a list of ([(TLCommon p, Renamer p)], [(Filename, Program A)])
-- How to decide which gets to be the "head" perhaps the one which triggers the *least* renaming (ooh!)
--  (this is calculated by looking for the mode of the TLCommon (for a particular Common)
--  (need to do gorouping, but sortBy is used already so... (IS THIS STABLE- does this matter?))

onCommonBlock :: (TCommon A -> TCommon A) -> TLCommon A -> TLCommon A
onCommonBlock f (fname, (pname, tcommon)) = (fname, (pname, f tcommon))

commonName Nothing  = "Common"
commonName (Just x) = x

-- Freshen the names for a common block and generate a renamer from the old block to this
freshenCommonNames :: TLCommon A -> (TLCommon A, RenamerCoercer)
freshenCommonNames (fname, (pname, (cname, fields))) = 
        let mkRenamerAndCommon (r, tc) (v, t) =
                           let v' = (caml $ commonName cname) ++ "_" ++ v
                           in (Data.Map.insert v (Just v', Nothing) r, (v', t) : tc) 
            (r, fields') = foldl mkRenamerAndCommon (Data.Map.empty, []) fields
        in ((fname, (pname, (cname, fields'))), Just r)

-- From a list of typed and located common blocks 
-- group by the common block name, and then group/sort within such that the "mode" block is first
groupSortCommonBlock :: [TLCommon A] -> [[[TLCommon A]]]
groupSortCommonBlock commons = let -- Group by names of the common blocks
                                  gcs = groupBy (\x y -> cmpEq $ cmpTLConBNames x y) commons 
                                  -- Group within by the different common block variable-type fields
                                  gccs = map (sortBy (\y x -> length x `compare` length y) . group . sortBy cmpVarName) gcs
                              in gccs

cmpVarName :: TLCommon A -> TLCommon A -> Ordering
cmpVarName (fname1, (pname1, (name1, vtys1))) (fnam2, (pname2, (name2, vtys2))) = map fst vtys1 `compare` map fst vtys2

mkTLCommonRenamers :: [TLCommon A] -> [(TLCommon A, RenamerCoercer)]
mkTLCommonRenamers commons = case allCoherentCommonsP commons of
                (r, False) -> error $ "Common blocks are incoherent!\n" ++ r -- (r, []) -- Incoherent commons
                (_, True) -> let gccs = groupSortCommonBlock commons
                                 -- Find the "mode" common block and freshen the names for this, creating
                                 -- a renamer between this and every module
                                 gcrcs = map (\grp -> -- grp are block decls all for the same block
                                                 let (com, r) = freshenCommonNames (head (head grp))
                                                 in  map (\c -> (c, r)) (head grp) ++ 
                                                     map (\c -> (c, mkRenamerCoercerTLC c com)) (concat $ tail grp)) gccs
                                 -- Now re-sort based on the file and program unit
                                 gcrcs' = sortBy (cmpFst cmpTLConFName) (sortBy (cmpFst cmpTLConPName) (concat gcrcs))
                             in gcrcs'


updateUseDecls :: [(Filename, Program A)] -> [TLCommon A] -> [(Filename, Program A)]
updateUseDecls fps tcs = 
      let tcrs = mkTLCommonRenamers tcs 

          concatUses :: Uses A -> Uses A -> Uses A
          concatUses (UseNil p) y      = y
          concatUses (Use p x us p') y = Use p x (UseNil p) p'

          inames :: Decl A -> Maybe String
          inames (Include _ (Con _ _ inc)) = Just inc
          inames _ = Nothing

          importIncludeCommons :: ProgUnit A -> ProgUnit A
          importIncludeCommons p = foldl (\p' iname -> ("Iname = " ++ iname) `trace` matchPUnitAlt iname p') p (reduceCollect inames p)

          matchPUnitAlt :: Filename -> ProgUnit A -> ProgUnit A
          matchPUnitAlt fname p = ("fname = " ++ fname ++ "\n" ++ (show ((lookups' fname) (lookups' fname tcrs)))) `trace` 
                                let tcrs' = (lookups' fname) (lookups' fname tcrs)
                                    srcloc = useSrcLoc p
                                    uses = mkUseStatements srcloc tcrs'
                                    p' = transformBi ((flip concatUses) uses) p
                                in let ?fname = fname in removeDecls (map snd tcrs') p'

                                      
          matchPUnit :: Filename -> ProgUnit A -> ProgUnit A
          matchPUnit fname p = let pname = case getSubName p of
                                              Nothing -> fname -- If no subname is available, use the filename
                                              Just pname -> pname
                                   tcrs' = (lookups' pname) (lookups' fname tcrs)
                                   srcloc = useSrcLoc p
                                   uses = mkUseStatements srcloc tcrs'
                                   p' = transformBi ((flip concatUses) uses) p
                               in let ?fname = fname in removeDecls (map snd tcrs') p'
                                                
          -- Given the list of renamed/coercerd variables form common blocks, remove any declaration sites
          removeDecls :: (?fname :: Filename) => [RenamerCoercer] -> ProgUnit A -> ProgUnit A
          removeDecls rcs p = let (p', remainingAssignments) = runState (transformBiM (removeDecl rcs) p) []
                               in addToProgUnit p' remainingAssignments

          -- Removes a declaration and collects a list of any default values given at declaration time
          -- (which then need to be turned into separate assignment statements)
          removeDecl :: (?fname :: Filename) => [RenamerCoercer] -> Decl A -> State [Fortran A] (Decl A)
          removeDecl rcs d@(Decl p srcP vars typ) = 
               (modify (++ assgns)) >> (return $ if (vars' == []) then  NullDecl p' srcP 
                                                                  else  Decl p' srcP vars' typ)
               where
                   (assgns, vars') = foldl matchVar ([],[]) vars 
                   p'    = if (length vars == length vars') then p else p { refactored = Just (fst srcP) }

                   matchVar :: ([Fortran A], [(Expr A, Expr A, Maybe Int)]) 
                             -> (Expr A, Expr A, Maybe Int)
                            -> ([Fortran A], [(Expr A, Expr A, Maybe Int)])
                   matchVar (assgns, decls) dec@(lvar@(Var _ _ [(VarName _ v, _)]), e, _) = 
                                 if (hasRenaming v rcs) then
                                    case e of
                                     -- Renaming exists and no default, then remove
                                        NullExpr _ _ -> (assgns, decls)  
                                     -- Renaming exists but has default, so create an assignment for this
                                        e            -> ((Assg p' srcP lvar e) : assgns, decls)
                                 else -- no renaming, preserve declaration
                                       (assgns, dec : decls)
                   matchVar (assgns, decls) _ = (assgns, decls)
          removeDecl _ d = return d
                     
       in each fps (\(f, p) -> (f, map importIncludeCommons $ transformBi (matchPUnit f) p))

-- Adds additional statements to the start of the statement block in a program unit
addToProgUnit :: ProgUnit A -> [Fortran A] -> ProgUnit A
addToProgUnit p [] = p
addToProgUnit (IncludeProg p sp decl Nothing) stmts = IncludeProg p sp decl (Just $ 
                                                           prependStatements (Just $ afterEnd sp) (NullStmt unitAnnotation (afterEnd sp)) stmts)
addToProgUnit (IncludeProg p sp decl (Just f)) stmts = IncludeProg p sp decl (Just $ prependStatements Nothing f stmts)
addToProgUnit p stmts = transformBi (flip addToBlock stmts) p

-- Add additional statements to the start of a block
addToBlock :: Block A -> [Fortran A] -> Block A
addToBlock b [] = b
addToBlock (Block p useBlock imps sp decls stmt) stmts = Block p useBlock imps sp decls (prependStatements Nothing stmt stmts)

-- Prepends statements onto a statement
prependStatements :: Maybe SrcSpan -> Fortran A -> [Fortran A] -> Fortran A
prependStatements sp stmt ss = FSeq p' sp' (foldl1 (FSeq p' sp') ss) stmt
                                  where p' = (annotation stmt) { refactored = Just (fst sp') }
                                        sp' = case sp of 
                                                Nothing -> srcSpan stmt
                                                Just s  -> s

useSrcLoc :: ProgUnit A -> SrcLoc
useSrcLoc (Main _ _ _ _ b _)      = useSrcLocB b
useSrcLoc (Sub _ _ _ _ _ b)       = useSrcLocB b
useSrcLoc (Function _ _ _ _ _ _ b)= useSrcLocB b
useSrcLoc (Module _ s _ _ _ _ _)  = fst s -- TOOD: this isn't very accurate 
useSrcLoc (BlockData _ s _ _ _ _) = fst s
useSrcLocB (Block _ (UseBlock _ s) _ _ _ _) = s

renamerToUse :: RenamerCoercer -> [(Variable, Variable)]
renamerToUse Nothing = []
renamerToUse (Just m) = let entryToPair v (Nothing, _) = []
                            entryToPair v (Just v', _) = [(v, v')]
                        in Data.Map.foldlWithKey (\xs v e -> (entryToPair v e) ++ xs) [] m

-- make the use statements for a particular program unit's common blocks
mkUseStatements :: SrcLoc -> [(TCommon A, RenamerCoercer)] -> Uses A
mkUseStatements s [] = UseNil (unitAnnotation)
mkUseStatements s (((name, _), r):trs) = 
                        let a = unitAnnotation { refactored = Just s, newNode = True } -- previously-- Just (toCol0 s)
                        in Use a (commonName name, renamerToUse r) (mkUseStatements s trs) a

mkRenamerCoercerTLC :: TLCommon A :? source -> TLCommon A :? target -> RenamerCoercer
mkRenamerCoercerTLC x@(fname, (pname, common1)) (_, (_, common2)) = mkRenamerCoercer common1 common2

mkRenamerCoercer :: TCommon A :? source -> TCommon A :? target -> RenamerCoercer
mkRenamerCoercer (name1, vtys1) (name2, vtys2)
     | name1 == name2 = if (vtys1 == vtys2) then Nothing else Just $ generate vtys1 vtys2 Data.Map.empty
     | otherwise      = error "Can't generate renamer between different common blocks\n"
                           where
                             generate [] [] theta = theta
                             generate ((var1, ty1):vtys1) ((var2, ty2):vtys2) theta = 
                                 let varR = if (var1 == var2) then Nothing else Just var2
                                     typR = if (ty1  ==  ty2) then Nothing else Just (ty1, ty2)
                                 in generate vtys1 vtys2 (Data.Map.insert var1 (varR, typR) theta)
                             generate _ _ _ = error "Common blocks of different field length\n"

allCoherentCommonsP :: [TLCommon A] -> (Report, Bool)
allCoherentCommonsP commons = foldM (\p (c1, c2) -> (coherentCommonsP c1 c2) >>= (\p' -> return $ p && p')) True (pairs commons)

coherentCommonsP :: TLCommon A -> TLCommon A -> (Report, Bool)
coherentCommonsP (f1, (p1, (n1, vtys1))) (f2, (p2, (n2, vtys2))) =
    if (n1 == n2) then
         let  coherent ::  [(Variable, Type A)] -> [(Variable, Type A)] -> (Report, Bool)
              coherent []               []                = ("", True)
              coherent ((var1, ty1):xs) ((var2, ty2):ys) 
                      | af ty1 == af ty2 = let (r', c) = coherent xs ys
                                           in (r', c && True)
                      | otherwise = let ?variant = Alt1 in
                                    let r = (var1 ++ ":" ++ (outputF ty1) ++ "(" ++ (show $ af ty1) ++ ")" ++ " differs from " ++ 
                                             var2 ++ ":" ++ (outputF ty2) ++ "(" ++ (show $ af ty2) ++ ")" ++ "\n")
                                        (r', _) = coherent xs ys
                                    in (r ++ r', False)
              coherent _ _ = ("Common blocks of different field lengths", False) -- Doesn't say which is longer
         in coherent vtys1 vtys2 

    else ("", True) -- Not sure if this is supposed to fail here- in retrospect I think no
          -- False -> ("Trying to compare differently named common blocks: " ++ show n1 ++ " and " ++ show n2 ++ "\n", False)

introduceModules :: Directory -> [TLCommon A] -> (Report, [(Filename, Program A)]) 
introduceModules d cenv = mapM (mkModuleFile d) (map (head . head) (groupSortCommonBlock cenv))


mkModuleFile :: Directory -> (TLCommon A) -> (Report, (Filename, Program A))
mkModuleFile d (_, (_, (name, varTys))) =
        let modname = commonName name
            fullpath = d ++ "/" ++ modname ++ ".f90"
            r = "Created module " ++ modname ++ " at " ++ fullpath ++ "\n"
        in (r, (fullpath, [mkModule modname varTys modname]))

mkModule :: String -> [(Variable, Type A)] -> String -> ProgUnit A
mkModule name vtys fname = 
                      let a = unitAnnotation { refactored = Just loc }
                          loc = SrcLoc (fname ++ ".f90") 0 0 
                          sp = (loc, loc)
                          toDecl (v, t) = Decl a sp [(Var a sp [(VarName a (name ++ "_" ++ v), [])], NullExpr a sp, Nothing)] -- note here could pull in initialising definition? What if conflicts- highlight as potential source of error?
                                                            t
                          decls = foldl1 (DSeq a) (map toDecl vtys)
                      in Module a (loc, loc) (SubName a fname) (UseNil a) (ImplicitNone a) decls []

