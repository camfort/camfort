> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable, TypeOperators #-}

> module Transformation.CommonBlockElim where

> import Control.Monad

> import Debug.Trace
> import Data.Data
> import Data.List
> import Data.Ord
> import qualified Data.Map as Data.Map

> import Language.Fortran
> import Language.Fortran.Pretty

> import Language.Haskell.Syntax (SrcLoc(..))

> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy

> import Helpers

> import Analysis.Annotations
> import Analysis.Syntax
> import Analysis.Types
> import Transformation.Syntax
> import Traverse

Todo: CallExpr, changing assignments 

> -- Typed common block representation
> type TCommon p = (Maybe String, [(Variable, Type p)])

> -- Typed and "located" common block representation
> -- TODO: include column + line information 
> type TLCommon p = (Filename, (String, TCommon p))

> commonElimToCalls, commonElimToModules :: Directory -> [(Filename, Program A)] -> (Report, [(Filename, Program A)])

> -- Eliminates common blocks in a program directory (and convert to calls)
> commonElimToCalls d ps = let (ps', (r, cg)) = runState (definitionSites ps) ("", [])
>                              (r', ps'') = mapM (introduceCalls cg) ps'
>                          in (r ++ r', ps'')

> -- Eliminates common blocks in a program directory (and convert to modules)
> commonElimToModules d ps = let (ps', (r, cg)) = runState (definitionSites ps) ("", [])
>                                (r', ps'') = introduceModules d cg
>                                psR = updateUseDecls ps' cg
>                            in  (r ++ r', psR ++ ps'') 

> nonNullArgs (ASeq _ _ _) = True
> nonNullArgs (ArgName _ _) = True
> nonNullArgs (NullArg _) = False


-- Fold [TLCommon p] to get a list of ([(TLCommon p, Renamer p)], [(Filename, Program A)])
-- How to decide which gets to be the "head" perhaps the one which triggers the *least* renaming (ooh!)
--  (this is calculated by looking for the mode of the TLCommon (for a particular Common)
--  (need to do gorouping, but sortBy is used already so... (IS THIS STABLE- does this matter?))

> onCommonBlock :: (TCommon A -> TCommon A) -> TLCommon A -> TLCommon A
> onCommonBlock f (fname, (pname, tcommon)) = (fname, (pname, f tcommon))

> commonName Nothing  = "Common"
> commonName (Just x) = x


> -- Freshen the names for a common block and generate a renamer from the old block to this
> freshenCommonNames :: TLCommon A -> (TLCommon A, RenamerCoercer)
> freshenCommonNames (fname, (pname, (cname, fields))) = 
>         let mkRenamerAndCommon (r, tc) (v, t) =
>                            let v' = (caml $ commonName cname) ++ "_" ++ v
>                            in (Data.Map.insert v (Just v', Nothing) r, (v', t) : tc) 
>             (r, fields') = foldl mkRenamerAndCommon (Data.Map.empty, []) fields
>         in ((fname, (pname, (cname, fields'))), Just r)


> -- From a list of typed and located common blocks 
> -- group by the common block name, and then group/sort within such that the "mode" block is first
> groupSortCommonBlock :: [TLCommon A] -> [[[TLCommon A]]]
> groupSortCommonBlock commons = let -- Group by names of the common blocks
>                                   gcs = groupBy (\x y -> cmpEq $ cmpTLConBNames x y) commons 
>                                   -- Group within by the different common block variable-type fields
>                                   gccs = map (sortBy (\y x -> length x `compare` length y) . group . sortBy cmpVarName) gcs
>                               in gccs

> cmpVarName :: TLCommon A -> TLCommon A -> Ordering
> cmpVarName (fname1, (pname1, (name1, vtys1))) (fnam2, (pname2, (name2, vtys2))) = map fst vtys1 `compare` map fst vtys2

> mkTLCommonRenamers :: [TLCommon A] -> [(TLCommon A, RenamerCoercer)]
> mkTLCommonRenamers commons = case allCoherentCommonsP commons of
>                 (r, False) -> error $ "Common blocks are incoherent!\n" ++ r -- (r, []) -- Incoherent commons
>                 (_, True) -> let gccs = groupSortCommonBlock commons
>                                  -- Find the "mode" common block and freshen the names for this, creating
>                                  -- a renamer between this and every module
>                                  gcrcs = map (\grp -> -- grp are block decls all for the same block
>                                                  let (com, r) = freshenCommonNames (head (head grp))
>                                                  in  map (\c -> (c, r)) (head grp) ++ 
>                                                      map (\c -> (c, mkRenamerCoercerTLC c com)) (concat $ tail grp)) gccs
>                                  -- Now re-sort based on the file and program unit
>                                  gcrcs' = sortBy (cmpFst cmpTLConFName) (sortBy (cmpFst cmpTLConPName) (concat gcrcs))
>                              in gcrcs'


> updateUseDecls :: [(Filename, Program A)] -> [TLCommon A] -> [(Filename, Program A)]
> updateUseDecls fps tcs = 
>       let tcrs = mkTLCommonRenamers tcs 

>           concatUses :: Uses A -> Uses A -> Uses A
>           concatUses (UseNil p) y      = y
>           concatUses (Use p x us p') y = Use p x (UseNil p) p'

>           inames :: Decl A -> Maybe String
>           inames (Include _ (Con _ _ inc)) = Just inc
>           inames _ = Nothing

>           importIncludeCommons :: ProgUnit A -> ProgUnit A
>           importIncludeCommons p = foldl (\p' iname -> ("Iname = " ++ iname) `trace` matchPUnitAlt iname p') p (reduceCollect inames p)

>           matchPUnitAlt :: Filename -> ProgUnit A -> ProgUnit A
>           matchPUnitAlt fname p = ("fname = " ++ fname ++ "\n" ++ (show ((lookups' fname) (lookups' fname tcrs)))) `trace` 
>                                let tcrs' = (lookups' fname) (lookups' fname tcrs)
>                                    srcloc = useSrcLoc p
>                                    uses = mkUseStatements srcloc tcrs'
>                                    p' = transformBi ((flip concatUses) uses) p
>                                in removeDecls (map snd tcrs') p'

>                                      
>           matchPUnit :: Filename -> ProgUnit A -> ProgUnit A
>           matchPUnit fname p = let pname = case getSubName p of
>                                              Nothing -> fname -- If no subname is available, use the filename
>                                              Just pname -> pname
>                                    tcrs' = (lookups' pname) (lookups' fname tcrs)
>                                    srcloc = useSrcLoc p
>                                    uses = mkUseStatements srcloc tcrs'
>                                    p' = transformBi ((flip concatUses) uses) p
>                                in removeDecls (map snd tcrs') p'

>           removeDecls :: [RenamerCoercer] -> ProgUnit A -> ProgUnit A
>           removeDecls rcs p = transformBi (remDecl rcs) p

>           matchrc _ Nothing = False
>           matchrc v (Just rc) =  Data.Map.member v rc

>           remDecl :: [RenamerCoercer] -> Decl A -> (Decl A) --  [Fortran A])
>           remDecl rcs d@(Decl p srcP [lvar@(Var _ _ [(VarName _ v, [])], e, _)] _) =
>                 if (or (map (matchrc v) rcs)) then 
>                   case e of
>                     NullExpr _ _ -> (NullDecl ( p { refactored = Just (fst srcP) }) srcP) --  [])
>                     e            -> (NullDecl ( p { refactored = Just (fst srcP) }) srcP) -- [Assg (p { refactored = Just (fst srcP) }) srcP lvar e])
>                 else d
>           remDecl _ d = d
>                     

>       in each fps (\(f, p) -> (f, map importIncludeCommons $ transformBi (matchPUnit f) p))


> useSrcLoc :: ProgUnit A -> SrcLoc
> useSrcLoc (Main _ _ _ _ b _)      = useSrcLocB b
> useSrcLoc (Sub _ _ _ _ _ b)       = useSrcLocB b
> useSrcLoc (Function _ _ _ _ _ _ b)= useSrcLocB b
> useSrcLoc (Module _ s _ _ _ _ _)  = fst s -- TOOD: this isn't very accurate 
> useSrcLoc (BlockData _ s _ _ _ _) = fst s
> useSrcLocB (Block _ (UseBlock _ s) _ _ _ _) = s

> renamerToUse :: RenamerCoercer -> [(Variable, Variable)]
> renamerToUse Nothing = []
> renamerToUse (Just m) = let entryToPair v (Nothing, _) = []
>                             entryToPair v (Just v', _) = [(v, v')]
>                         in Data.Map.foldlWithKey (\xs v e -> (entryToPair v e) ++ xs) [] m

> -- make the use statements for a particular program unit's common blocks
> mkUseStatements :: SrcLoc -> [(TCommon A, RenamerCoercer)] -> Uses A
> mkUseStatements s [] = UseNil (unitAnnotation)
> mkUseStatements s (((name, _), r):trs) = 
>                         let a = unitAnnotation { refactored = Just s, newNode = True } -- previously-- Just (toCol0 s)
>                         in Use a (commonName name, renamerToUse r) (mkUseStatements s trs) a

> mkRenamerCoercerTLC :: TLCommon A :? source -> TLCommon A :? target -> RenamerCoercer
> mkRenamerCoercerTLC x@(fname, (pname, common1)) (_, (_, common2)) = mkRenamerCoercer common1 common2

> mkRenamerCoercer :: TCommon A :? source -> TCommon A :? target -> RenamerCoercer
> mkRenamerCoercer (name1, vtys1) (name2, vtys2)
>      | name1 == name2 = if (vtys1 == vtys2) then Nothing else Just $ generate vtys1 vtys2 Data.Map.empty
>      | otherwise      = error "Can't generate renamer between different common blocks\n"
>                            where
>                              generate [] [] theta = theta
>                              generate ((var1, ty1):vtys1) ((var2, ty2):vtys2) theta = 
>                                  let varR = if (var1 == var2) then Nothing else Just var2
>                                      typR = if (ty1  ==  ty2) then Nothing else Just (ty1, ty2)
>                                  in generate vtys1 vtys2 (Data.Map.insert var1 (varR, typR) theta)
>                              generate _ _ _ = error "Common blocks of different field length\n"

> allCoherentCommonsP :: [TLCommon A] -> (Report, Bool)
> allCoherentCommonsP commons = foldM (\p (c1, c2) -> (coherentCommonsP c1 c2) >>= (\p' -> return $ p && p')) True (pairs commons)

> coherentCommonsP :: TLCommon A -> TLCommon A -> (Report, Bool)
> coherentCommonsP (f1, (p1, (n1, vtys1))) (f2, (p2, (n2, vtys2))) =
>     if (n1 == n2) then
>          let  coherent ::  [(Variable, Type A)] -> [(Variable, Type A)] -> (Report, Bool)
>               coherent []               []                = ("", True)
>               coherent ((var1, ty1):xs) ((var2, ty2):ys) 
>                       | af ty1 == af ty2 = let (r', c) = coherent xs ys
>                                            in (r', c && True)
>                       | otherwise = let ?variant = Alt1 in
>                                     let r = (var1 ++ ":" ++ (outputF ty1) ++ "(" ++ (show $ af ty1) ++ ")" ++ " differs from " ++ 
>                                              var2 ++ ":" ++ (outputF ty2) ++ "(" ++ (show $ af ty2) ++ ")" ++ "\n")
>                                         (r', _) = coherent xs ys
>                                     in (r ++ r', False)
>               coherent _ _ = ("Common blocks of different field lengths", False) -- Doesn't say which is longer
>          in coherent vtys1 vtys2 

>     else ("", True) -- Not sure if this is supposed to fail here- in retrospect I think no
>           -- False -> ("Trying to compare differently named common blocks: " ++ show n1 ++ " and " ++ show n2 ++ "\n", False)

> introduceModules :: Directory -> [TLCommon A] -> (Report, [(Filename, Program A)]) 
> introduceModules d cenv = mapM (mkModuleFile d) (map (head . head) (groupSortCommonBlock cenv))


> mkModuleFile :: Directory -> (TLCommon A) -> (Report, (Filename, Program A))
> mkModuleFile d (_, (_, (name, varTys))) =
>         let modname = commonName name
>             fullpath = d ++ "/" ++ modname ++ ".f90"
>             r = "Created module " ++ modname ++ " at " ++ fullpath ++ "\n"
>         in (r, (fullpath, [mkModule modname varTys modname]))

> mkModule :: String -> [(Variable, Type A)] -> String -> ProgUnit A
> mkModule name vtys fname = 
>                       let a = unitAnnotation { refactored = Just loc }
>                           loc = SrcLoc (fname ++ ".f90") 0 0 
>                           sp = (loc, loc)
>                           toDecl (v, t) = Decl a sp [(Var a sp [(VarName a (name ++ "_" ++ v), [])], NullExpr a sp, Nothing)] -- note here could pull in initialising definition? What if conflicts- highlight as potential source of error?
>                                                             t
>                           decls = foldl1 (DSeq a) (map toDecl vtys)
>                       in Module a (loc, loc) (SubName a fname) (UseNil a) (ImplicitNone a) decls []

Extending calls version


> introduceCalls :: [TLCommon A] -> (Filename, Program A) -> (Report, (Filename, Program A))
> introduceCalls cenv (fname, ps) = do ps' <- mapM (transformBiM commonElim) ps
>                                      -- ps'' <- mapM (transformBiM commonElim'') ps'
>                                      return (fname, ps')

>               where commonElim s@(Sub a sp mbt (SubName a' moduleName) (Arg p arg asp) b) = 
>                         
>                          let commons = lookups moduleName (lookups fname cenv) 
>                              sortedC = sortBy cmpTConBNames commons
>                              tArgs = extendArgs (nonNullArgs arg) asp (concatMap snd sortedC)
>                              ra = p { refactored = Just (fst sp) }
>                              arg' = Arg unitAnnotation (ASeq unitAnnotation arg tArgs) asp
>                              a' = a -- { pRefactored = Just sp }
>                              r = fname ++ (show $ srcLineCol $ snd asp) ++ ": changed common variables to parameters\n"
>                          in do b' <- transformBiM (extendCalls fname moduleName cenv) b
>                                (r, Sub a' sp mbt (SubName a' moduleName) arg' b')
>
>                     commonElim s = --case (getSubName s) of
>                                    --    Just n -> transformBiM (extendCalls fname n cenv) s
>                                    --    Nothing -> 
>                                                   transformBiM r s 
>                                                    where r :: ProgUnit A -> (Report, ProgUnit A)
>                                                          r p = case getSubName p of
>                                                                Just n -> transformBiM (extendCalls fname n cenv) p
>                                                                Nothing -> return p


> extendCalls :: String -> String -> [TLCommon A] -> Fortran A -> (Report, Fortran A)
> extendCalls fname localSub cenv f@(Call p sp v@(Var _ _ ((VarName _ n, _):_)) (ArgList ap arglist)) =
>         let commons = lookups n (map snd cenv)
>             targetCommonNames = map fst (sortBy cmpTConBNames commons)

>             localCommons = lookups localSub (lookups fname cenv)
>             localCommons' = sortBy cmpTConBNames localCommons

>             p' = p { refactored = Just $ toCol0 $ fst sp }
>             ap' = ap { refactored = Just $ fst sp } 

>             arglist' = toArgList p' sp (select targetCommonNames localCommons')
>             r = fname  ++ (show $ srcLineCol $ fst sp) ++ ": call, added common variables as parameters\n"
>         in (r, Call p' sp v (ArgList ap' $ ESeq p' sp arglist arglist'))
>         
>       --       Nothing -> error "Source has less commons than the target!"
> extendCalls _ _ _ f = return f
>                                       

> toArgList :: A -> SrcSpan -> [(Variable, Type A)] -> Expr A
> toArgList p sp [] = NullExpr p sp
> toArgList p sp ((v, _):xs) = ESeq p sp (Var p sp [(VarName p v, [])]) (toArgList p sp xs)

> select :: [Maybe String] -> [TCommon A] -> [(Variable, Type A)]
> select [] _ = []
> select x [] = error $ "Source has less commons than the target!" ++ show x
> select a@(x:xs) b@((y, e):yes) | x == y = e ++ select xs yes
>                                | otherwise = select xs yes
> 

> extendArgs nonNullArgs sp' args = if nonNullArgs then 
>                                      let p' = unitAnnotation { refactored = Just $ snd sp' }
>                                      in ASeq p' (ArgName p' "") (extendArgs' sp' args)
>                                   else extendArgs' sp' args
>                                  

> extendArgs'  _ [] = NullArg unitAnnotation
> extendArgs' sp' ((v, t):vts) = 
>     let p' = unitAnnotation { refactored = Just $ fst sp' }
>     in ASeq p' (ArgName p' v) (extendArgs' sp' vts)

 blockExtendDecls (Block a s i sp ds f) ds' = Block a s i sp (DSeq unitAnnotation ds ds') f
              
 extendArgs _ [] = (NullDecl unitAnnotation, NullArg unitAnnotation)
 extendArgs sp' ((v, t):vts) = 
     let p' = unitAnnotation { refactored = Just $ toCol0 $ fst sp' }
         dec = Decl p' [(Var p' sp' [(VarName p' v, [])], NullExpr p' sp')] t
         arg = ArgName p' v
         (decs, args) = extendArgs sp' vts
     in (DSeq p' dec decs, ASeq p' arg args)

> cmpTLConFName :: TLCommon A -> TLCommon A -> Ordering
> cmpTLConFName (f1, (_, _)) (f2, (_, _)) = compare f1 f2

> cmpTLConPName :: TLCommon A -> TLCommon A -> Ordering
> cmpTLConPName (_, (p1, _)) (_, (p2, _)) = compare p1 p2

> cmpTLConBNames :: TLCommon A -> TLCommon A -> Ordering
> cmpTLConBNames (_, (_, c1)) (_, (_, c2)) = cmpTConBNames c1 c2

> cmpTConBNames :: TCommon A -> TCommon A -> Ordering
> cmpTConBNames (Nothing, _) (Nothing, _) = EQ
> cmpTConBNames (Nothing, _) (Just _, _)  = LT
> cmpTConBNames (Just _, _) (Nothing, _)  = GT
> cmpTConBNames (Just n, _) (Just n', _) = if (n < n') then LT
>                                  else if (n > n') then GT else EQ


{-
 collectTCommons :: [Program Annotation] -> State (TCommons Annotation) [Program Annotation]
 collectTCommons p = transformBiM collectTCommons' p    
(transformBiM collectTCommons)
-}


> collectCommons :: Filename -> String -> Block A -> State (Report, [TLCommon A]) (Block A)
> collectCommons fname pname b = 
>     let tenv = typeEnv b
>                     
>         commons' :: Decl A -> State (Report, [TLCommon A]) (Decl A)
>         commons' f@(Common a sp cname exprs) = 
>             do let r' = fname ++ (show $ srcLineCol $ fst sp) ++ ": removed common declaration\n"
>                (r, env) <- get
>                put (r ++ r', (fname, (pname, (cname, typeCommonExprs exprs))):env)
>                return $ (NullDecl (a { refactored = (Just $ fst sp) }) sp)
>         commons' f = return f

>         typeCommonExprs :: [Expr Annotation] -> [(Variable, Type Annotation)]
>         typeCommonExprs [] = []
>         typeCommonExprs ((Var _ sp [(VarName _ v, _)]):es) = 
>             case (tenvLookup v tenv) of
>                  Just t -> (v, t) : (typeCommonExprs es)
>                  Nothing -> error $ "Variable " ++ (show v) ++ " is of an unknown type at: " ++ show sp
>         typeCommonExprs (e:_) = error $ "Not expecting a non-variable expression in expression at: " ++ show (srcSpan e)

>     in transformBiM commons' b                           

> definitionSites :: [(Filename, Program A)] -> State (Report, [TLCommon A]) [(Filename, Program A)] 
> definitionSites pss = let 
>                           defs' :: Filename -> ProgUnit A -> State (Report, [TLCommon A]) (ProgUnit A)
>                           defs' fname p = case (getSubName p) of
>                                             Just pname -> transformBiM (collectCommons fname pname) p
>                                             Nothing -> case p of 
>                                                          IncludeProg a sp ds -> 
>                                                             -- ("doing an include: " ++ (show fname)) `trace`
>                                                             let -- create dummy block
>                                                                 a0 = unitAnnotation
>                                                                 b = Block a (UseBlock (UseNil a0) nullLoc) 
>                                                                             (ImplicitNull a0) sp ds
>                                                                             (NullStmt a0 nullSpan)
>                                                              in do (Block _ _ _ _ ds' _) <- transformBiM (collectCommons fname fname) b
>                                                                    return $ IncludeProg a sp ds'
>                                                          otherwise -> return p 


>                           -- defs' f (Sub _ _ _ (SubName _ n) _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons' b) []))]
>                           -- Don't support functions yet
>                           -- defs' f (Function _ _ _ (SubName _ n) _ _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons b) []))]
>                           -- defs' _ _ rs = concat rs

>                       in mapM (\(f, ps) -> do ps' <- mapM (transformBiM (defs' f)) ps
>                                               return (f, ps')) pss

{-
-- Turn common blocks into type defs

 commonToTypeDefs :: String -> [(String, [Program Annotation])] -> IO Report
 commonToTypeDefs d = 
     let name = d ++ "Types"
         unitSrcLoc = SrcLoc (name ++ ".f90") 0 0
         decls = undefined
         mod = Module () (unitSrcLoc, unitSrcLoc) (SubName () name) [] ImplicitNode decls []
     in let ?variant = Alt1 in writeFile (d ++ "/" ++ name ++ ".f90") (outputF mod)

 
 commonToTypeDefs' :: String -> (String, [Program Annotation]) -> [Decls]
 commonToTypeDefs' = undefined -- DerivedTypeDef p 
-}
