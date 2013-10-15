> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module Transformation.CommonBlockElim where

> import Data.Data
> import Data.List
> import Data.Ord
> import qualified Data.Map as Data.Map

> import Language.Fortran
> import Language.Fortran.Pretty

> import Language.Haskell.Syntax (SrcLoc(..))

> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy
> import Debug.Trace

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
>                                (r', ps'') = introduceModules d (zip [0..] cg)
>                            in (r ++ r', ps' ++ ps'') -- ++ [("testNew", snd $ head ps)])

> nonNullArgs (ASeq _ _ _) = True
> nonNullArgs (ArgName _ _) = True
> nonNullArgs (NullArg _) = False


-- Fold [TLCommon p] to get a list of ([(TLCommon p, Renamer p)], [(Filename, Program A)])
-- How to decide which gets to be the "head" perhaps the one which triggers the *least* renaming (ooh!)
--  (this is calculated by looking for the mode of the TLCommon (for a particular Common)
--  (need to do gorouping, but sortBy is used already so... (IS THIS STABLE- does this matter?))


> fff commons = let gc = groupBy (\x y -> cmpEq $ cmpTC x y) commons -- groups by name the commons
>                   
>               in undefined
>                   

> generatRenamer :: TCommon A -> TCommon A -> Renamer
> generatRenamer = undefined -- ((n1, vtys1) : xs) ((n2, vtys2) : ys) = undefined

> coherentlyTypedCommons :: TLCommon A -> TLCommon A -> (Report, Bool)
> coherentlyTypedCommons (f1, (p1, (n1, vtys1))) (f2, (p2, (n2, vtys2))) =
>     case (n1 == n2) of
>       True -> coherent vtys1 vtys2 where
>                   coherent ::  [(Variable, Type p)] -> [(Variable, Type p)] -> (Report, Bool)
>                   coherent ((var1, ty1):xs) ((var2, ty2):ys) 
>                       | af ty1 == af ty2 = let (r', c) = coherent xs ys
>                                            in (r', c && True)
>                       | otherwise = let ?variant = Alt1 in
>                                     let r = (var1 ++ ":" ++ (outputF ty1) ++ " differs from " ++ 
>                                              var2 ++ ":" ++ (outputF ty2))
>                                         (r', _) = coherent xs ys
>                                     in (r ++ r', False)
>       False -> error "Trying to compare different common blocks\n"
>                                                            


> introduceModules :: Directory -> [(Int, TLCommon A)] -> (Report, [(Filename, Program A)]) 
> introduceModules d cenv = mapM (mkModuleFile d) cenv

 introduceModules :: [TLCommon A] -> (Filename, Program A) -> (Report, [(Filename, Program A)]) -- last part is new modules
 introduceModules cenv (fname, ps) = let (r, ps') = mapM (transformBiM commonElim) ps
                                        in (r, [(fname, ps')])

       where commonElim s@(Sub a sp mbt (SubName a' moduleName)(Arg p parg asp) b) =  -- 

                 let commons = lookups moduleName (lookups fname cenv)
                     sortedC = sortBy cmpTC commons
                     ra = p { refactored = Just (fst sp) }
                     r = fname ++ (show $ srcLineCol $ fst sp) ++ ": extracted common variables into module \n"
                 in (r, s)

> mkModuleFile :: Directory -> (Int, TLCommon A) -> (Report, (Filename, Program A))
> mkModuleFile d (n, (_, (_, (name, varTys)))) =
>         let modname = case name of Nothing -> "Common" ++ show n 
>                                    Just x  -> x
>             fullpath = d ++ "/" ++ modname ++ ".f"
>             r = "Created module " ++ modname ++ " at " ++ fullpath ++ "\n"
>         in (r, (fullpath, [mkModule varTys modname]))

> mkModule :: [(Variable, Type A)] -> String -> ProgUnit A
> mkModule vtys fname = let a = unitAnnotation { refactored = Just loc }
>                           loc = SrcLoc (fname ++ ".f") 0 0 
>                           sp = (loc, loc)
>                           toDecl (v, t) = Decl a sp [(Var a sp [(VarName a v, [])], NullExpr a sp)] -- note here could pull in initialising definition? What if conflicts- highlight as potential source of error?
>                                                             t
>                           decls = foldl1 (DSeq a) (map toDecl vtys)
>                       in Module a (loc, loc) (SubName a fname) [] (ImplicitNone a) decls []

Extending calls version


> introduceCalls :: [TLCommon A] -> (Filename, Program A) -> (Report, (Filename, Program A))
> introduceCalls cenv (fname, ps) = do ps' <- mapM (transformBiM commonElim) ps
>                                      return (fname, ps')

>               where commonElim s@(Sub a sp mbt (SubName a' moduleName) (Arg p arg asp) b) = 
>                         
>                          let commons = lookups moduleName (lookups fname cenv) 
>                              sortedC = sortBy cmpTC commons
>                              tArgs = extendArgs (nonNullArgs arg) asp (concatMap snd sortedC)
>                              ra = p { refactored = Just (fst sp) }
>                              arg' = Arg unitAnnotation (ASeq unitAnnotation arg tArgs) asp
>                              a' = a -- { pRefactored = Just sp }
>                              r = fname ++ (show $ srcLineCol $ fst sp) ++ ": changed common variables to parameters\n"
>                          in do b' <- transformBiM (extendCalls fname moduleName cenv) b
>                                (r, Sub a' sp mbt (SubName a' moduleName) arg' b')
>
>                                                 -- b' = blockExtendDecls b tDecls
>                     commonElim s = return s

>                             --  Nothing -> transformBi (extendCalls fname n cenv) s
>                     commonElim'' s = case (getSubName s) of
>                                        Just n -> transformBiM (extendCalls fname n cenv) s
>                                        Nothing -> transformBiM r s 
>                                                    where r :: ProgUnit A -> (Report, ProgUnit A)
>                                                          r p = case getSubName p of
>                                                                Just n -> transformBiM (extendCalls fname n cenv) p
>                                                                Nothing -> return p


> extendCalls :: String -> String -> [TLCommon A] -> Fortran A -> (Report, Fortran A)
> extendCalls fname localSub cenv f@(Call p sp v@(Var _ _ ((VarName _ n, _):_)) (ArgList ap arglist)) =
>         let commons = lookups n (map snd cenv)
>             targetCommonNames = map fst (sortBy cmpTC commons)

>             localCommons = lookups localSub (lookups fname cenv)
>             localCommons' = sortBy cmpTC localCommons

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
>                            | otherwise = select xs yes
> 

> extendArgs nonNullArgs sp' args = if nonNullArgs then 
>                                      let p' = unitAnnotation { refactored = Just $ fst sp' }
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

> cmpTC :: TCommon A -> TCommon A -> Ordering
> cmpTC (Nothing, _) (Nothing, _) = EQ
> cmpTC (Nothing, _) (Just _, _)  = LT
> cmpTC (Just _, _) (Nothing, _)  = GT
> cmpTC (Just n, _) (Just n', _) = if (n < n') then LT
>                                  else if (n > n') then GT else EQ


 collectTCommons :: [Program Annotation] -> State (TCommons Annotation) [Program Annotation]
 collectTCommons p = transformBiM collectTCommons' p    

(transformBiM collectTCommons)



> collectCommons :: String -> String -> (Block A) -> State (Report, [TLCommon A]) (Block A)
> collectCommons fname n b = 
>     let tenv = typeEnv b
>                     
>         commons' :: Decl A -> State (Report, [TLCommon A]) (Decl A)
>         commons' f@(Common a sp name exprs) = 
>             do let r' = fname ++ (show $ srcLineCol $ fst sp) ++ ": removed common declaration\n"
>                (r, env) <- get
>                put (r ++ r', (fname, (n, (name, typeCommonExprs exprs))):env)
>                return $ (NullDecl (a { refactored = (Just $ fst sp) }) sp)
>         commons' f = return f

>         typeCommonExprs :: [Expr Annotation] -> [(Variable, Type Annotation)]
>         typeCommonExprs [] = []
>         typeCommonExprs ((Var _ sp [(VarName _ v, _)]):es) = 
>             case (lookup v tenv) of
>                  Just t -> (v, t) : (typeCommonExprs es)
>                  Nothing -> error $ "Variable is of an unknown type at: " ++ show sp
>         typeCommonExprs (e:_) = error $ "Not expecting a non-variable expression in expression at: " ++ show (getSpan e)

>     in transformBiM commons' b                           

> definitionSites :: [(String, Program A)] -> State (Report, [TLCommon A]) [(String, Program A)] 
> definitionSites pss = let 
>                           defs' :: Filename -> ProgUnit A -> State (Report, [TLCommon A]) (ProgUnit A)
>                           defs' f p = case (getSubName p) of
>                                            Just n -> transformBiM (collectCommons f n) p
>                                            Nothing -> return $ p

>                           -- defs' f (Sub _ _ _ (SubName _ n) _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons' b) []))]
>                           -- Don't support functions yet
>                           -- defs' f (Function _ _ _ (SubName _ n) _ b) rs = (concat rs) ++ [(f, (n, snd $ runState (collectTCommons b) []))]
>                           -- defs' _ _ rs = concat rs

>                       in mapM (\(f, ps) -> do ps' <- mapM (transformBiM (defs' f)) ps
>                                               return (f, ps')) pss

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
