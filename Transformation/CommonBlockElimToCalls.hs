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
module Transformation.CommonBlockElimToCalls where

import Control.Monad
import Control.Monad.State.Lazy

import Data.Generics.Uniplate.Operations

import Data.List

import Language.Fortran
import Language.Haskell.Syntax (SrcLoc(..))

import Helpers
import Traverse
import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types
import Transformation.Syntax
import Transformation.CommonBlockElim

{- This is somewhat experimental and incomplete -}

-- Top-level functions for eliminating common blocks in a set of files
commonElimToCalls :: Directory -> [(Filename, Program A)] -> (Report, [(Filename, Program A)])

-- Eliminates common blocks in a program directory (and convert to calls)
commonElimToCalls d ps = let (ps', (r, cg)) = runState (analyseCommons ps) ("", [])
                             (r', ps'') = mapM (introduceCalls cg) ps'
                         in (r ++ r', ps'')

{-Extending calls version-}
introduceCalls :: [TLCommon A] -> (Filename, Program A) -> (Report, (Filename, Program A))
introduceCalls cenv (fname, ps) = do ps' <- mapM (transformBiM commonElim) ps
                                     -- ps'' <- mapM (transformBiM commonElim'') ps'
                                     return (fname, ps')

              where commonElim s@(Sub a sp mbt (SubName a' moduleName) (Arg p arg asp) b) = 
                        
                         let commons = lookups moduleName (lookups fname cenv) 
                             sortedC = sortBy cmpTConBNames commons
                             tArgs = extendArgs (nonNullArgs arg) asp (concatMap snd sortedC)
                             ra = p { refactored = Just (fst sp) }
                             arg' = Arg unitAnnotation (ASeq unitAnnotation arg tArgs) asp
                             a' = a -- { pRefactored = Just sp }
                             r = fname ++ (show $ srcLineCol $ snd asp) ++ ": changed common variables to parameters\n"
                         in do b' <- transformBiM (extendCalls fname moduleName cenv) b
                               (r, Sub a' sp mbt (SubName a' moduleName) arg' b')

                    commonElim s = --case (getSubName s) of
                                   --    Just n -> transformBiM (extendCalls fname n cenv) s
                                   --    Nothing -> 
                                                  transformBiM r s 
                                                    where r :: ProgUnit A -> (Report, ProgUnit A)
                                                          r p = case getSubName p of
                                                                  Just n -> transformBiM (extendCalls fname n cenv) p
                                                                  Nothing -> return p


extendCalls :: String -> String -> [TLCommon A] -> Fortran A -> (Report, Fortran A)
extendCalls fname localSub cenv f@(Call p sp v@(Var _ _ ((VarName _ n, _):_)) (ArgList ap arglist)) =
        let commons = lookups n (map snd cenv)
            targetCommonNames = map fst (sortBy cmpTConBNames commons)

            localCommons = lookups localSub (lookups fname cenv)
            localCommons' = sortBy cmpTConBNames localCommons

            p' = p { refactored = Just $ toCol0 $ fst sp }
            ap' = ap { refactored = Just $ fst sp } 

            arglist' = toArgList p' sp (select targetCommonNames localCommons')
            r = fname  ++ (show $ srcLineCol $ fst sp) ++ ": call, added common variables as parameters\n"
        in (r, Call p' sp v (ArgList ap' $ ESeq p' sp arglist arglist'))
        
      --       Nothing -> error "Source has less commons than the target!"
extendCalls _ _ _ f = return f
                                      

toArgList :: A -> SrcSpan -> [(Variable, Type A)] -> Expr A
toArgList p sp [] = NullExpr p sp
toArgList p sp ((v, _):xs) = ESeq p sp (Var p sp [(VarName p v, [])]) (toArgList p sp xs)

select :: [Maybe String] -> [TCommon A] -> [(Variable, Type A)]
select [] _ = []
select x [] = error $ "Source has less commons than the target!" ++ show x
select a@(x:xs) b@((y, e):yes) | x == y = e ++ select xs yes
                               | otherwise = select xs yes

nonNullArgs (ASeq _ _ _) = True
nonNullArgs (ArgName _ _) = True
nonNullArgs (NullArg _) = False


extendArgs nonNullArgs sp' args = if nonNullArgs then 
                                     let p' = unitAnnotation { refactored = Just $ snd sp' }
                                     in ASeq p' (ArgName p' "") (extendArgs' sp' args)
                                  else extendArgs' sp' args
                                 

extendArgs'  _ [] = NullArg unitAnnotation
extendArgs' sp' ((v, t):vts) = 
    let p' = unitAnnotation { refactored = Just $ fst sp' }
    in ASeq p' (ArgName p' v) (extendArgs' sp' vts)

{- blockExtendDecls (Block a s i sp ds f) ds' = Block a s i sp (DSeq unitAnnotation ds ds') f
              
 extendArgs _ [] = (NullDecl unitAnnotation, NullArg unitAnnotation)
 extendArgs sp' ((v, t):vts) = 
     let p' = unitAnnotation { refactored = Just $ toCol0 $ fst sp' }
         dec = Decl p' [(Var p' sp' [(VarName p' v, [])], NullExpr p' sp')] t
         arg = ArgName p' v
         (decs, args) = extendArgs sp' vts
     in (DSeq p' dec decs, ASeq p' arg args)
-}




{-
 collectTCommons :: [Program Annotation] -> State (TCommons Annotation) [Program Annotation]
 collectTCommons p = transformBiM collectTCommons' p    
(transformBiM collectTCommons)
-}


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

{-
-- Turn common blocks into type defs

 commonToTypeDefs :: String -> [(String, [Program Annotation])] -> IO Report
 commonToTypeDefs d = 
     let name = d ++ "Types"
         unitSrcLoc = SrcLoc (name ++ ".f90") 0 0
         decls = undefined
         mod = Module () (unitSrcLoc, unitSrcLoc) (SubName () name) [] ImplicitNode decls []
     in let ?variant = DefaultPP in writeFile (d ++ "/" ++ name ++ ".f90") (outputF mod)

 
 commonToTypeDefs' :: String -> (String, [Program Annotation]) -> [Decls]
 commonToTypeDefs' = undefined -- DerivedTypeDef p 
-}
