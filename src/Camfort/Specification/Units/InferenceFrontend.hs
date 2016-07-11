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
{-

  Units of measure extension to Fortran

TODO:
 * Deal with variable shadowing in "contained" functions.
 * Better errors with line number info

-}


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse #-}

module Camfort.Specification.Units.InferenceFrontend where

import Data.Data
import Data.Char
import Data.Function
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Ratio
import Data.Generics.Uniplate.Operations
import Data.Label.Monadic hiding (modify)
import Control.Monad.State.Strict hiding (gets)
import Control.Monad

import Language.Fortran
import Language.Fortran.Pretty

import Camfort.Analysis.Annotations hiding (Unitless)
import Camfort.Analysis.Types
import Camfort.Specification.Units.Debug
import Camfort.Specification.Units.Environment
import Camfort.Specification.Units.InferenceBackend
import Camfort.Specification.Units.Solve
import Camfort.Specification.Units.SyntaxConversion
import Camfort.Transformation.Syntax

-- Core procedure for inferring units
doInferUnits ::
       (?criticals :: Bool, ?solver :: Solver, ?debug :: Bool,
        ?assumeLiterals :: AssumeLiterals)
    => Program Annotation -> State UnitEnv (Program Annotation)
doInferUnits x = do
    mapM perProgInfer x
    --
    ifDebug (report <<++ "Finished inferring prog units")
    ifDebug debugGaussian
    --
    inferInterproceduralUnits x
    return x

-- For a program unit, infer its units
perProgInfer ::
       (?criticals :: Bool, ?solver :: Solver, ?debug :: Bool,
        ?assumeLiterals :: AssumeLiterals)
    => ProgUnit Annotation -> State UnitEnv ()
perProgInfer p =
  do -- infer units for *root* program unit
     inferPUnit p
     -- infer units for the *children* program units
     -- (so that the parent scope is processed first)
     mapM_ perProgInfer $ ((children p)::[ProgUnit Annotation])

  where
    -- Infer the units for the *root* program unit (not children)
     inferPUnit :: ProgUnit Annotation -> State UnitEnv ()
     inferPUnit (Main x sp n a b _) =
                perBlockInfer b Nothing

     inferPUnit (Sub x sp t (SubName _ n) (Arg _ a _) b) =
                perBlockInfer b (Just (n, Nothing, argNames a))

     inferPUnit (Function _ _ _ (SubName _ n) (Arg _ a _) r b) =
                perBlockInfer b (Just (n, Just (resultName n r), argNames a))

     inferPUnit (Module x sp (SubName _ n) _ _ d ds) =
                transformBiM (inferDecl (Just (n, Nothing, []))) d >> return ()

     inferPUnit x = return ()

     argNames :: ArgName a -> [Variable]
     argNames (ArgName _ n) = [n]
     argNames (ASeq _ n1 n2) = argNames n1 ++ argNames n2
     argNames (NullArg _) = []

     resultName :: Variable -> Maybe (VarName a) -> Variable
     resultName n Nothing = n
     resultName _ (Just (VarName _ r)) = r

perBlockInfer :: (?solver :: Solver, ?criticals :: Bool, ?debug :: Bool,
                  ?assumeLiterals :: AssumeLiterals)
    => Block Annotation -> Maybe ProcedureNames -> State UnitEnv ()
perBlockInfer x proc = do
    resetTemps
    enterDecls x proc
    addProcedure proc
    descendBiM handleStmt x

    case proc of
        Just _ -> do {- not ?criticals -> -}
          -- Intermediate solve for procedures (subroutines & functions)
          ifDebug (report <<++ "Pre doing row reduce")
          consistent <- solveSystemM ""
          success =: consistent

          linearSystem =. reduceRows 1
          ifDebug (report <<++ "Post doing reduce")
          ifDebug (debugGaussian)
          return ()
        _ -> return ()
  where
    handleStmt :: Fortran Annotation -> State UnitEnv (Fortran Annotation)
    handleStmt x = do inferStmtUnits x
                      return x

{-| reduceRows is a core part of the polymorphic unit checking for procedures.

               It is essentially an "optimisation" of the Gaussian matrix (not in the sense of performance),
               that elimiantes rows in the system such that there are as few variables as possible. Within
               a function, assuming everything is consistent, then this should generate a linear constraint
               between the parameters and the return as a single row in the matrix. This is then used by the
               interprocedural constraints to hookup call-sites with definitions (in a parametrically polymorphic
               way- i.e. lambda abstraction is polymorphic in its units, different to say ML).
-}
reduceRows :: Col -> LinearSystem -> LinearSystem
reduceRows m (matrix, vector)
    | m > ncols matrix = (matrix, vector)
    | otherwise = case (find (\n -> matrix ! (n, m) /= 0) [1..nrows matrix]) of
                    Just r1 -> case (find (\n -> matrix ! (n, m) /= 0) [(r1 + 1)..nrows matrix]) of
                                 Just r2 -> -- Found two rows with non-zero coeffecicients in this column
                                            case (elimRow (matrix, vector) (Just r1) m r2) of
                                                 -- Eliminate the row and cut the system down
                                                 Ok (matrix', vector') -> reduceRows m (cutSystem r2 (matrix', vector'))
                                                 Bad _ _ _             -> reduceRows (m+1) (matrix, vector)

                                 Nothing -> -- If there are no two rows with non-zero coeffecieints in colum m
                                            -- then move onto the next column
                                            reduceRows (m+1) (matrix, vector)
                    Nothing -> reduceRows (m+1) (matrix, vector)


addProcedure :: Maybe ProcedureNames -> State UnitEnv ()
addProcedure Nothing = return ()
addProcedure (Just (name, resultName, argNames)) =
      do uenv <- gets varColEnv
         resultVar <- case resultName of
                        Just rname -> case (lookupWithoutSrcSpan rname uenv) of
                                        Just (uvar, _) -> return $ Just uvar
                                        Nothing        -> do m <- addCol Variable
                                                             return $ Just (VarCol m)
                        Nothing    -> return Nothing
         let argVars = fmap (lookupUnitByName uenv) argNames
         procedureEnv << (name, (resultVar, argVars))
        where
          lookupUnitByName uenv v = maybe (VarCol 1) fst $ lookupWithoutSrcSpan v uenv

-- ***************************************
--
-- *  Unit inference (main, over all AST)
--
-- ***************************************

enterDecls :: (?assumeLiterals :: AssumeLiterals) => Block Annotation -> Maybe ProcedureNames -> State UnitEnv (Block Annotation)
enterDecls x proc = transformBiM (inferDecl proc) x

processVar :: (?assumeLiterals :: AssumeLiterals) =>
              [UnitConstant] -> Maybe ProcedureNames ->
              (Expr Annotation, Expr Annotation) -> Type Annotation ->
              State UnitEnv (Expr Annotation, Expr Annotation)
processVar units proc exps@(Var a s names, e) typ =
    do
       let (VarName _ v, es) = head names
       system <- gets linearSystem
       let m = ncols (fst system) + 1
       unitVarCats <<++ (unitVarCat v proc)
       extendConstraints units
       ms <- case toArrayType typ es of
               ArrayT _ bounds _ _ _ _ -> mapM (const $ fmap VarCol $ addCol Variable) bounds
               _                       -> return []
       varColEnv << (VarBinder (v, s), (VarCol m, ms))

       uv <- gets varColEnv
       -- If the declaration has a null expression, do not create a unifying variable
       case e of
              NullExpr _ _ -> return ()
              _            -> do uv <- inferExprUnits e
                                 mustEqual False (VarCol m) uv
                                 return ()
       return exps

unitVarCat :: Variable -> Maybe ProcedureNames -> UnitVarCategory
unitVarCat v proc | Just (n, r, args) <- proc, v `elem` args = Argument
                  | otherwise                                = Variable


{-| inferDecl - extract and record information from explicit unit declarations -}
inferDecl :: (?assumeLiterals :: AssumeLiterals) => Maybe ProcedureNames -> Decl Annotation -> State UnitEnv (Decl Annotation)
inferDecl proc decl@(Decl a s d typ) =
      do let BaseType _ _ attrs _ _ = arrayElementType typ
         units <- sequence $ concatMap extractUnit attrs
         mapM_ (\(e1, e2, multiplier) -> processVar units proc (e1, e2) typ) d
         return $ decl

inferDecl proc x@(MeasureUnitDef a s d) =
   do mapM learnDerivedUnit d
      return x
     where
        learnDerivedUnit (name, spec) =
          do denv <- gets derivedUnitEnv
             when (isJust $ lookup name denv) $ error "Redeclared unit of measure"
             unit <- convertUnit spec
             denv <- gets derivedUnitEnv
             when (isJust $ lookup name denv) $ error "Recursive unit-of-measure definition"
             derivedUnitEnv << (name, unit)
inferDecl _ x = return x

extendConstraints :: [UnitConstant] -> State UnitEnv ()
extendConstraints units =
        do (matrix, vector) <- gets linearSystem
           let n = nrows matrix + 1
               m = ncols matrix + 1
           linearSystem =: case units of
                             [] -> do (extendTo 0 0 m matrix, vector)
                             _ -> (setElem 1 (n, m) $ extendTo 0 n m matrix, vector ++ [last units])
           tmpColsAdded << m
           tmpRowsAdded << n
           return ()

inferInterproceduralUnits :: (?solver :: Solver, ?criticals :: Bool, ?debug :: Bool, ?assumeLiterals :: AssumeLiterals) => Program Annotation -> State UnitEnv ()
inferInterproceduralUnits x =
  do --reorderColumns
     if ?criticals then reorderVarCols else return ()
     consistent <- solveSystemM "inconsistent"
     if consistent then
         do system <- gets linearSystem
            let dontAssumeLiterals = case ?assumeLiterals of
                                       Poly     -> True
                                       Unitless -> False
                                       Mixed    -> False
            inferInterproceduralUnits' x dontAssumeLiterals system -- edited
            return ()
     else
         return ()

inferInterproceduralUnits' :: (?solver :: Solver, ?criticals :: Bool, ?debug :: Bool) => Program Annotation -> Bool -> LinearSystem -> State UnitEnv (Program Annotation)
inferInterproceduralUnits' x haveAssumedLiterals system1 =
  do addInterproceduralConstraints x
     consistent <- solveSystemM "inconsistent"
     if not consistent then
          do  linearSystem =: system1
              return x
      else do
        system2 <- gets linearSystem
        if system1 == system2
          then if ?criticals then nextStep else checkUnderdeterminedM >> nextStep
          else inferInterproceduralUnits' x haveAssumedLiterals system2
  where nextStep | haveAssumedLiterals = return x
                 | otherwise           = do consistent <- assumeLiteralUnits
                                            if not consistent
                                             then return x
                                             else do system3 <- gets linearSystem
                                                     inferInterproceduralUnits' x True system3


class UpdateColInfo t where
    updateColInfo :: Col -> Col -> t -> t

instance UpdateColInfo VarCol where
    updateColInfo x n (VarCol y) | y == x = VarCol n
                                       | y == n = VarCol x
                                       | otherwise = VarCol y

instance UpdateColInfo VarColEnv where
    updateColInfo _ _ [] = []
    updateColInfo x n ((v, (uv, uvs)):ys) = (v, (updateColInfo x n uv, map (updateColInfo x n) uvs)) : (updateColInfo x n ys)

instance UpdateColInfo Procedure where
    updateColInfo x n (Nothing, ps) = (Nothing, map (updateColInfo x n) ps)
    updateColInfo x n (Just p, ps) = (Just $ updateColInfo x n p, map (updateColInfo x n) ps)

instance UpdateColInfo ProcedureEnv where
    updateColInfo x n = map (\(s, p) -> (s, updateColInfo x n p))

instance UpdateColInfo (Int, a) where
    updateColInfo x n (y, s) | y == x = (n, s)
                             | y == n = (x, s)
                             | otherwise = (y, s)

instance UpdateColInfo Int where
    updateColInfo x n y | y == x = x
                        | y == n = n
                        | otherwise = y

swapUnitVarCats x n xs = swapUnitVarCats' x n xs xs 1
swapUnitVarCats' x n [] ys c = []
swapUnitVarCats' x n (z:zs) ys c | c == x = (ys !! (n - 1)) : (swapUnitVarCats' x n zs ys (c + 1))
                                 | c == n = (ys !! (x - 1)) : (swapUnitVarCats' x n zs ys (c + 1))
                                 | otherwise = z : (swapUnitVarCats' x n zs ys (c + 1))

swapCols :: Int -> Int -> State UnitEnv ()
swapCols x n = do --report <<++ ("Pre swap - " ++ (show x) ++ " <-> " ++ (show n))
                  --debugGaussian
                  varColEnv   =. updateColInfo x n
                  procedureEnv =. updateColInfo x n
                  calls        =. updateColInfo x n
                  unitVarCats  =. swapUnitVarCats x n
                  linearSystem =. (\(m, v) -> (switchCols x n m, v))
                  debugInfo    =. map (updateColInfo x n)
                  tmpColsAdded =. map (updateColInfo x n)
                  --report <<++ "Post swap"
                  --debugGaussian
                  return ()


{-| reorderVarCols puts any variable columns to the end of the Gaussian matrix (along with the associated information) -}
reorderVarCols :: State UnitEnv ()
reorderVarCols = do ucats <- gets unitVarCats
                    (matrix, _) <- gets linearSystem
                    reorderVarCols' (ncols matrix) 1
                   where   correctEnd :: Int -> State UnitEnv Int
                           correctEnd 0   = return 0
                           correctEnd end = do ucats <- gets unitVarCats
                                               case (ucats !! (end - 1)) of
                                                  Variable -> correctEnd (end - 1)
                                                  _        -> return $ end

                           reorderVarCols' :: Int -> Int -> State UnitEnv ()
                           reorderVarCols' end c | c >= end = return ()
                           reorderVarCols' end c = do ucats <- gets unitVarCats
                                                      case (ucats !! (c - 1)) of
                                                        Variable -> do end' <- correctEnd end
                                                                       swapCols end' c
                                                                       reorderVarCols' (end' - 1) (c+1)
                                                        _        -> reorderVarCols' end (c+1)

assumeLiteralUnits :: (?solver :: Solver, ?debug :: Bool) => State UnitEnv Bool
assumeLiteralUnits =
  do system@(matrix, vector) <- gets linearSystem
     mapM_ assumeLiteralUnits' [1 .. ncols matrix]
     consistent <- solveSystemM "underdetermined"
     when (not consistent) $ linearSystem =: system
     return consistent

assumeLiteralUnits' m =
      do (matrix, vector) <- gets linearSystem
         ucats <- gets unitVarCats
         let n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
             m' = n >>= (\n -> find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix])
             nonLiteral n m = matrix ! (n, m) /= 0 && ucats !! (m - 1) /= (Literal True)
             m's = n >>= (\n -> find (nonLiteral n) [1 .. ncols matrix])
         when (ucats !! (m - 1) == (Literal True) && (m' /= Just m || isJust m's)) $ do
           n' <- addRow
           modify $ liftUnitEnv $ setElem 1 (n', m)

addInterproceduralConstraints :: (?debug :: Bool) => Program Annotation -> State UnitEnv ()
addInterproceduralConstraints x =
  do
    cs <- gets calls
    mapM_ addCall cs
  where
    addCall (name, (result, args)) =
      do penv <- gets procedureEnv
         case lookup name penv of
           Just (r, as) ->
                             let (r1, r2) = decodeResult result r
                             in handleArgs (args ++ r1) (as ++ r2)
           Nothing      -> return ()

    handleArgs actualVars dummyVars =
      do order <- gets reorderedCols
         let actual = map (\(VarCol uv) -> uv) actualVars
             dummy = map (\(VarCol uv) -> uv) dummyVars
         mapM_ (handleArg $ zip dummy actual) dummy

    -- experimentation but now deprecated.
{-
    handleArgNew dummyToActual dummy =
        do grid0 <- debugGaussian'
           mapM (\(l, r) -> do n <- addRow
                               modify $ liftUnitEnv $ setElem 1 (n, l)
                               modify $ liftUnitEnv $ setElem (-1) (n, r)
                ) dummyToActual
           grid1 <- debugGaussian'
           if (grid0 == grid1) then
               return ()
           else
             do report <<++ "HANDLED AND DIFFERENT!"
                report <<++ ("\n" ++ grid0)
                report <<++ ("\n" ++ grid1)
                return ()-}

    -- TODO: this can be optimised
    handleArg dummyToActual dummy =
      do (matrix, vector) <- gets linearSystem
         --grid0 <- debugGaussian'
         ifDebug (debugGaussian)

         ifDebug (report <<++ ("hArg - " ++ show dummyToActual ++ "-" ++ show dummy))

         let -- find the first row with a non-zero column for the variable
             n = maybe 1 id $ find (\n -> matrix ! (n, dummy) /= 0) [1 .. nrows matrix]

             -- find the first non-zero column on the row just selected
             Just m = find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

         ifDebug (report <<++ ("n = " ++ show n ++ ", m = " ++ show m))

         if (m == dummy) then
           do  let -- Get list of columns with non-zero coefficients to the right of the focus
                   ms = filter (\m -> matrix ! (n, m) /= 0) [m .. ncols matrix]

                   -- Get the list of columns to which the non-zero coeffecients are paired by 'dummyToActual' relation.
                   m's = mapMaybe (flip lookup dummyToActual) ms
                   pairs = --if (length m's == 1) then -- i.e. there is not a direct relationship between variable and return
                           --    zip ms (repeat (head m's))
                           --else
                               (zip ms m's)

               ifDebug(report <<++ ("ms = " ++ show ms ++ ", m's' = " ++ show m's ++ ", their zip = " ++ show pairs ++ " dA = " ++ show dummyToActual))

               if (True) -- length m's == length ms)
                 then do { newRow <- addRow' $ vector !! (n - 1);
--                           mapM_ (handleArgPair matrix n newRow) pairs ; }
                           mapM_ (handleArgPair matrix n newRow) dummyToActual ; }
                 else return ()
         else
             return ()

    -- Copy the row
    handleArgPair matrix n newRow (m, m') = do modify $ liftUnitEnv $ setElem (matrix ! (n, m)) (newRow, m')

    decodeResult (Just r1) (Just r2) = ([r1], [r2])
    decodeResult Nothing Nothing = ([], [])
    decodeResult (Just _) Nothing = error "Subroutine used as a function!"
    decodeResult Nothing (Just _) = error "Function used as a subroutine!"


inferLiteral e = do uv@(VarCol uvn) <- anyUnits (Literal (?assumeLiterals /= Mixed))
                    debugInfo << (uvn, (srcSpan e, pprint e))
                    return uv


data BinOpKind = AddOp | MulOp | DivOp | PowerOp | LogicOp | RelOp
binOpKind :: BinOp a -> BinOpKind
binOpKind (Plus _)  = AddOp
binOpKind (Minus _) = AddOp
binOpKind (Mul _)   = MulOp
binOpKind (Div _)   = DivOp
binOpKind (Or _)    = LogicOp
binOpKind (And _)   = LogicOp
binOpKind (Concat _)= AddOp
binOpKind (Power _) = PowerOp
binOpKind (RelEQ _) = RelOp
binOpKind (RelNE _) = RelOp
binOpKind (RelLT _) = RelOp
binOpKind (RelLE _) = RelOp
binOpKind (RelGT _) = RelOp
binOpKind (RelGE _) = RelOp

(<**>) :: Maybe a -> Maybe a -> Maybe a
Nothing <**> x = x
(Just x) <**> y = (Just x)


inferExprUnits :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv VarCol
inferExprUnits e@(Con _ _ _)    = inferLiteral e
inferExprUnits e@(ConL _ _ _ _) = inferLiteral e
inferExprUnits e@(ConS _ _ _)   = inferLiteral e
inferExprUnits ve@(Var _ _ names) =
 do uenv <- gets varColEnv
    penv <- gets procedureEnv
    let (VarName _ v, args) = head names

    case lookupWithoutSrcSpan v uenv of
       -- array variable?
       Just (uv, uvs@(_:_)) -> inferArgUnits' uvs >> return uv
       -- function call?
       Nothing | not (null args) -> do case (lookup (map toUpper v) intrinsicsDict) of
                                          Just fun -> fun v
                                          Nothing  -> return () -- error $ "I don't know the intrinsic " ++ v -- return ()
                                       uv@(VarCol uvn) <- anyUnits Temporary
                                       debugInfo << (uvn, (srcSpan ve, pprint ve))
                                       uvs <- inferArgUnits
                                       let uvs' = justArgUnits args uvs
                                       calls << (v, (Just uv, uvs'))
                                       return uv
       -- scalar variable or external function call?
       Just (uv, []) -> inferArgUnits >> return uv
       -- default specifier
       _ | v == "*" -> inferLiteral ve
       -- just bad code
       x -> case lookupCaseInsensitive v penv of
              Just (Just uv, argUnits) ->
                   if (null args) then inferArgUnits' argUnits >> return uv
                   else  do uv <- anyUnits Temporary
                            uvs <- inferArgUnits
                            let uvs' = justArgUnits args uvs
                            calls << (v, (Just uv, uvs'))
                            return uv

              Nothing -> error $ "\n" ++ (showSrcFile . srcSpan $ ve) ++ ": undefined variable " ++ v ++ " at " ++ (showSrcSpan . srcSpan $ ve)
  where inferArgUnits = sequence [mapM inferExprUnits exprs | (_, exprs) <- names, not (nullExpr exprs)]
        inferArgUnits' uvs = sequence [(inferExprUnits expr) >>= (\uv' -> mustEqual True uv' uv) | ((_, exprs), uv) <- zip names uvs, expr <- exprs, not (nullExpr [expr])]

        nullExpr []                  = False
        nullExpr [NullExpr _ _]      = True
        nullExpr ((NullExpr _ _):xs) = nullExpr xs
        nullExpr _                   = False

        justArgUnits [NullExpr _ _] _ = []  -- zero-argument function call
        justArgUnits _ uvs = head uvs
inferExprUnits e@(Bin _ _ op e1 e2) = do uv1 <- inferExprUnits e1
                                         uv2 <- inferExprUnits e2
                                         (VarCol n) <- case binOpKind op of
                                                               AddOp   -> mustEqual True uv1 uv2
                                                               MulOp   -> mustAddUp uv1 uv2 1 1
                                                               DivOp   -> mustAddUp uv1 uv2 1 (-1)
                                                               PowerOp -> powerUnits uv1 e2
                                                               LogicOp -> mustEqual True uv1 uv2
                                                               RelOp   -> do mustEqual True uv1 uv2
                                                                             return $ VarCol 1
                                         debugInfo << (n, (srcSpan e, pprint e))
                                         return (VarCol n)
inferExprUnits (Unary _ _ _ e) = inferExprUnits e
inferExprUnits (CallExpr _ _ e1 (ArgList _ e2)) = do uv <- anyUnits Temporary
                                                     inferExprUnits e1
                                                     inferExprUnits e2
                                                     error "CallExpr not implemented"
                                                     return uv
-- inferExprUnits (NullExpr .... Shouldn't occur very often as adds unnnecessary cruft
inferExprUnits (NullExpr _ _) = anyUnits Temporary

inferExprUnits (Null _ _) = return $ VarCol 1
inferExprUnits (ESeq _ _ e1 e2) = do inferExprUnits e1
                                     inferExprUnits e2
                                     return $ error "ESeq units wanted"
inferExprUnits (Bound _ _ e1 e2) = do uv1 <- inferExprUnits e1
                                      uv2 <- inferExprUnits e2
                                      mustEqual False uv1 uv2
inferExprUnits (Sqrt _ _ e) = do uv <- inferExprUnits e
                                 sqrtUnits uv
inferExprUnits (ArrayCon _ _ (e:exprs)) =
  do uv <- inferExprUnits e
     mapM_ (\e' -> do { uv' <- inferExprUnits e'; mustEqual True uv uv'}) exprs
     return uv
inferExprUnits (AssgExpr _ _ _ e) = inferExprUnits e

inferExprSeqUnits :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv [VarCol]
inferExprSeqUnits (ESeq _ _ e1 e2) = liftM2 (++) (inferExprSeqUnits e1) (inferExprSeqUnits e2)
inferExprSeqUnits e = (:[]) `liftM` inferExprUnits e

handleExpr :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv (Expr Annotation)
handleExpr x = do inferExprUnits x
                  return x

inferForHeaderUnits :: (?assumeLiterals :: AssumeLiterals) => (Variable, Expr Annotation, Expr Annotation, Expr Annotation) -> State UnitEnv ()
inferForHeaderUnits (v, e1, e2, e3) =
  do uenv <- gets varColEnv
     case (lookupWithoutSrcSpan v uenv) of
       Just (uv, []) -> do uv1 <- inferExprUnits e1
                           mustEqual True uv uv1
                           uv2 <- inferExprUnits e2
                           mustEqual True uv uv2
                           uv3 <- inferExprUnits e3
                           mustEqual True uv uv3
                           return ()
       Nothing -> report <<++ "Ill-formed Fortran code. Variable '" ++ v ++ "' is not declared."

inferSpecUnits :: (?assumeLiterals :: AssumeLiterals) => [Spec Annotation] -> State UnitEnv ()
inferSpecUnits = mapM_ $ descendBiM handleExpr

{-| inferStmtUnits, does what it says on the tin -}
inferStmtUnits :: (?assumeLiterals :: AssumeLiterals) => Fortran Annotation -> State UnitEnv ()
inferStmtUnits e@(Assg _ _ e1 e2) =
  do uv1 <- inferExprUnits e1
     uv2 <- inferExprUnits e2
     mustEqual False uv1 uv2
     return ()

inferStmtUnits (DoWhile _ _ _ f)   = inferStmtUnits f
inferStmtUnits (For _ _ _ (NullExpr _ _) _ _ s)   = inferStmtUnits s
inferStmtUnits (For _ _ (VarName _ v) e1 e2 e3 s) =
  do inferForHeaderUnits (v, e1, e2, e3)
     inferStmtUnits s

inferStmtUnits (FSeq _ _ s1 s2) = mapM_ inferStmtUnits [s1, s2]
inferStmtUnits (If _ _ e1 s1 elseifs ms2) =
  do inferExprUnits e1
     inferStmtUnits s1
     sequence [inferExprUnits e >> inferStmtUnits s | (e, s) <- elseifs]
     case ms2 of
           Just s2 -> inferStmtUnits s2
           Nothing -> return ()
inferStmtUnits (Allocate _ _ e1 e2) = mapM_ inferExprUnits [e1, e2]
inferStmtUnits (Backspace _ _ specs) = inferSpecUnits specs
inferStmtUnits (Call _ _ (Var _ _ [(VarName _ v, [])]) (ArgList _ e2)) =
  do uvs <- case e2 of
              NullExpr _ _ -> return []
              _ -> inferExprSeqUnits e2
     calls << (v, (Nothing, uvs))
inferStmtUnits (Call _ _ e1 (ArgList _ e2)) = mapM_ inferExprUnits [e1, e2]
inferStmtUnits (Open _ _ specs) = inferSpecUnits specs
inferStmtUnits (Close _ _ specs) = inferSpecUnits specs
inferStmtUnits (Continue _ _) = return ()
inferStmtUnits (Cycle _ _ _) = return ()
inferStmtUnits (Deallocate _ _ exprs e) =
  do mapM_ inferExprUnits exprs
     inferExprUnits e
     return ()
inferStmtUnits (Endfile _ _ specs) = inferSpecUnits specs
inferStmtUnits (Exit _ _ _) = return ()
inferStmtUnits (Forall _ _ (header, e) s) =
  do mapM_ inferForHeaderUnits header
     inferExprUnits e
     inferStmtUnits s
inferStmtUnits (Goto _ _ _) = return ()
inferStmtUnits (Nullify _ _ exprs) = mapM_ inferExprUnits exprs
inferStmtUnits (Inquire _ _ specs exprs) =
  do inferSpecUnits specs
     mapM_ inferExprUnits exprs
inferStmtUnits (Rewind _ _ specs) = inferSpecUnits specs
inferStmtUnits (Stop _ _ e) =
  do inferExprUnits e
     return ()
inferStmtUnits (Where _ _ e s s') =
  do inferExprUnits e
     inferStmtUnits s
     case s' of
       Nothing -> return ()
       Just s' -> inferStmtUnits s'
inferStmtUnits (Write _ _ specs exprs) =
  do inferSpecUnits specs
     mapM_ inferExprUnits exprs
inferStmtUnits (PointerAssg _ _ e1 e2) =
  do uv1 <- inferExprUnits e1
     uv2 <- inferExprUnits e2
     mustEqual False uv1 uv2
     return ()
inferStmtUnits (Return _ _ e) =
  do inferExprUnits e
     return ()
inferStmtUnits (Label _ _ _ s) = inferStmtUnits s
inferStmtUnits (Print _ _ e exprs) = mapM_ inferExprUnits (e:exprs)
inferStmtUnits (ReadS _ _ specs exprs) =
  do inferSpecUnits specs
     mapM_ inferExprUnits exprs
inferStmtUnits (TextStmt _ _ _) = return ()
inferStmtUnits (NullStmt _ _) = return ()



-- TODO: error handling in powerUnits

powerUnits :: (?assumeLiterals :: AssumeLiterals) => VarCol -> Expr Annotation -> State UnitEnv VarCol
powerUnits (VarCol uv) (Con _ _ powerString) =
  case fmap (fromInteger . fst) $ listToMaybe $ reads powerString of
    Just power -> do
      m <- addCol Temporary
      n <- addRow
      modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem power (n, uv)
      return $ VarCol m
    Nothing -> mustEqual False (VarCol uv) (VarCol 1)

powerUnits uv e =
  do mustEqual False uv (VarCol 1)
     uv <- inferExprUnits e
     mustEqual False uv (VarCol 1)
