{-  

  Units of measure extension to Fortran

  Files: Units.hs 
         UnitsEnvironment.hs 


TODO: 
 * Deal with variable shadowing in "contained" functions. 
 * Better errors with line number info 

-}


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse #-}

module Extensions.Units where

import Data.Ratio
import Data.Maybe
import Data.Matrix
import Data.List
import qualified Data.Vector as V
import Data.Label.Mono (Lens)
import qualified Data.Label
import Data.Label.Monadic hiding (modify)
import Data.Function
import Data.Data
import Data.Char
import Control.Monad.State.Strict hiding (gets)
import Data.Generics.Uniplate.Operations

import Helpers
import Output
import Analysis.Annotations
import Analysis.Syntax
import Analysis.Types
import Extensions.UnitsEnvironment -- Provides the types and data accessors used in this module
import Extensions.UnitsSolve -- Solvers for the Gaussian matrix

import Language.Fortran
import Language.Fortran.Pretty
import Transformation.Syntax

-- For debugging and development purposes
import Test.QuickCheck
import qualified Debug.Trace as D

{- HELPERS -}

-- Update a list state by consing
infix 2 <<
(<<) :: MonadState f m => Lens (->) f [o] -> o -> m ()
(<<) lens o = lens =. (o:)

-- Update a list state by appending
infix 2 <<++
(<<++) lens o = lens =. (++ [o])

{- START HERE! Two main functions of this file: inferUnits and removeUnits -}

removeUnits :: (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
removeUnits (fname, x) = let ?criticals = False in ("", (fname, map (descendBi removeUnitsInBlock) x))


-- *************************************
--   Unit inference (top - level)
-- *************************************


inferCriticalVariables :: (?solver :: Solver, ?assumeLiterals :: AssumeLiterals) => 
                          (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
inferCriticalVariables (fname, x) = 
    let ?criticals = True 
        ?debug = True
    in  let infer = do doInferUnits x
                       vars <- criticalVars
                       case vars of 
                         [] -> do report <<++ "No critical variables. Appropriate annotations."
                         _  -> do report <<++ "Critical variables: " ++ (concat $ intersperse "," vars)
                       ifDebug debugGaussian
                       
                       

            (_, env) = runState infer emptyUnitEnv
            r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env]
        in (r, (fname, x))

inferUnits :: (?solver :: Solver, ?assumeLiterals :: AssumeLiterals) => (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
inferUnits (fname, x) = 
    let ?criticals = False
        ?debug = False
    in let (y, env) = runState (doInferUnits x) emptyUnitEnv
           r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env] 
               ++ fname ++ ": checked/inferred " 
               ++ (show $ countVariables (_unitVarEnv env) (_debugInfo env) (_procedureEnv env) (fst $ _linearSystem env) (_unitVarCats env))
               ++ " user variables\n"
       in (r, (fname, y))


countVariables vars debugs procs matrix ucats = 
    length $ filter (\c -> case (ucats !! (c - 1)) of 
                             Variable -> case (lookupVarsByCols vars [c]) of
                                           [] -> False
                                           _  -> True
                             _        -> False) [1..ncols matrix]

emptyUnitEnv = UnitEnv { _report              = [],
                         _unitVarEnv          = [],
                         _derivedUnitEnv      = [],
                         _procedureEnv        = [],
                         _calls               = [],
                         _unitVarCats         = [Magic],
                         _reorderedCols       = [],
                         _underdeterminedCols = [],
                         _linearSystem        = (fromLists [[1]], [Unitful []]),
                         _debugInfo           = [],
                         _tmpRowsAdded        = [],
                         _tmpColsAdded        = []}


doInferUnits :: (?criticals :: Bool, ?solver :: Solver, ?debug :: Bool, ?assumeLiterals :: AssumeLiterals) => Program Annotation -> State UnitEnv (Program Annotation)
doInferUnits x = do mapM inferProgUnits x
                    ifDebug (report <<++ "Finished inferring prog units")
                    ifDebug debugGaussian
                    inferInterproceduralUnits x
                    if ?criticals then  return x -- don't insert unit annotations
                                  else  mapM (descendBiM insertUnitsInBlock) x

{-
To be removed: 
infoA [] = []
infoA ((Main _ _ _ _ _ _):ms) = "Main" : infoA ms
infoA ((Module _ _ _ _ _ _ _ ):ms) = "Module" : infoA ms
infoA (m:ms) = "Other" : infoA ms
-}

inferProgUnits :: (?criticals :: Bool, ?solver :: Solver, ?debug :: Bool, ?assumeLiterals :: AssumeLiterals) => ProgUnit Annotation -> State UnitEnv ()
inferProgUnits p =
  do -- infer units for *root* program unit
     inferPUnit p
     -- infer units for the *children* program units (so that the parent scope is processed first)
     mapM_ inferProgUnits $ ((children p)::[ProgUnit Annotation])

  where 
    -- Infer the units for the *root* program unit (not children)
     inferPUnit :: ProgUnit Annotation -> State UnitEnv ()
     inferPUnit (Main x sp n a b _)                       = inferBlockUnits b Nothing
     inferPUnit (Sub x sp t (SubName _ n) (Arg _ a _) b)  = inferBlockUnits b (Just (n, Nothing, argNames a))
     inferPUnit (Function _ _ _ (SubName _ n) (Arg _ a _) r b) = inferBlockUnits b (Just (n, Just (resultName n r), argNames a))
     inferPUnit (Module x sp (SubName _ n) _ _ d ds)       = transformBiM (inferDecl (Just (n, Nothing, []))) d >> return ()
     inferPUnit x = return ()

     argNames :: ArgName a -> [Variable]
     argNames (ArgName _ n) = [n]
     argNames (ASeq _ n1 n2) = argNames n1 ++ argNames n2
     argNames (NullArg _) = []

     resultName :: Variable -> Maybe (VarName a) -> Variable
     resultName n Nothing = n
     resultName _ (Just (VarName _ r)) = r


inferBlockUnits :: (?solver :: Solver, ?criticals :: Bool, ?debug :: Bool, ?assumeLiterals :: AssumeLiterals) => Block Annotation -> Maybe ProcedureNames -> State UnitEnv ()
inferBlockUnits x proc = do resetTemps
                            enterDecls x proc
                            addProcedure proc
                            descendBiM handleStmt x

                            case proc of 
                              Just _ -> {- not ?criticals -> -}
                                         do -- Intermediate solve for procedures (subroutines & functions)
                                            ifDebug (report <<++ "Pre doing row reduce")
                                            solveSystemM ""                                             
                                            linearSystem =. reduceRows 1
                                            ifDebug (report <<++ "Post doing reduce")
                                            ifDebug (debugGaussian)
                                            return ()
                              _     -> return ()
                            -- return x

                         where
                           handleStmt :: Fortran Annotation -> State UnitEnv (Fortran Annotation)
                           handleStmt x = do inferStmtUnits x
                                             return x

{-| reduceRows is a core part of the polymorphic unit checking for procedures. 
               
               It is essentially an "optimiation" of the Gaussian matrix (not in the sense of performance),
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


addProcedure Nothing = return ()
addProcedure (Just (name, resultName, argNames)) = 
      do uenv <- gets unitVarEnv 
         resultVar <- case resultName of 
                        Just rname -> case (lookup rname uenv) of
                                        Just (uvar, _) -> return $ Just uvar
                                        Nothing        -> do m <- addCol Variable
                                                             return $ Just (UnitVariable m)
                        Nothing    -> return Nothing
         let argVars = fmap (lookupUnitByName uenv) argNames
         procedureEnv << (name, (resultVar, argVars)) 
        where
          lookupUnitByName uenv v = maybe (UnitVariable 1) fst $ lookup v uenv 



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
               ArrayT _ bounds _ _ _ _ -> mapM (const $ fmap UnitVariable $ addCol Variable) bounds
               _                       -> return []
       unitVarEnv << (v, (UnitVariable m, ms)) 

       -- If the declaration has a null expression, do not create a unifying variable
       case e of 
            NullExpr _ _ -> return ()
            _            -> do uv <- inferExprUnits e
                               mustEqual False (UnitVariable m) uv
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

instance UpdateColInfo UnitVariable where
    updateColInfo x n (UnitVariable y) | y == x = UnitVariable n
                                       | y == n = UnitVariable x
                                       | otherwise = UnitVariable y

instance UpdateColInfo UnitVarEnv where
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
                  unitVarEnv   =. updateColInfo x n
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

addInterproceduralConstraints :: Program Annotation -> State UnitEnv ()
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
         let actual = map (\(UnitVariable uv) -> uv) actualVars
             dummy = map (\(UnitVariable uv) -> uv) dummyVars
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
   
    handleArg dummyToActual dummy =
      do (matrix, vector) <- gets linearSystem
         -- grid0 <- debugGaussian'

         --report <<++ ("hArg - " ++ show dummyToActual ++ "-" ++ show dummy)

         let -- find the first row with a non-zero column for the variable
             n = maybe 1 id $ find (\n -> matrix ! (n, dummy) /= 0) [1 .. nrows matrix]

             -- find the first non-zero column on the row just select
             Just m = find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

         --report <<++ ("n = " ++ show n ++ ", m = " ++ show m)

         if (m == dummy) then
           do  let -- Get list of columns with non-zero coefficients to the left of the focus
                   ms = filter (\m -> matrix ! (n, m) /= 0) [m .. ncols matrix]

                   -- Get the list of columns to which the non-zero coeffecients are paied by 'dummyToActual' relation. 
                   m's = mapMaybe (flip lookup dummyToActual) ms
                   pairs = --if (length m's == 1) then -- i.e. there is not a direct relationship between variable and return
                           --    zip ms (repeat (head m's))
                           --else
                               (zip ms m's)

               --report <<++ ("ms = " ++ show ms ++ ", m's' = " ++ show m's ++ ", their zip = " ++ show pairs)

               if (length m's == length ms) 
                 then do { n' <- addRow' $ vector !! (n - 1);
                           mapM_ (handleArgPair matrix n n') pairs; }
                 else return ()
         else
             return ()

         {-(matrix', vector') <- gets linearSystem
         if ((matrix == matrix') && (vector == vector')) then
             return ()
         else
             do report <<++ "HANDLED AND DIFFERENT!"
                report <<++ ("\n" ++ grid0)
                gridn <- debugGaussian'
                report <<++ ("\n" ++ gridn)
                return ()-}

    handleArgPair matrix n n' (m, m') = do modify $ liftUnitEnv $ setElem (matrix ! (n, m)) (n', m')
                                           
    decodeResult (Just r1) (Just r2) = ([r1], [r2])
    decodeResult Nothing Nothing = ([], [])
    decodeResult (Just _) Nothing = error "Subroutine used as a function!"
    decodeResult Nothing (Just _) = error "Function used as a subroutine!"


inferLiteral e = do uv@(UnitVariable uvn) <- anyUnits (Literal (?assumeLiterals /= Mixed))
                    debugInfo << (uvn, (srcSpan e, let ?variant = Alt1 in outputF e))
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

lookupCaseInsensitive :: String -> [(String, a)] -> Maybe a
lookupCaseInsensitive x ys = lookup' (map toUpper x) ys where
                               lookup' :: String -> [(String, a)] -> Maybe a
                               lookup' x [] = Nothing
                               lookup' x ((y,r):ys) | x == (map toUpper y) = Just r
                                                    | otherwise        = lookup' x ys

inferExprUnits :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv UnitVariable
inferExprUnits e@(Con _ _ _)    = inferLiteral e
inferExprUnits e@(ConL _ _ _ _) = inferLiteral e
inferExprUnits e@(ConS _ _ _)   = inferLiteral e
inferExprUnits ve@(Var _ _ names) =
 do uenv <- gets unitVarEnv
    penv <- gets procedureEnv
    let (VarName _ v, args) = head names

    case (lookup v uenv) <**> (lookupCaseInsensitive v uenv) of 
       -- array variable?
       Just (uv, uvs@(_:_)) -> inferArgUnits' uvs >> return uv
       -- function call?
       Nothing | not (null args) -> do case (lookup (map toUpper v) intrinsicsDict) of
                                          Just fun -> fun v 
                                          Nothing  -> return ()
                                       uv@(UnitVariable uvn) <- anyUnits Temporary
                                       debugInfo << (uvn, (srcSpan ve , let ?variant = Alt1 in outputF ve))
                                       uvs <- inferArgUnits
                                       let uvs' = justArgUnits args uvs
                                       calls << (v, (Just uv, uvs'))
                                       return uv
       -- scalar variable or external function call?
       Just (uv, []) -> inferArgUnits >> return uv
       -- default specifier
       _ | v == "*" -> inferLiteral ve
       -- just bad code
       x -> case ((lookup v penv) <**> (lookupCaseInsensitive v penv)) of
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
                                         (UnitVariable n) <- case binOpKind op of
                                                               AddOp   -> mustEqual True uv1 uv2
                                                               MulOp   -> mustAddUp uv1 uv2 1 1
                                                               DivOp   -> mustAddUp uv1 uv2 1 (-1)
                                                               PowerOp -> powerUnits uv1 e2
                                                               LogicOp -> mustEqual True uv1 uv2
                                                               RelOp   -> do mustEqual True uv1 uv2
                                                                             return $ UnitVariable 1
                                         debugInfo << (n, (srcSpan e, let ?variant = Alt1 in outputF e))
                                         return (UnitVariable n)
inferExprUnits (Unary _ _ _ e) = inferExprUnits e
inferExprUnits (CallExpr _ _ e1 (ArgList _ e2)) = do uv <- anyUnits Temporary
                                                     inferExprUnits e1
                                                     inferExprUnits e2
                                                     error "CallExpr not implemented"
                                                     return uv
-- inferExprUnits (NullExpr .... Shouldn't occur very often as adds unnnecessary cruft
inferExprUnits (NullExpr _ _) = anyUnits Temporary 

inferExprUnits (Null _ _) = return $ UnitVariable 1
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

inferExprSeqUnits :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv [UnitVariable]
inferExprSeqUnits (ESeq _ _ e1 e2) = liftM2 (++) (inferExprSeqUnits e1) (inferExprSeqUnits e2)
inferExprSeqUnits e = (:[]) `liftM` inferExprUnits e

handleExpr :: (?assumeLiterals :: AssumeLiterals) => Expr Annotation -> State UnitEnv (Expr Annotation)
handleExpr x = do inferExprUnits x
                  return x

inferForHeaderUnits :: (?assumeLiterals :: AssumeLiterals) => (Variable, Expr Annotation, Expr Annotation, Expr Annotation) -> State UnitEnv ()
inferForHeaderUnits (v, e1, e2, e3) =
  do uenv <- gets unitVarEnv
     case (lookup v uenv) of
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




-- *************************************
--    Matrix operations
-- 
-- *************************************


inverse :: [Int] -> [Int]
inverse perm = [j + 1 | Just j <- map (flip elemIndex perm) [1 .. length perm]]

fixValue :: Eq a => (a -> a) -> a -> a
fixValue f x = snd $ until (uncurry (==)) (\(x, y) -> (y, f y)) (x, f x)

-- The indexing for switchScaleElems and moveElem is 1-based, in line with Data.Matrix.



moveElem :: Int -> Int -> [a] -> [a]
moveElem i j []             = []
moveElem i j xs | i > j     = moveElem j i xs
                 | otherwise = moveElemA i j xs Nothing
                                where moveElemA i    j []     (Just z) = [z]
                                      moveElemA i    j []     Nothing  = []
                                      moveElemA 1    j (x:xs) (Just z) = x : moveElemA 1 (j - 1) xs (Just z)
                                      moveElemA 1    j (x:xs) Nothing  = moveElemA 1 j xs (Just x)
                                      moveElemA i    j (x:xs) Nothing  = x : moveElemA (i - 1) j xs Nothing


incrElem :: Num a => a -> (Int, Int) -> Matrix a -> Matrix a
incrElem value pos matrix = setElem (matrix ! pos + value) pos matrix

moveCol :: Int -> Int -> Matrix a -> Matrix a
moveCol i j m
    | i > j = moveCol j i m
    | otherwise = matrix (nrows m) (ncols m) 
                     $ \(r, c) -> if (c < i || c > j)       then m ! (r, c)
                                  else if (c >= i && c < j) then m ! (r, c+1)
                                       else                      m ! (r, i) 

addCol :: UnitVarCategory -> State UnitEnv Int
addCol category =
  do (matrix, vector) <- gets linearSystem
     let m = ncols matrix + 1
     linearSystem =: (extendTo 0 0 m matrix, vector)
     unitVarCats <<++ category
     tmpColsAdded << m
     return m

addRow :: State UnitEnv Int
addRow = addRow' (Unitful [])

addRow' :: UnitConstant -> State UnitEnv Int
addRow' uc =
  do (matrix, vector) <- gets linearSystem
     let n = nrows matrix + 1
     linearSystem =: (extendTo 0 n 0 matrix, vector ++ [uc])
     tmpRowsAdded << n
     return n

liftUnitEnv :: (Matrix Rational -> Matrix Rational) -> UnitEnv -> UnitEnv
liftUnitEnv f = Data.Label.modify linearSystem $ \(matrix, vector) -> (f matrix, vector)


-- *************************************
--   Unit inferences (Helpers)
-- 
-- *************************************
                                   
-- mustEqual - used for saying that two units must be the same- returns one of the variables
--             (choice doesn't matter, but left is chosen).
--             Returns the unit variables equaled upon
mustEqual :: (?assumeLiterals :: AssumeLiterals) => Bool -> UnitVariable -> UnitVariable -> State UnitEnv UnitVariable
mustEqual flagAsUnitlessIfLit (UnitVariable uv1) (UnitVariable uv2) = 
  do n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, uv1) . incrElem 1 (n, uv2)
     ucats <- gets unitVarCats 
     if flagAsUnitlessIfLit then 
       case ?assumeLiterals of 
         Mixed -> unitVarCats =: (map (\(n, cat) -> if ((n == uv1 || n == uv2) && ((cat == Literal True) || (cat == Literal False))) 
                                                    then Literal True 
                                                    else cat)  (zip [1..] ucats))
         _     -> return ()
      else return ()
     return $ UnitVariable uv1

-- mustAddUp - used for multipling and dividing. Creates a new 'temporary' column and returns
--             the variable associated with it
mustAddUp :: UnitVariable -> UnitVariable -> Rational -> Rational -> State UnitEnv UnitVariable
mustAddUp (UnitVariable uv1) (UnitVariable uv2) k1 k2 =
  do m <- addCol Temporary
     n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem k1 (n, uv1) . incrElem k2 (n, uv2)
     return $ UnitVariable m

-- TODO: error handling in powerUnits

powerUnits :: (?assumeLiterals :: AssumeLiterals) => UnitVariable -> Expr Annotation -> State UnitEnv UnitVariable
powerUnits (UnitVariable uv) (Con _ _ powerString) =
  case fmap (fromInteger . fst) $ listToMaybe $ reads powerString of
    Just power -> do
      m <- addCol Temporary
      n <- addRow
      modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem power (n, uv)
      return $ UnitVariable m
    Nothing -> mustEqual False (UnitVariable uv) (UnitVariable 1)

powerUnits uv e =
  do mustEqual False uv (UnitVariable 1)
     uv <- inferExprUnits e
     mustEqual False uv (UnitVariable 1)

sqrtUnits :: UnitVariable -> State UnitEnv UnitVariable
sqrtUnits (UnitVariable uv) =
  do m <- addCol Temporary
     n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem 0.5 (n, uv)
     return $ UnitVariable m

anyUnits :: UnitVarCategory -> State UnitEnv UnitVariable
anyUnits category =
  do m <- addCol category
     return $ UnitVariable m     


-- *************************************
--   Gaussian Elimination (Main)
-- 
-- *************************************
                                                                    


{- | An attempt at getting some useful user information. Needs position information -}
errorMessage :: (?debug :: Bool) => Row -> UnitConstant -> [Rational] -> State UnitEnv String
errorMessage row unit vars = 
 let ?variant = Alt1 in
    do uvarEnv <- gets unitVarEnv
       debugs <- gets debugInfo
       let unitStr = outputF (makeUnitSpec unit)
       let varCols = map (+1) (findIndices (\n -> n /= 0) vars)
       if varCols == [] then
           case unit of
             Unitful xs | length xs > 1 -> 
                            let unitStrL = outputF (makeUnitSpec (Unitful [head xs]))
                                xs' = map (\(v, r) -> (v, r * (-1))) (tail xs)
                                unitStrR = outputF (makeUnitSpec (Unitful $ xs'))
                                msg = "Conflict since " ++ unitStrL ++ " != " ++ unitStrR

                                getConflict (n, 0)      = ""
                                getConflict (n, r) =  case lookup n debugs of
                                                        (Just (span, s)) -> "\t" ++ (showSrcSpan span) ++ " - " ++ s ++ "\n"
                                                        _                -> ""


                                conflictSpots = concatMap getConflict (zip [1..] vars)
                            in (return $ msg ++ (if (conflictSpots == []) then "" else " arising from \n" ++ conflictSpots))
             {- A single unit with no variable column suggests an attempt to unify an unit
                with unitless -}
             Unitful xs | length xs == 1 ->                                
                            let xs' = map (\(v, r) -> (v, r * (-1))) xs
                                unitStrL = outputF (makeUnitSpec (Unitful xs'))
                            in do ifDebug debugGaussian
                                  return $ "Conflict since " ++ unitStrL ++ " != 1"
             _ -> do debugGaussian
                     return "Sorry, I can't give a better error."
       else
           let varColsAndNames = zip varCols (lookupVarsByCols uvarEnv varCols)
               exprStr' = map (\(k,v) -> if (vars !! (k - 1)) == 1 
                                         then v
                                         else (showRational (vars !! (k - 1))) ++ "*" ++ v) varColsAndNames
               exprStr = concat $ intersperse "*" exprStr'
               msg     = "Conflict arising from " ++ exprStr ++ " of unit " ++ unitStr
           in return msg

solveSystemM :: (?solver :: Solver, ?debug :: Bool) => String -> State UnitEnv Bool
solveSystemM adjective =
  do system <- gets linearSystem
     ifDebug debugGaussian
     case (solveSystem system) of
       Ok system'     -> do linearSystem =: system'
                            ifDebug (report <<++ "After solve")
                            ifDebug (debugGaussian)
                            return True
       Bad system' row (unit, vars) -> 
                     do report <<++ (adjective ++ " units of measure")
                        linearSystem =: system' 
                        if (adjective `elem` ["inconsistent", "underdetermined"]) then 
                            do msg <- errorMessage row unit vars
                               report <<++ msg
                               return False
                        else
                            return False
       BadL system' -> do report <<++ (adjective ++ " units of measure")
                          linearSystem =: system'
                          ifDebug debugGaussian
                          return False

checkUnderdeterminedM :: State UnitEnv ()
checkUnderdeterminedM = do ucats <- gets unitVarCats
                           system <- gets linearSystem
                           varenv  <- gets unitVarEnv
                           debugs  <- gets debugInfo
                           procenv <- gets procedureEnv

                           let badCols = checkUnderdetermined ucats system
                           uenv <- gets unitVarEnv
                           if not (null badCols) then 
                               do let exprs = map (showExprLines ucats varenv procenv debugs) badCols
                                  let exprsL = concat $ intersperse "\n\t" exprs
                                  debugGaussian
                                  report <<++ "Underdetermined units of measure. Try adding units to: \n\t" ++ exprsL
                                  return ()
                           else return ()
                           underdeterminedCols =: badCols


checkUnderdetermined :: [UnitVarCategory] -> LinearSystem -> [Int]
checkUnderdetermined ucats system@(matrix, vector) =
  fixValue (propagateUnderdetermined matrix) $ checkUnderdetermined' ucats system 1

criticalVars :: State UnitEnv [String]
criticalVars = do uvarenv     <- gets unitVarEnv
                  (matrix, _) <- gets linearSystem
                  ucats       <- gets unitVarCats
                  dbgs        <- gets debugInfo
                  let cv1 = criticalVars' uvarenv ucats matrix 1 dbgs
                  let cv2 = [] -- criticalVars
                  return (cv1 ++ cv2)

criticalVars' :: UnitVarEnv -> [UnitVarCategory] -> Matrix Rational -> Row -> DebugInfo -> [String]
criticalVars' varenv ucats matrix i dbgs =
  let m = firstNonZeroCoeff matrix ucats
  in
    if (i == nrows matrix) then 
       if (m i) /= (ncols matrix) then
          lookupVarsByColsFilterByArg matrix varenv ucats [((m i) + 1)..(ncols matrix)] dbgs
       else []
    else  
        if (m (i + 1)) /= ((m i) + 1)         
        then (lookupVarsByColsFilterByArg matrix varenv ucats [((m i) + 1)..(m (i + 1) - 1)] dbgs) ++ (criticalVars' varenv ucats matrix (i + 1) dbgs)
        else criticalVars' varenv ucats matrix (i + 1) dbgs

lookupVarsByColsFilterByArg :: Matrix Rational -> UnitVarEnv -> [UnitVarCategory] -> [Int] -> DebugInfo -> [String]
lookupVarsByColsFilterByArg matrix uenv ucats cols dbgs = 
      mapMaybe (\j -> lookupEnv j uenv) cols
         where lookupEnv j [] = --Nothing
                                case (lookup (j - 1) dbgs) of
                                  Just (srcSpan, info) -> Just ("[expr: " ++ (showSrcSpan srcSpan) ++ "@" ++ info ++ "]")
                                  Nothing              -> Nothing
               lookupEnv j ((v, (UnitVariable i, _)):uenv)
                                              | i == j    = if (j <= length ucats) then
                                                             case (ucats !! (j - 1)) of
                                                                Argument -> Nothing
                                                                _        -> if (all (==0) (V.toList (getCol j matrix)))
                                                                            then Nothing
                                                                            else Just v 
                                                            else Nothing
                                              | otherwise = lookupEnv j uenv

firstNonZeroCoeff :: Matrix Rational -> [UnitVarCategory] -> Row -> Col
firstNonZeroCoeff matrix ucats row = 
      case (V.findIndex (/= 0) (getRow row matrix)) of
                                  Nothing -> ncols matrix
                                  Just i  -> i + 1
{-    firstNonZeroCoeff' (V.toList $ getRow row matrix) 0
       where
                {-   -}
         firstNonZeroCoeff' []     n = n + 1
         firstNonZeroCoeff' (0:rs) n = firstNonZeroCoeff' rs (n+1)
         firstNonZeroCoeff' (r:rs) n = case (ucats !! n) of
                                         Literal -> firstNonZeroCoeff' rs (n + 1)
                                         _       -> n + 1-}



-- debug string ("n = " ++ show n ++ " vc = " ++ (show (vector !! (n - 1))) ++ " ms = " ++ show ms ++ " rest = " ++ show rest) `D.trace` 
checkUnderdetermined' :: [UnitVarCategory] -> LinearSystem -> Int -> [Int]
checkUnderdetermined' ucats system@(matrix, vector) n
  | n > nrows matrix = []
  | not ((drop 1 ms) == []) && vector !! (n - 1) /= Unitful [] = ms ++ rest
  | otherwise = rest
  where ms = filter significant [2 .. ncols matrix]
        significant m = matrix ! (n, m) /= 0 && ucats !! (m - 1) `notElem` [Literal False, Literal True, Argument, Temporary]
        rest = checkUnderdetermined' ucats system (n + 1)

propagateUnderdetermined :: Matrix Rational -> [Int] -> [Int]
propagateUnderdetermined matrix list = 
    nub $ do m <- list
             n <- filter (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
             filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]


    


-- *************************************
--   Intrinsic functions: information &
--      setup functions for them.
--
-- *************************************

intrinsicsDict :: (?assumeLiterals :: AssumeLiterals) => [(String, String -> State UnitEnv ())]
intrinsicsDict = 
    map (\x -> (x, addPlain1ArgIntrinsic)) ["ABS", "ACHAR", "ADJUSTL", "ADJUSTR", "AIMAG", "AINT", "ANINT", "CEILING", "CONJG", "DBLE", "EPSILON", "FLOOR","FLOAT", "FRACTION", "HUGE", "IACHAR", "ICHAR", "INT", "IPARITY", "LOGICAL", "MAXEXPONENT", "MINEXPONENT",  "NEW_LINE", "NINT", "NORM2", "NOT", "NULL", "PARITY", "REAL", "RRSPACING", "SPACING", "SUM", "TINY", "TRANSPOSE", "TRIM"]
    
 ++ map (\x -> (x, addPlain2ArgIntrinsic)) ["ALL", "ANY", "IALL", "IANY", "CHAR", "CMPLX", "DCOMPLX", "DIM", "HYPOT", "IAND", "IEOR", "IOR", "MAX", "MIN", "MAXVAL", "MINVAL","MODULO", "MOD"] 
    
 ++ map (\x -> (x, addPlain1Arg1ExtraIntrinsic)) ["CSHIFT", "EOSHIFT", "IBCLR", "IBSET", "NEAREST", "PACK", "REPEAT", "RESHAPE", "SHIFTA", "SHIFTL", "SHIFTR", "SIGN"]

 ++ map (\x -> (x, addPlain2Arg1ExtraIntrinsic)) ["DSHIFTL", "DSHIFTR", "ISHFT", "ISHFTC", "MERGE", "MERGE_BITS"]

 ++ map (\x -> (x, addProductIntrinsic)) ["DOT_PRODUCT", "DPROD", "MATMUL"]

 ++ map (\x -> (x, addPowerIntrinsic)) ["SCALE", "SET_EXPONENT"]

 ++ map (\x -> (x, addUnitlessIntrinsic)) ["ACOS", "ACOSH", "ASIN", "ASINH", "ATAN", "ATANH", "BESSEL_J0", "BESSEL_J1", "BESSEL_Y0", "BESSEL_Y1", "COS", "COSH", "ERF", "ERFC", "ERFC_SCALED", "EXP", "EXPONENT", "GAMMA", "LOG", "LOG10", "LOG_GAMMA", "PRODUCT", "SIN", "SINH", "TAN", "TANH"]

 ++ map (\x -> (x, addUnitlessSubIntrinsic)) ["CPU_TIME", "RANDOM_NUMBER"]

 ++ map (\x -> (x, addUnitlessResult0ArgIntrinsic)) ["COMMAND_ARGUMENT_COUNT", "COMPILER_OPTIONS", "COMPILER_VERSION"]

 ++ map (\x -> (x, addUnitlessResult1ArgIntrinsic)) ["ALLOCATED", "ASSOCIATED", "BIT_SIZE", "COUNT", "DIGITS",  "IS_IOSTAT_END", "IS_IOSTAT_EOR", "KIND", "LBOUND", "LCOBOUND", "LEADZ", "LEN", "LEN_TRIM", "MASKL", "MASKR", "MAXLOC", "MINLOC", "POPCOUNT", "POPPAR", "PRECISION", "PRESENT", "RADIX", "RANGE", "SELECTED_CHAR_KIND", "SELECTED_INT_KIND", "SELECTED_REAL_KIND", "SHAPE", "SIZE", "STORAGE_SIZE", "TRAILZ", "UBOUND", "UCOBOUND"]

 ++ map (\x -> (x, addUnitlessResult2SameArgIntrinsic)) ["ATAN2", "BGE", "BGT", "BLE", "BLT", "INDEX", "LGE", "LGT", "LLE", "LLT", "SCAN", "VERIFY"]

 ++ map (\x -> (x, addUnitlessResult2AnyArgIntrinsic)) ["BTEST", "EXTENDS_TYPE_OF", "SAME_TYPE_AS"]

     -- missing: ATOMIC_DEFINE, ATOMIC_REF, BESSEL_JN, BESSEL_YN, C_*, DATE_AND_TIME, EXECUTE_COMMAND_LINE, GET_COMMAND, GET_COMMAND_ARGUMENT, GET_ENVIRONMENT_VARIABLE, IBITS, any of the image stuff, MOVE_ALLOC, MVBITS, RANDOM_SEED, SPREAD, SYSTEM_CLOCK, TRANSFER, UNPACK


{- [A] Various helpers for adding information about procedures to the type system -}

addPlain1ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPlain1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual False result arg
     procedureEnv << (name, (Just result, [arg]))

addPlain2ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPlain2ArgIntrinsic name =
  do result <- anyUnits Variable
     arg1  <- anyUnits Argument
     arg2  <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False result arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain1Arg1ExtraIntrinsic :: (?assumeLiterals :: AssumeLiterals) =>  String -> State UnitEnv ()
addPlain1Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result arg1
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain2Arg1ExtraIntrinsic :: (?assumeLiterals :: AssumeLiterals) =>  String -> State UnitEnv ()
addPlain2Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     arg3   <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False result arg2
     procedureEnv << (name, (Just result, [arg1, arg2, arg3]))

addProductIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addProductIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     temp   <- mustAddUp arg1 arg2 1 1
     mustEqual False result temp
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPowerIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addPowerIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result arg1
     mustEqual False arg2 (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual False result (UnitVariable 1)
     mustEqual False arg (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessSubIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessSubIntrinsic name =
  do arg <- anyUnits Variable
     mustEqual False arg (UnitVariable 1)
     procedureEnv << (name, (Nothing, [arg]))

addUnitlessResult0ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult0ArgIntrinsic name =
  do result <- anyUnits Variable
     mustEqual False result (UnitVariable 1)
     procedureEnv << (name, (Just result, []))

addUnitlessResult1ArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg <- anyUnits Argument
     mustEqual False result (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessResult2AnyArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult2AnyArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessResult2SameArgIntrinsic :: (?assumeLiterals :: AssumeLiterals) => String -> State UnitEnv ()
addUnitlessResult2SameArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual False result (UnitVariable 1)
     mustEqual False arg1 arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))



-- *************************************
--   Debugging and testing functions
-- 
-- *************************************

    

-- QuickCheck instance for matrices, used for testing matrix operations
instance (Arbitrary a) => Arbitrary (Matrix a) where
    arbitrary = sized (\n -> do xs <- vectorOf (n*n) arbitrary
                                return $ matrix n n (\(i, j) -> xs !! ((i-1)*n + (j-1))))


-- Matrix for development 
fooMatrix :: Matrix Rational
fooMatrix = matrix 4 4 $ (\(i,j) -> if (i==j) then (toInteger i) % 1 else 0)


{-| debugGaussian - a debugging routine which shose the Gaussian matrix with various peieces of info
                    mainly used for development purposes -}
debugGaussian :: State UnitEnv String
debugGaussian = do grid' <- debugGaussian'
                   report <<++ ("Dump of units-of-measure system matrix\n" ++ grid')
                   return grid'

debugGaussian' = do ucats   <- gets unitVarCats
                    (matrix,rowv)  <- gets linearSystem
                    varenv  <- gets unitVarEnv
                    debugs  <- gets debugInfo
                    procenv <- gets procedureEnv

                    let -- Column headings and then a space 
                        grid = ["" : map show [1..(ncols matrix)], []] 
                        -- Gaussian matrix 
                            ++ map (\r -> (show r) : (map showRational $ V.toList $ getRow r matrix) ++ [show $ rowv !! (r - 1)]) [1..(nrows matrix)]
                        -- Column categories
                            ++ [[], "" : map showCat ucats]
                        -- Debug info, e.g., expression or variable
                            ++ ["" : map (showExpr ucats varenv procenv debugs) [1.. (ncols matrix)]]
                        -- Additional debug info for args that are also variables
                            ++ ["" : map (showArgVars ucats varenv) [1..(ncols matrix)]]
                    let colSize = maximum' (map maximum' (map (notLast . (map length)) grid)) 
                    let expand r = r ++ (replicate (colSize - length r) ' ') 
                    let showLine x = (concatMap expand x) ++ "\n"
                              
                    let grid' = concatMap showLine grid
                    return grid'

   where maximum' [] = 0
         maximum' xs = maximum xs

         notLast xs = take (length xs - 1) xs

showExpr cats vars procs debugInfo c = 
             case (cats !! (c - 1)) of
               Variable  -> case (lookupVarsByCols vars [c]) of
                              []    -> case (lookupProcByCols procs [c]) of
                                         []    -> "?"
                                         (x:_) -> "=" ++ x
                              (x:_) -> x
               Temporary -> snd $ case (lookup c debugInfo) of
                                    Just x -> x
                                    Nothing -> (undefined, "") -- error $ "Temporary fail " ++ (show c) " not in " ++ (show cats)
               Argument  -> case (lookupProcByArgCol procs [c]) of 
                              []    -> "?"
                              (x:_) -> x
               Literal _  -> snd $ case (lookup c debugInfo) of
                                    Just x -> x
                                    Nothing -> show c `D.trace` error "Literal fail"
               Magic     -> ""

showSrcLoc loc = show (srcLine loc) ++ ":" ++ show (srcColumn loc)
showSrcSpan (start, end) = "(" ++ showSrcLoc start ++ " - " ++ showSrcLoc end ++ ")"

showSrcFile (start, _) = srcFilename start

showExprLines cats vars procs debugInfo c = 
             case (cats !! (c - 1)) of
               Variable  -> case (lookup c debugInfo) of 
                              Just (sp, expr) -> (showSrcSpan sp) ++ "\t" ++ expr 
                              Nothing -> 
                                case (lookupVarsByCols vars [c]) of
                                  []    -> case (lookupProcByCols procs [c]) of
                                             []    -> "?"
                                             (x:_) -> "=" ++ x
                                  (x:_) -> x
               Temporary -> let (sp, expr) = fromJust $ lookup c debugInfo
                            in (showSrcSpan sp) ++ "\t" ++ expr 
               Argument  -> case (lookupProcByArgCol procs [c]) of 
                              []    -> "?"
                              (x:_) -> x
               Literal _ -> let (sp, expr) = fromJust $ lookup c debugInfo
                            in (showSrcSpan sp) ++ "\t" ++ expr 
               Magic     -> ""

showArgVars cats vars c = 
             case (cats !! (c - 1)) of
               Argument -> case (lookupVarsByCols vars [c]) of
                             []    -> ""
                             (x:_) -> x
               _        -> ""
                              

showCat Variable  = "Var"
showCat Magic     = "Magic"
showCat Temporary = "Temp"
showCat Argument  = "Arg"
showCat (Literal False) = "Lit"
showCat (Literal True)  = "Lit="

lookupProcByArgCol :: ProcedureEnv -> [Int] -> [String]
lookupProcByArgCol penv cols = 
             mapMaybe (\j -> lookupEnv j penv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((p, (_, args)):penv) 
                           | elem (UnitVariable j) args  = Just (p ++ "#" ++ (show $ fromJust $ elemIndex (UnitVariable j) args))
                           | otherwise    = lookupEnv j penv
             

lookupProcByCols :: ProcedureEnv -> [Int] -> [String]
lookupProcByCols penv cols = 
             mapMaybe (\j -> lookupEnv j penv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((p, (Just (UnitVariable i), _)):penv) 
                                    | i == j    = Just p
                                    | otherwise = lookupEnv j penv
                       lookupEnv j ((p, (Nothing, _)):penv) = lookupEnv j penv

lookupVarsByCols :: UnitVarEnv -> [Int] -> [String]
lookupVarsByCols uenv cols = mapMaybe (\j -> lookupEnv j uenv) cols
                 where lookupEnv j [] = Nothing
                       lookupEnv j ((v, (UnitVariable i, _)):uenv)
                                    | i == j    = Just v 
                                    | otherwise = lookupEnv j uenv

showRational r = show (numerator r) ++ if ((denominator r) == 1) then "" else "%" ++ (show $ denominator r) 

-- *************************************
--   Insert unit declarations into code
--
-- *************************************

insertUnitsInBlock :: Block Annotation -> State UnitEnv (Block Annotation)
insertUnitsInBlock x = do env <- get
                          return $ transformBi (insertUnits env) x

removeUnitsInBlock :: Block Annotation -> Block Annotation
removeUnitsInBlock = transformBi deleteUnits

convertUnit :: MeasureUnitSpec a -> State UnitEnv UnitConstant
convertUnit (UnitProduct _ units) = convertUnits units
convertUnit (UnitQuotient _ units1 units2) = liftM2 (-) (convertUnits units1) (convertUnits units2)
convertUnit (UnitNone _) = return $ Unitful []

convertUnits :: [(MeasureUnit, Fraction a)] -> State UnitEnv UnitConstant
convertUnits units =
  foldl (+) (Unitful []) `liftM` sequence [convertSingleUnit unit (fromFraction f) | (unit, f) <- units]

convertSingleUnit :: MeasureUnit -> Rational -> State UnitEnv UnitConstant
convertSingleUnit unit f =
  do denv <- gets derivedUnitEnv
     let uc f' = Unitful [(unit, f')]
     case lookup unit denv of 
       Just uc' -> return $ uc' * (fromRational f)
       Nothing  -> derivedUnitEnv << (unit, uc 1) >> return (uc f) 

fromFraction :: Fraction a -> Rational
fromFraction (IntegerConst _ n) = fromInteger $ read n
fromFraction (FractionConst _ p q) = fromInteger (read p) / fromInteger (read q)
fromFraction (NullFraction _) = 1

extractUnit :: Attr a -> [State UnitEnv UnitConstant]
extractUnit attr = case attr of
                     MeasureUnit _ unit -> [convertUnit unit]
                     _ -> []


lookupUnit :: [UnitVarCategory] -> [Int] -> LinearSystem -> Col -> Maybe UnitConstant
lookupUnit ucats badCols system@(matrix, vector) m =
  let -- m is the column corresopnding to the variable for which we are looking up the unit
      n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
      defaultUnit = if ucats !! (m - 1) == Argument then Nothing else Just (Unitful [])
  in maybe defaultUnit (lookupUnit' ucats badCols system m) n


lookupUnit' :: [UnitVarCategory] -> [Int] -> LinearSystem -> Int -> Int -> Maybe UnitConstant
lookupUnit' ucats badCols (matrix, vector) m n
  | not $ null ms = Nothing
  | ucats !! (m - 1) /= Argument && m `notElem` badCols = Just $ vector !! (n - 1)
  | ms' /= [m] = Nothing
  | otherwise = Just $ vector !! (n - 1)
  where ms = filter significant [1 .. ncols matrix]
        significant m' = m' /= m && matrix ! (n, m') /= 0 && ucats !! (m' - 1) == Argument
        ms' = filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]

insertUnits :: UnitEnv -> Decl Annotation -> Decl Annotation
insertUnits env (Decl a sp@(s1, s2) d t) | not (pRefactored a || hasUnits t || types == [t]) =
  DSeq a (NullDecl a' sp'') (foldr1 (DSeq a) decls)
  where system = Data.Label.get linearSystem env
        ucats = Data.Label.get unitVarCats env
        badCols = Data.Label.get underdeterminedCols env
        uVarEnv = Data.Label.get unitVarEnv env
        varCol (Var _ _ ((VarName _ v, _):_), _, _) = case (lookup v uVarEnv) of
                                                          (Just (UnitVariable m,_)) -> m
                                                          Nothing -> error $ "No variable " ++ (show v)
        sameUnits = (==) `on` (lookupUnit ucats badCols system . varCol)
        groups = groupBy sameUnits d
        types = map (insertUnit ucats badCols system t . varCol . head) groups
        a' = a { refactored = Just s1 }
        sp' = dropLine $ refactorSpan sp
        sp'' = (toCol0 s1, snd $ dropLine sp)
        decls = [Decl a' sp' group t' | (group, t') <- zip groups types]
insertUnits _ decl = decl

deleteUnits :: Decl Annotation -> Decl Annotation
deleteUnits (Decl a sp@(s1, s2) d t) | hasUnits t =
  Decl a' (dropLine sp) d t'
  where a' = a { refactored = Just $ toCol0 s1 }
        t' = deleteUnit t
deleteUnits (MeasureUnitDef a sp@(s1, s2) d) =
  NullDecl a' sp'
  where a' = a { refactored = Just s1 }
        sp' = (toCol0 s1, snd $ dropLine sp)
deleteUnits decl = decl

hasUnits :: Type a -> Bool
hasUnits (BaseType _ _ attrs _ _) = any isUnit attrs
hasUnits (ArrayT _ _ _ attrs _ _) = any isUnit attrs

isUnit :: Attr a -> Bool
isUnit (MeasureUnit _ _) = True
isUnit _ = False

insertUnit :: [UnitVarCategory] -> [Int] -> LinearSystem -> Type Annotation -> Int -> Type Annotation
insertUnit ucats badCols system (BaseType aa tt attrs kind len) uv =
  BaseType aa tt (insertUnit' unit attrs) kind len
  where unit = lookupUnit ucats badCols system uv
insertUnit ucats badCols system (ArrayT dims aa tt attrs kind len) uv =
  ArrayT dims aa tt (insertUnit' unit attrs) kind len
  where unit = lookupUnit ucats badCols system uv

deleteUnit :: Type Annotation -> Type Annotation
deleteUnit (BaseType aa tt attrs kind len) =
  BaseType aa tt (filter (not . isUnit) attrs) kind len
deleteUnit (ArrayT dims aa tt attrs kind len) =
  ArrayT dims aa tt (filter (not . isUnit) attrs) kind len

insertUnit' :: Maybe UnitConstant -> [Attr Annotation] -> [Attr Annotation]
insertUnit' (Just unit) attrs = attrs ++ [MeasureUnit unitAnnotation $ makeUnitSpec unit]
insertUnit' Nothing attrs = attrs

makeUnitSpec :: UnitConstant -> MeasureUnitSpec Annotation
makeUnitSpec (UnitlessC r) = UnitProduct unitAnnotation [("1", (FractionConst unitAnnotation (show $ numerator r) (show $ denominator r)))] --hm!
makeUnitSpec (Unitful []) = UnitNone unitAnnotation
makeUnitSpec (Unitful units)
  | null neg = UnitProduct unitAnnotation $ formatUnits pos
  | otherwise = UnitQuotient unitAnnotation (formatUnits pos) (formatUnits neg)
  where pos = filter (\(unit, r) -> r > 0) units
        neg = [(unit, -r) | (unit, r) <- units, r < 0]

formatUnits :: [(MeasureUnit, Rational)] -> [(MeasureUnit, Fraction Annotation)]
formatUnits units = [(unit, toFraction r) | (unit, r) <- units]

toFraction :: Rational -> Fraction Annotation
toFraction 1 = NullFraction unitAnnotation
toFraction r
  | q == 1 = IntegerConst unitAnnotation $ show p
  | otherwise = FractionConst unitAnnotation (show p) (show q)
  where p = numerator r
        q = denominator r