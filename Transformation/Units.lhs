> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}

> module Transformation.Units where

> import Data.Ratio
> import Data.Matrix
> import Data.List
> import Data.Label.Monadic hiding (modify)
> import qualified Data.Label
> import Data.Function
> import Data.Data
> import Control.Monad.State.Strict hiding (gets)

> import Data.Generics.Uniplate.Operations

> import Analysis.Annotations
> import Analysis.Types
> import Transformation.Syntax
> import Language.Fortran

> import Helpers

> data UnitConstant = Unitful [(MeasureUnit, Rational)] | Unitless Rational deriving Eq
> trim = filter $ \(unit, r) -> r /= 0
> instance Num UnitConstant where
>   (Unitful u1) + (Unitful u2) = Unitful $ trim $ merge u1 u2
>     where merge [] u2 = u2
>           merge u1 [] = u1
>           merge ((unit1, r1) : u1) ((unit2, r2) : u2)
>             | unit1 == unit2 = (unit1, r1 + r2) : merge u1 u2
>             | unit1 <  unit2 = (unit1, r1) : merge u1 ((unit2, r2) : u2)
>             | otherwise      = (unit2, r2) : merge ((unit1, r1) : u1) u2
>   (Unitless n1) + (Unitless n2) = Unitless (n1 + n2)
>   (Unitful units) * (Unitless n) = Unitful $ trim [(unit, r * n) | (unit, r) <- units]
>   (Unitless n) * (Unitful units) = Unitful $ trim [(unit, n * r) | (unit, r) <- units]
>   (Unitless n1) * (Unitless n2) = Unitless (n1 * n2)
>   negate (Unitful units) = Unitful [(unit, -r) | (unit, r) <- units]
>   negate (Unitless n) = Unitless (-n)
>   abs (Unitful units) = Unitful [(unit, abs r) | (unit, r) <- units]
>   abs (Unitless n) = Unitless $ abs n
>   signum (Unitful units) = Unitful [(unit, signum r) | (unit, r) <- units]
>   signum (Unitless n) = Unitless $ signum n
>   fromInteger = Unitless . fromInteger
> instance Fractional UnitConstant where
>   (Unitful units) / (Unitless n) = Unitful [(unit, r / n) | (unit, r) <- units]
>   (Unitless n1) / (Unitless n2) = Unitless (n1 / n2)
>   fromRational = Unitless . fromRational

> data UnitVariable = UnitVariable Int
> data UnitVarCategory = Literal | Temporary | Variable deriving Eq
> type UnitVarEnv = [(Variable, UnitVariable)]
> type DerivedUnitEnv = [(MeasureUnit, UnitConstant)]
> type LinearSystem = (Matrix Rational, [UnitConstant])
> data Lalala = Lalala {
>   _unitVarEnv :: UnitVarEnv,
>   _derivedUnitEnv :: DerivedUnitEnv,
>   _unitVarCats :: [UnitVarCategory],
>   _linearSystem :: LinearSystem
> }
> emptyLalala = Lalala [] [] [Literal] (fromLists [[1]], [Unitful []])
> Data.Label.mkLabels [''Lalala]

> descendBi' :: (Data (from a), Data (to a)) => (to a -> to a) -> from a -> from a
> descendBi' f x = descendBi f x

> descendBiM' :: (Monad m, Data (from a), Data (to a)) => (to a -> m (to a)) -> from a -> m (from a)
> descendBiM' f x = descendBiM f x

> transformBi' :: (Data (from a), Data (to a)) => (to a -> to a) -> from a -> from a
> transformBi' f x = transformBi f x

> transformBiM' :: (Monad m, Data (from a), Data (to a)) => (to a -> m (to a)) -> from a -> m (from a)
> transformBiM' f x = transformBiM f x

The indexing for switchScaleElems is 1-based, in line with Data.Matrix.

> switchScaleElems :: Num a => Int -> Int -> a -> [a] -> [a]
> switchScaleElems i j factor list = a ++ factor * b : c
>   where (lj, b:rj) = splitAt (j - 1) list
>         (a, _:c) = splitAt (i - 1) (lj ++ list !! (i - 1) : rj)

> solveSystemM :: State Lalala ()
> solveSystemM = linearSystem =. solveSystem

> solveSystem :: LinearSystem -> LinearSystem
> solveSystem system = solveSystem' system 1 1

> solveSystem' :: LinearSystem -> Int -> Int -> LinearSystem
> solveSystem' (matrix, vector) m k
>   | m > ncols matrix = checkSystem (matrix, vector) k
>   | otherwise = elimRow (matrix, vector) n m k
>                 where n = find (\n -> matrix ! (n, m) /= 0) [k .. nrows matrix]

> checkSystem :: LinearSystem -> Int -> LinearSystem
> checkSystem (matrix, vector) k
>   | k > nrows matrix = (matrix, vector)
>   | vector !! (k - 1) /= Unitful [] = error "Inconsistent units of measure!"
>   | otherwise = checkSystem (matrix, vector) (k + 1)

> elimRow :: LinearSystem -> Maybe Int -> Int -> Int -> LinearSystem
> elimRow system Nothing m k = solveSystem' system (m + 1) k
> elimRow (matrix, vector) (Just n) m k = solveSystem' system' (m + 1) (k + 1)
>   where matrix' = switchRows k n $ scaleRow (recip $ matrix ! (n, m)) n matrix
>         vector' = switchScaleElems k n (fromRational $ recip $ matrix ! (n, m)) vector
>         system' = elimRow' (matrix', vector') k m

> elimRow' :: LinearSystem -> Int -> Int -> LinearSystem
> elimRow' (matrix, vector) k m = (matrix', vector')
>   where mstep matrix n = combineRows n (- matrix ! (n, m)) k matrix
>         matrix' = foldl mstep matrix $ [1 .. k - 1] ++ [k + 1 .. nrows matrix]
>         vector'' = [x - fromRational (matrix ! (n, m)) * vector !! (k - 1) | (n, x) <- zip [1..] vector]
>         (a, _ : b) = splitAt (k - 1) vector''
>         vector' = a ++ vector !! (k - 1) : b

> checkUnderdeterminedM :: State Lalala ()
> checkUnderdeterminedM = do ucats <- gets unitVarCats
>                            linearSystem =. checkUnderdetermined ucats

> checkUnderdetermined :: [UnitVarCategory] -> LinearSystem -> LinearSystem
> checkUnderdetermined ucats system = checkUnderdetermined' ucats system 1

> checkUnderdetermined' :: [UnitVarCategory] -> LinearSystem -> Int -> LinearSystem
> checkUnderdetermined' ucats system@(matrix, vector) n
>   | n > nrows matrix = system
>   | not (null $ drop 1 ms) && vector !! (n - 1) /= Unitful [] = error "Underdetermined units of measure!"
>   | otherwise = checkUnderdetermined' ucats system (n + 1)
>   where ms = filter (\m -> matrix ! (n, m) /= 0 && ucats !! (m - 1) /= Literal) [1 .. ncols matrix]

> inferUnits :: (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
> inferUnits (fname, x) = ("", (fname, evalState (doInferUnits x) emptyLalala))

> doInferUnits :: Program Annotation -> State Lalala (Program Annotation)
> doInferUnits x = do y <- mapM (descendBiM' blockLalala) x
>                     mapM (descendBiM' insertUnitsInBlock) y

> blockLalala :: Block Annotation -> State Lalala (Block Annotation)
> blockLalala x = do uenv <- gets unitVarEnv
>                    y <- enterDecls x
>                    descendBiM' handleStmt y
>                    checkUnderdeterminedM
>                    unitVarEnv =: uenv
>                    return y

> convertUnit :: MeasureUnitSpec a -> State Lalala UnitConstant
> convertUnit (UnitProduct _ units) = convertUnits units
> convertUnit (UnitQuotient _ units1 units2) = liftM2 (-) (convertUnits units1) (convertUnits units2)
> convertUnit (UnitNone _) = return $ Unitful []

> convertUnits :: [(MeasureUnit, Fraction a)] -> State Lalala UnitConstant
> convertUnits units = foldl1 (+) `liftM` sequence [convertSingleUnit unit (fromFraction f) | (unit, f) <- units]

> convertSingleUnit :: MeasureUnit -> Rational -> State Lalala UnitConstant
> convertSingleUnit unit f =
>   do denv <- gets derivedUnitEnv
>      let uc = Unitful [(unit, f)]
>          denv' = denv ++ [(unit, uc)]
>      case lookup unit denv of
>        Just uc' -> return $ uc' * (fromRational f)
>        Nothing  -> derivedUnitEnv =: denv' >> return uc

> fromFraction :: Fraction a -> Rational
> fromFraction (IntegerConst _ n) = fromInteger $ read n
> fromFraction (FractionConst _ p q) = fromInteger (read p) / fromInteger (read q)
> fromFraction (NullFraction _) = 1

> extractUnit :: Attr a -> [State Lalala UnitConstant]
> extractUnit attr = case attr of
>                      MeasureUnit _ unit -> [convertUnit unit]
>                      _ -> []

> extendConstraints :: [UnitConstant] -> LinearSystem -> LinearSystem
> extendConstraints units (matrix, vector) =
>   case units of
>     [] -> (extendTo 0 m matrix, vector)
>     _ -> (setElem 1 (n, m) $ extendTo n m matrix, vector ++ [last units])
>   where n = nrows matrix + 1
>         m = ncols matrix + 1

> enterDecls :: Block Annotation -> State Lalala (Block Annotation)
> enterDecls x =
>   transformBiM f x
>   where
>     f (Decl a s d t) =
>       do
>         let BaseType _ _ attrs _ _ = arrayElementType t
>         units <- sequence $ concatMap extractUnit attrs
>         d' <- dm units
>         return $ Decl a s d' t
>       where
>         dm units = sequence $ do
>           (Var a s names, e) <- d
>           let (VarName _ v, _) = head names
>           return $ do
>             uenv <- gets unitVarEnv
>             ucats <- gets unitVarCats
>             system <- gets linearSystem
>             let m = ncols (fst system) + 1
>                 uenv' = uenv ++ [(v, UnitVariable m)]
>                 ucats' = ucats ++ [Variable]
>                 system' = extendConstraints units system
>             unitVarEnv =: uenv'
>             unitVarCats =: ucats'
>             linearSystem =: system'
>             return (Var a { unitVar = m } s names, e)
>     f x@(MeasureUnitDef a s d) =
>       do
>         mapM_ learnDerivedUnit d
>         return x
>       where
>         learnDerivedUnit (name, spec) =
>           do denv <- gets derivedUnitEnv
>              let Nothing = lookup name denv -- FIXME: error handling
>              unit <- convertUnit spec
>              let denv' = denv ++ [(name, unit)]
>              derivedUnitEnv =: denv'
>     f x = return x

> insertUnitsInBlock :: Block Annotation -> State Lalala (Block Annotation)
> insertUnitsInBlock x = do system <- gets linearSystem
>                           return $ transformBi' (insertUnits system) x

> lookupUnit :: LinearSystem -> Int -> UnitConstant
> lookupUnit (matrix, vector) m = maybe (Unitful []) (\n -> vector !! (n - 1)) n
>   where n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]

> insertUnits :: LinearSystem -> Decl Annotation -> Decl Annotation
> insertUnits system (Decl a sp@(s1, s2) d t) | not (pRefactored a || hasUnits t) =
>   DSeq a (NullDecl a' sp'') (foldr1 (DSeq a) decls)
>   where unitVar' (Var a' _ _, _) = unitVar a'
>         sameUnits = (==) `on` (lookupUnit system . unitVar')
>         groups = groupBy sameUnits d
>         types = map (insertUnit system t . unitVar' . head) groups
>         a' = a { refactored = Just s1 }
>         sp' = dropLine $ refactorSpan sp
>         sp'' = (toCol0 s1, snd $ dropLine sp)
>         decls = [Decl a' sp' group t' | (group, t') <- zip groups types]
> insertUnits _ decl = decl

> hasUnits :: Type a -> Bool
> hasUnits (BaseType _ _ attrs _ _) = any isUnit attrs
> hasUnits _ = False

> isUnit :: Attr a -> Bool
> isUnit (MeasureUnit _ _) = True
> isUnit _ = False

> insertUnit :: LinearSystem -> Type Annotation -> Int -> Type Annotation
> insertUnit system (BaseType aa tt attrs kind len) uv =
>   BaseType aa tt (insertUnit' unit attrs) kind len
>   where unit = lookupUnit system uv

> insertUnit' :: UnitConstant -> [Attr Annotation] -> [Attr Annotation]
> insertUnit' unit attrs = attrs ++ [MeasureUnit unitAnnotation $ makeUnitSpec unit]

> makeUnitSpec :: UnitConstant -> MeasureUnitSpec Annotation
> makeUnitSpec (Unitful []) = UnitNone unitAnnotation
> makeUnitSpec (Unitful units)
>   | null neg = UnitProduct unitAnnotation $ formatUnits pos
>   | otherwise = UnitQuotient unitAnnotation (formatUnits pos) (formatUnits neg)
>   where pos = filter (\(unit, r) -> r > 0) units
>         neg = [(unit, -r) | (unit, r) <- units, r < 0]

> formatUnits :: [(MeasureUnit, Rational)] -> [(MeasureUnit, Fraction Annotation)]
> formatUnits units = [(unit, toFraction r) | (unit, r) <- units]

> toFraction :: Rational -> Fraction Annotation
> toFraction 1 = NullFraction unitAnnotation
> toFraction r
>   | q == 1 = IntegerConst unitAnnotation $ show p
>   | otherwise = FractionConst unitAnnotation (show p) (show q)
>   where p = numerator r
>         q = denominator r

> data BinOpKind = AddOp | MulOp | DivOp | PowerOp | LogicOp | RelOp
> binOpKind :: BinOp a -> BinOpKind
> binOpKind (Plus _) = AddOp
> binOpKind (Minus _) = AddOp
> binOpKind (Mul _) = MulOp
> binOpKind (Div _) = DivOp
> binOpKind (Or _) = LogicOp
> binOpKind (And _) = LogicOp
> binOpKind (Concat _) = AddOp
> binOpKind (Power _) = PowerOp
> binOpKind (RelEQ _) = RelOp
> binOpKind (RelNE _) = RelOp
> binOpKind (RelLT _) = RelOp
> binOpKind (RelLE _) = RelOp
> binOpKind (RelGT _) = RelOp
> binOpKind (RelGE _) = RelOp

> addCol :: UnitVarCategory -> State Lalala Int
> addCol category =
>   do (matrix, vector) <- gets linearSystem
>      let m = ncols matrix + 1
>      linearSystem =: (extendTo 0 m matrix, vector)
>      unitVarCats =. (++ [category])
>      return m

> addRow :: State Lalala Int
> addRow =
>   do (matrix, vector) <- gets linearSystem
>      let n = nrows matrix + 1
>      linearSystem =: (extendTo n 0 matrix, vector ++ [Unitful []])
>      return n

> liftLalala :: (Matrix Rational -> Matrix Rational) -> Lalala -> Lalala
> liftLalala f = Data.Label.modify linearSystem $ \(matrix, vector) -> (f matrix, vector)

> mustEqual :: State Lalala UnitVariable -> State Lalala UnitVariable -> State Lalala UnitVariable
> mustEqual uvm1 uvm2 =
>   do UnitVariable uv1 <- uvm1
>      UnitVariable uv2 <- uvm2
>      n <- addRow
>      modify $ liftLalala $ setElem (-1) (n, uv1) . setElem 1 (n, uv2)
>      solveSystemM
>      return $ UnitVariable uv1

> mustAddUp :: State Lalala UnitVariable -> State Lalala UnitVariable -> Rational -> Rational -> State Lalala UnitVariable
> mustAddUp uvm1 uvm2 k1 k2 =
>   do m <- addCol Temporary
>      UnitVariable uv1 <- uvm1
>      UnitVariable uv2 <- uvm2
>      n <- addRow
>      modify $ liftLalala $ setElem (-1) (n, m) . setElem k1 (n, uv1) . setElem k2 (n, uv2)
>      solveSystemM
>      return $ UnitVariable m

TODO: error handling in powerUnits

> powerUnits :: State Lalala UnitVariable -> Expr a -> State Lalala UnitVariable
> powerUnits uvm (Con _ _ powerString) =
>   do let power = fromInteger $ read powerString
>      m <- addCol Temporary
>      UnitVariable uv <- uvm
>      n <- addRow
>      modify $ liftLalala $ setElem (-1) (n, m) . setElem power (n, uv)
>      solveSystemM
>      return $ UnitVariable m

> sqrtUnits :: State Lalala UnitVariable -> State Lalala UnitVariable
> sqrtUnits uvm =
>   do m <- addCol Temporary
>      UnitVariable uv <- uvm
>      n <- addRow
>      modify $ liftLalala $ setElem (-1) (n, m) . setElem 0.5 (n, uv)
>      solveSystemM
>      return $ UnitVariable m

> anyUnits :: UnitVarCategory -> State Lalala UnitVariable
> anyUnits category =
>   do m <- addCol category
>      return $ UnitVariable m

> inferExprUnits :: Expr a -> State Lalala UnitVariable
> inferExprUnits (Con _ _ _) = anyUnits Literal
> inferExprUnits (ConL _ _ _ _) = anyUnits Literal
> inferExprUnits (ConS _ _ _) = anyUnits Literal
> inferExprUnits (Var _ _ names) =
>   do uenv <- gets unitVarEnv
>      let (VarName _ v, _) = head names
>          Just uv = lookup v uenv
>      sequence_ [mapM_ inferExprUnits exprs | (_, exprs) <- names]
>      return uv
> inferExprUnits (Bin _ _ op e1 e2) = do let uv1 = inferExprUnits e1
>                                            uv2 = inferExprUnits e2
>                                        case binOpKind op of
>                                          AddOp -> mustEqual uv1 uv2
>                                          MulOp -> mustAddUp uv1 uv2 1 1
>                                          DivOp -> mustAddUp uv1 uv2 1 (-1)
>                                          PowerOp -> powerUnits uv1 e2
>                                          LogicOp -> mustEqual uv1 uv2
>                                          RelOp -> do mustEqual uv1 uv2
>                                                      return $ UnitVariable 1
> inferExprUnits (Unary _ _ _ e) = inferExprUnits e
> inferExprUnits (CallExpr _ _ e1 (ArgList _ e2)) = do inferExprUnits e1
>                                                      inferExprUnits e2
>                                                      anyUnits Temporary
> inferExprUnits (NullExpr _ _) = anyUnits Temporary
> inferExprUnits (Null _ _) = return $ UnitVariable 1
> inferExprUnits (ESeq _ _ e1 e2) = do inferExprUnits e1
>                                      inferExprUnits e2
>                                      return $ UnitVariable 1
> inferExprUnits (Bound _ _ e1 e2) = mustEqual (inferExprUnits e1) (inferExprUnits e2)
> inferExprUnits (Sqrt _ _ e) = sqrtUnits $ inferExprUnits e
> inferExprUnits (ArrayCon _ _ (e:exprs)) =
>   do uv <- inferExprUnits e
>      mapM_ (mustEqual (return uv) . inferExprUnits) exprs
>      return uv
> inferExprUnits (AssgExpr _ _ _ e) = inferExprUnits e

> handleExpr :: Expr a -> State Lalala (Expr a)
> handleExpr x = do inferExprUnits x
>                   return x

> inferForHeaderUnits :: (Variable, Expr a, Expr a, Expr a) -> State Lalala ()
> inferForHeaderUnits (v, e1, e2, e3) =
>   do uenv <- gets unitVarEnv
>      let Just uv = lookup v uenv
>      mustEqual (return uv) (inferExprUnits e1)
>      mustEqual (return uv) (inferExprUnits e2)
>      mustEqual (return uv) (inferExprUnits e3)
>      return ()

> inferSpecUnits :: Data a => [Spec a] -> State Lalala ()
> inferSpecUnits = mapM_ $ descendBiM' handleExpr

> inferStmtUnits :: Data a => Fortran a -> State Lalala ()
> inferStmtUnits (Assg _ _ e1 e2) =
>   do mustEqual (inferExprUnits e1) (inferExprUnits e2)
>      return ()
> inferStmtUnits (For _ _ _ (NullExpr _ _) _ _ s) = inferStmtUnits s
> inferStmtUnits (For _ _ (VarName _ v) e1 e2 e3 s) =
>   do inferForHeaderUnits (v, e1, e2, e3)
>      inferStmtUnits s
> inferStmtUnits (FSeq _ _ s1 s2) = mapM_ inferStmtUnits [s1, s2]
> inferStmtUnits (If _ _ e1 s1 elseifs ms2) =
>   do inferExprUnits e1
>      inferStmtUnits s1
>      sequence_ [inferExprUnits e >> inferStmtUnits s | (e, s) <- elseifs]
>      case ms2 of
>        Just s2 -> inferStmtUnits s2
>        Nothing -> return ()
> inferStmtUnits (Allocate _ _ e1 e2) = mapM_ inferExprUnits [e1, e2]
> inferStmtUnits (Backspace _ _ specs) = inferSpecUnits specs
> inferStmtUnits (Call _ _ e1 (ArgList _ e2)) = mapM_ inferExprUnits [e1, e2]
> inferStmtUnits (Open _ _ specs) = inferSpecUnits specs
> inferStmtUnits (Close _ _ specs) = inferSpecUnits specs
> inferStmtUnits (Deallocate _ _ exprs e) =
>   do mapM_ inferExprUnits exprs
>      inferExprUnits e
>      return ()
> inferStmtUnits (Endfile _ _ specs) = inferSpecUnits specs
> inferStmtUnits (Forall _ _ (header, e) s) =
>   do mapM_ inferForHeaderUnits header
>      inferExprUnits e
>      inferStmtUnits s
> inferStmtUnits (Nullify _ _ exprs) = mapM_ inferExprUnits exprs
> inferStmtUnits (Inquire _ _ specs exprs) =
>   do inferSpecUnits specs
>      mapM_ inferExprUnits exprs
> inferStmtUnits (Rewind _ _ specs) = inferSpecUnits specs
> inferStmtUnits (Stop _ _ e) =
>   do inferExprUnits e
>      return ()
> inferStmtUnits (Where _ _ e s) =
>   do inferExprUnits e
>      inferStmtUnits s
> inferStmtUnits (Write _ _ specs exprs) =
>   do inferSpecUnits specs
>      mapM_ inferExprUnits exprs
> inferStmtUnits (PointerAssg _ _ e1 e2) =
>   do mustEqual (inferExprUnits e1) (inferExprUnits e2)
>      return ()
> inferStmtUnits (Return _ _ e) =
>   do inferExprUnits e
>      return ()
> inferStmtUnits (Label _ _ _ s) = inferStmtUnits s
> inferStmtUnits (Print _ _ e exprs) = mapM_ inferExprUnits (e:exprs)
> inferStmtUnits (ReadS _ _ specs exprs) =
>   do inferSpecUnits specs
>      mapM_ inferExprUnits exprs

> handleStmt :: Data a => Fortran a -> State Lalala (Fortran a)
> handleStmt x = do inferStmtUnits x
>                   return x
