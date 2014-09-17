{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Extensions.Units where

import Data.Ratio
import Data.Maybe
import Data.Matrix
import Data.List
import Data.Label.Mono (Lens)
import Data.Label.Monadic hiding (modify)
import qualified Data.Label
import Data.Function
import Data.Data
import Data.Char
import Control.Monad.State.Strict hiding (gets)

import Data.Generics.Uniplate.Operations

import Analysis.Annotations
import Analysis.Types
import Transformation.Syntax
import Language.Fortran

import Test.QuickCheck

import qualified Debug.Trace as D

import Helpers

instance (Arbitrary a) => Arbitrary (Matrix a) where
    arbitrary = sized (\n -> do xs <- vectorOf (n*n) arbitrary
                                return $ matrix n n (\(i, j) -> xs !! ((i-1)*n + (j-1))))
    

data UnitConstant = Unitful [(MeasureUnit, Rational)] | Unitless Rational deriving (Eq, Show)
trim = filter $ \(unit, r) -> r /= 0

{- Treat 'UnitConstant's as numbers -}

instance Num UnitConstant where
  (Unitful u1) + (Unitful u2) = Unitful $ trim $ merge u1 u2
    where merge [] u2 = u2
          merge u1 [] = u1
          merge ((unit1, r1) : u1) ((unit2, r2) : u2)
            | unit1 == unit2 = (unit1, r1 + r2) : merge u1 u2
            | unit1 <  unit2 = (unit1, r1) : merge u1 ((unit2, r2) : u2)
            | otherwise      = (unit2, r2) : merge ((unit1, r1) : u1) u2
  (Unitless n1) + (Unitless n2) = Unitless (n1 + n2)
  (Unitful units) * (Unitless n) = Unitful $ trim [(unit, r * n) | (unit, r) <- units]
  (Unitless n) * (Unitful units) = Unitful $ trim [(unit, n * r) | (unit, r) <- units]
  (Unitless n1) * (Unitless n2) = Unitless (n1 * n2)
  negate (Unitful units) = Unitful [(unit, -r) | (unit, r) <- units]
  negate (Unitless n) = Unitless (-n)
  abs (Unitful units) = Unitful [(unit, abs r) | (unit, r) <- units]
  abs (Unitless n) = Unitless $ abs n
  signum (Unitful units) = Unitful [(unit, signum r) | (unit, r) <- units]
  signum (Unitless n) = Unitless $ signum n
  fromInteger = Unitless . fromInteger

{- Treat 'UnitConstant's as fractionals -}
instance Fractional UnitConstant where
  (Unitful units) / (Unitless n) = Unitful [(unit, r / n) | (unit, r) <- units]
  (Unitless n1) / (Unitless n2) = Unitless (n1 / n2)
  fromRational = Unitless . fromRational

newtype UnitVariable = UnitVariable Int deriving Show
data UnitVarCategory = Literal | Temporary | Variable | Argument | Magic deriving Eq
type UnitVarEnv = [(Variable, (UnitVariable, [UnitVariable]))]
type DerivedUnitEnv = [(MeasureUnit, UnitConstant)]
type ProcedureNames = (String, Maybe Variable, [Variable])
type Procedure = (Maybe UnitVariable, [UnitVariable])
type ProcedureEnv = [(String, Procedure)]
type LinearSystem = (Matrix Rational, [UnitConstant])

data UnitEnv = UnitEnv {
  _report              :: [String],
  _unitVarEnv          :: UnitVarEnv,
  _derivedUnitEnv      :: DerivedUnitEnv,
  _procedureEnv        :: ProcedureEnv,
  _calls               :: ProcedureEnv,
  _unitVarCats         :: [UnitVarCategory],
  _reorderedCols       :: [Int],
  _underdeterminedCols :: [Int],
  _linearSystem        :: LinearSystem
}
emptyUnitEnv = UnitEnv [] [] [] [] [] [Magic] [] [] (fromLists [[1]], [Unitful []])
Data.Label.mkLabels [''UnitEnv]

infix 2 <<
(<<) :: MonadState f m => Lens (->) f [o] -> o -> m ()
(<<) lens o = lens =. (o:)
--(<<) lens o = lens =. (++ [o])

prepend :: MonadState f m => Lens (->) f [o] -> o -> m ()
prepend lens o = lens =. (o:)

descendBiM' :: (Monad m, Data (from a), Data (to a)) => (to a -> m (to a)) -> from a -> m (from a)
descendBiM' f x = descendBiM f x

inverse :: [Int] -> [Int]
inverse perm = [j + 1 | Just j <- map (flip elemIndex perm) [1 .. length perm]]

fixValue :: Eq a => (a -> a) -> a -> a
fixValue f x = snd $ until (uncurry (==)) (\(x, y) -> (y, f y)) (x, f x)

-- The indexing for switchScaleElems and moveElem is 1-based, in line with Data.Matrix.

switchScaleElems :: Num a => Int -> Int -> a -> [a] -> [a]
switchScaleElems i j factor list = a ++ factor * b : c
  where (lj, b:rj) = splitAt (j - 1) list
        (a, _:c) = splitAt (i - 1) (lj ++ list !! (i - 1) : rj)


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
                                                                    

solveSystemM :: String -> State UnitEnv Bool
solveSystemM adjective =
  do system <- gets linearSystem
     case solveSystem system of
       Just system' -> linearSystem =: system' >> return True
       Nothing      -> report << (adjective ++ " units of measure") >> return False

solveSystem :: LinearSystem -> Maybe LinearSystem
solveSystem system = solveSystem' system 1 1

solveSystem' :: LinearSystem -> Int -> Int -> Maybe LinearSystem
solveSystem' (matrix, vector) m k
  | m > ncols matrix = fmap (cutSystem k) $ checkSystem (matrix, vector) k
  | otherwise = elimRow (matrix, vector) n m k
                where n = find (\n -> matrix ! (n, m) /= 0) [k .. nrows matrix]

cutSystem :: Int -> LinearSystem -> LinearSystem
cutSystem k (matrix, vector) = (matrix', vector')
  where matrix' = submatrix 1 (k - 1) 1 (ncols matrix) matrix
        vector' = take (k - 1) vector

checkSystem :: LinearSystem -> Int -> Maybe LinearSystem
checkSystem (matrix, vector) k
  | k > nrows matrix = Just (matrix, vector)
  | vector !! (k - 1) /= Unitful [] = Nothing
  | otherwise = checkSystem (matrix, vector) (k + 1)

elimRow :: LinearSystem -> Maybe Int -> Int -> Int -> Maybe LinearSystem
elimRow system Nothing m k = solveSystem' system (m + 1) k
elimRow (matrix, vector) (Just n) m k = solveSystem' system' (m + 1) (k + 1)
  where matrix' = let s = matrix ! (n, m) in 
                    (if (k == n) then id else switchRows k n)
                       (if s == 1 then matrix else scaleRow (recip $ s) n matrix)
        vector' = switchScaleElems k n (fromRational $ recip $ matrix ! (n, m)) vector
        system' = elimRow' (matrix', vector') k m

elimRow' :: LinearSystem -> Int -> Int -> LinearSystem
elimRow' (matrix, vector) k m = (matrix', vector')
  where mstep matrix n = let s = (- matrix ! (n, m)) in if s == 0 then matrix else combineRows n s k matrix 
        matrix' = foldl mstep matrix $ [1 .. k - 1] ++ [k + 1 .. nrows matrix]
        vector'' = [x - fromRational (matrix ! (n, m)) * vector !! (k - 1) | (n, x) <- zip [1..] vector]
        (a, _ : b) = splitAt (k - 1) vector''
        vector' = a ++ vector !! (k - 1) : b

checkUnderdeterminedM :: State UnitEnv ()
checkUnderdeterminedM = do ucats <- gets unitVarCats
                           system <- gets linearSystem
                           uvarenv <- gets unitVarEnv
                           let badCols = checkUnderdetermined ucats system
                           -- (show system) `D.trace` (unless (null badCols) $ report << "underdetermined units of measure: " ++ show badCols ++ " - " ++ show uvarenv)
                           report << "please give annotation to variables " ++ (show (lookupUnderdeterminedVariables uvarenv badCols))
                           underdeterminedCols =: badCols

lookupUnderdeterminedVariables :: UnitVarEnv -> [Int] -> [String]
lookupUnderdeterminedVariables uenv badcols = 
      mapMaybe (\j -> lookupEnv j uenv) badcols
         where lookupEnv j [] = Nothing
               lookupEnv j ((v, (UnitVariable i, _)):uenv)
                                              | i == j    = Just v 
                                              | otherwise = lookupEnv j uenv

checkUnderdetermined :: [UnitVarCategory] -> LinearSystem -> [Int]
checkUnderdetermined ucats system@(matrix, vector) =
  fixValue (propagateUnderdetermined matrix) $ checkUnderdetermined' ucats system 1

checkUnderdetermined' :: [UnitVarCategory] -> LinearSystem -> Int -> [Int]
checkUnderdetermined' ucats system@(matrix, vector) n
  | n > nrows matrix = []
  | not (null $ drop 1 ms) && vector !! (n - 1) /= Unitful [] = ms ++ rest
  | otherwise = rest
  where ms = filter significant [1 .. ncols matrix]
        significant m = matrix ! (n, m) /= 0 && ucats !! (m - 1) `notElem` [Literal, Argument]
        rest = checkUnderdetermined' ucats system (n + 1)

propagateUnderdetermined :: Matrix Rational -> [Int] -> [Int]
propagateUnderdetermined matrix list =
  nub $ do
    m <- list
    n <- filter (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
    filter (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]


intrinsicsDict = 
    map (\x -> (x, addPlain2ArgIntrinsic)) ["ABS", "ACHAR", "ADJUSTL", "ADJUSTR", "AIMAG", "AINT", "ALL", "ANINT", "ANY", "CEILING", "CHAR", "CONJG", "DBLE", "EPSILON", "FLOOR", "FRACTION", "HUGE", "IACHAR", "IALL", "IANY", "ICHAR", "INT", "IPARITY", "LOGICAL", "MAXEXPONENT", "MAXVAL", "MINEXPONENT", "MINVAL", "NEW_LINE", "NINT", "NORM2", "NOT", "NULL", "PARITY", "REAL", "RRSPACING", "SPACING", "SUM", "TINY", "TRANSPOSE", "TRIM"]
    
 ++ map (\x -> (x, addPlain2ArgIntrinsic)) ["CMPLX", "DCOMPLX", "DIM", "HYPOT", "IAND", "IEOR", "IOR", "MAX", "MIN"] 
    
 ++ map (\x -> (x, addPlain1Arg1ExtraIntrinsic)) ["CSHIFT", "EOSHIFT", "IBCLR", "IBSET", "MOD", "MODULO", "NEAREST", "PACK", "REPEAT", "RESHAPE", "SHIFTA", "SHIFTL", "SHIFTR", "SIGN"]

 ++ map (\x -> (x, addPlain2Arg1ExtraIntrinsic)) ["DSHIFTL", "DSHIFTR", "ISHFT", "ISHFTC", "MERGE", "MERGE_BITS"]

 ++ map (\x -> (x, addProductIntrinsic)) ["DOT_PRODUCT", "DPROD", "MATMUL"]

 ++ map (\x -> (x, addPowerIntrinsic)) ["SCALE", "SET_EXPONENT"]

 ++ map (\x -> (x, addUnitlessIntrinsic)) ["ACOS", "ACOSH", "ASIN", "ASINH", "ATAN", "ATANH", "BESSEL_J0", "BESSEL_J1", "BESSEL_Y0", "BESSEL_Y1", "COS", "COSH", "ERF", "ERFC", "ERFC_SCALED", "EXP", "EXPONENT", "GAMMA", "LOG", "LOG10", "LOG_GAMMA", "PRODUCT", "SIN", "SINH", "TAN", "TANH"]

 ++ map (\x -> (x, addUnitlessSubIntrinsic)) ["CPU_TIME", "RANDOM_NUMBER"]

 ++ map (\x -> (x, addUnitlessResult0ArgIntrinsic)) ["COMMAND_ARGUMENT_COUNT", "COMPILER_OPTIONS", "COMPILER_VERSION"]

 ++ map (\x -> (x, addUnitlessResult1ArgIntrinsic)) ["ALLOCATED", "ASSOCIATED", "BIT_SIZE", "COUNT", "DIGITS", "IS_IOSTAT_END", "IS_IOSTAT_EOR", "KIND", "LBOUND", "LCOBOUND", "LEADZ", "LEN", "LEN_TRIM", "MASKL", "MASKR", "MAXLOC", "MINLOC", "POPCOUNT", "POPPAR", "PRECISION", "PRESENT", "RADIX", "RANGE", "SELECTED_CHAR_KIND", "SELECTED_INT_KIND", "SELECTED_REAL_KIND", "SHAPE", "SIZE", "STORAGE_SIZE", "TRAILZ", "UBOUND", "UCOBOUND"]

 ++ map (\x -> (x, addUnitlessResult2SameArgIntrinsic)) ["ATAN2", "BGE", "BGT", "BLE", "BLT", "INDEX", "LGE", "LGT", "LLE", "LLT", "SCAN", "VERIFY"]

 ++ map (\x -> (x, addUnitlessResult2AnyArgIntrinsic)) ["BTEST", "EXTENDS_TYPE_OF", "SAME_TYPE_AS"]

     -- missing: ATOMIC_DEFINE, ATOMIC_REF, BESSEL_JN, BESSEL_YN, C_*, DATE_AND_TIME, EXECUTE_COMMAND_LINE, GET_COMMAND, GET_COMMAND_ARGUMENT, GET_ENVIRONMENT_VARIABLE, IBITS, any of the image stuff, MOVE_ALLOC, MVBITS, RANDOM_SEED, SPREAD, SYSTEM_CLOCK, TRANSFER, UNPACK


{- [A] Various helpers for adding information about procedures to the type system -}

addPlain1ArgIntrinsic :: String -> State UnitEnv ()
addPlain1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual result arg
     procedureEnv << (name, (Just result, [arg]))

addPlain2ArgIntrinsic :: String -> State UnitEnv ()
addPlain2ArgIntrinsic name =
  do result <- anyUnits Variable
     arg1  <- anyUnits Argument
     arg2  <- anyUnits Argument
     mustEqual result arg1
     mustEqual result arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain1Arg1ExtraIntrinsic :: String -> State UnitEnv ()
addPlain1Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual result arg1
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPlain2Arg1ExtraIntrinsic :: String -> State UnitEnv ()
addPlain2Arg1ExtraIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     arg3   <- anyUnits Argument
     mustEqual result arg1
     mustEqual result arg2
     procedureEnv << (name, (Just result, [arg1, arg2, arg3]))

addProductIntrinsic :: String -> State UnitEnv ()
addProductIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     temp   <- mustAddUp arg1 arg2 1 1
     mustEqual result temp
     procedureEnv << (name, (Just result, [arg1, arg2]))

addPowerIntrinsic :: String -> State UnitEnv ()
addPowerIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual result arg1
     mustEqual arg2 (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessIntrinsic :: String -> State UnitEnv ()
addUnitlessIntrinsic name =
  do result <- anyUnits Variable
     arg    <- anyUnits Argument
     mustEqual result (UnitVariable 1)
     mustEqual arg (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessSubIntrinsic :: String -> State UnitEnv ()
addUnitlessSubIntrinsic name =
  do arg <- anyUnits Variable
     mustEqual arg (UnitVariable 1)
     procedureEnv << (name, (Nothing, [arg]))

addUnitlessResult0ArgIntrinsic :: String -> State UnitEnv ()
addUnitlessResult0ArgIntrinsic name =
  do result <- anyUnits Variable
     mustEqual result (UnitVariable 1)
     procedureEnv << (name, (Just result, []))

addUnitlessResult1ArgIntrinsic :: String -> State UnitEnv ()
addUnitlessResult1ArgIntrinsic name =
  do result <- anyUnits Variable
     arg <- anyUnits Argument
     mustEqual result (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg]))

addUnitlessResult2AnyArgIntrinsic :: String -> State UnitEnv ()
addUnitlessResult2AnyArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual result (UnitVariable 1)
     procedureEnv << (name, (Just result, [arg1, arg2]))

addUnitlessResult2SameArgIntrinsic :: String -> State UnitEnv ()
addUnitlessResult2SameArgIntrinsic name =
  do result <- anyUnits Variable
     arg1   <- anyUnits Argument
     arg2   <- anyUnits Argument
     mustEqual result (UnitVariable 1)
     mustEqual arg1 arg2
     procedureEnv << (name, (Just result, [arg1, arg2]))


removeUnits :: (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
removeUnits (fname, x) = ("", (fname, doRemoveUnits x))

doRemoveUnits :: Program Annotation -> Program Annotation
doRemoveUnits = map (descendBi removeUnitsInBlock)

inferUnitsInProgUnit :: ProgUnit Annotation -> State UnitEnv (ProgUnit Annotation)
inferUnitsInProgUnit x =
  do --uenv <- gets unitVarEnv
     ps <- mapM inferUnitsInProgUnit $ (progUnits x)
     b <- enterBlock $ block x
     --unitVarEnv =: uenv
     return $ refillProgUnits (refillBlock x b) ps
  where enterBlock Nothing = return Nothing
        enterBlock (Just (block, proc)) = fmap Just $ blockUnitEnv block proc

block :: ProgUnit Annotation -> Maybe (Block Annotation, Maybe ProcedureNames)
block (Main x sp n a b ps)                            = Just (b, Nothing)
block (Sub x sp t (SubName _ n) (Arg _ a _) b)        = Just (b, Just (n, Nothing, argNames a))
block (Function x sp t (SubName _ n) (Arg _ a _) r b) = ("--# " ++ show (resultName n r)) `D.trace` Just (b, Just (n, Just (resultName n r), argNames a))
block x                                               = Nothing

progUnits :: ProgUnit Annotation -> [ProgUnit Annotation]
progUnits (Main x sp n a b ps)     = ps
progUnits (Module x sp n u i d ps) = ps
progUnits (PSeq x sp p1 p2)        = [p1, p2]
progUnits (Prog x sp p)            = [p]
progUnits x                        = []

refillBlock :: ProgUnit Annotation -> Maybe (Block Annotation) -> ProgUnit Annotation
refillBlock (Main x sp n a _ ps)      (Just b) = Main x sp n a b ps
refillBlock (Sub x sp t n a _)        (Just b) = Sub x sp t n a b
refillBlock (Function x sp t n a r _) (Just b) = Function x sp t n a r b
refillBlock x                          _       = x

refillProgUnits :: ProgUnit Annotation -> [ProgUnit Annotation] -> ProgUnit Annotation
refillProgUnits (Main x sp n a b _)     ps       = Main x sp n a b ps
refillProgUnits (Module x sp n u i d _) ps       = Module x sp n u i d ps
refillProgUnits (PSeq x sp _ _)         [p1, p2] = PSeq x sp p1 p2
refillProgUnits (Prog x sp _)           [p]      = Prog x sp p
refillProgUnits x                       _        = x

argNames :: ArgName a -> [Variable]
argNames (ArgName _ n) = [n]
argNames (ASeq _ n1 n2) = argNames n1 ++ argNames n2
argNames (NullArg _) = []

resultName :: Variable -> Maybe (VarName a) -> Variable
resultName n Nothing = n
resultName _ (Just (VarName _ r)) = r

blockUnitEnv :: Block Annotation -> Maybe ProcedureNames -> State UnitEnv (Block Annotation)
blockUnitEnv x proc = do y <- enterDecls x proc
                         descendBiM' handleStmt y
                         return y

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
     case lookup (map toUpper unit) denv of
       Just uc' -> return $ uc' * (fromRational f)
       Nothing  -> derivedUnitEnv << (map toUpper unit, uc 1) >> return (uc f)

fromFraction :: Fraction a -> Rational
fromFraction (IntegerConst _ n) = fromInteger $ read n
fromFraction (FractionConst _ p q) = fromInteger (read p) / fromInteger (read q)
fromFraction (NullFraction _) = 1

extractUnit :: Attr a -> [State UnitEnv UnitConstant]
extractUnit attr = case attr of
                     MeasureUnit _ unit -> [convertUnit unit]
                     _ -> []

extendConstraints :: [UnitConstant] -> LinearSystem -> LinearSystem
extendConstraints units (matrix, vector) =
  case units of
    [] -> (extendTo 0 0 m matrix, vector)
    _ -> (setElem 1 (n, m) $ extendTo 0 n m matrix, vector ++ [last units])
  where n = nrows matrix + 1
        m = ncols matrix + 1

enterDecls :: Block Annotation -> Maybe ProcedureNames -> State UnitEnv (Block Annotation)
enterDecls x proc =
  do
    y <- transformBiM f x
    addProcedure proc
    return y
  where
    addProcedure Nothing = return ()
    addProcedure (Just (name, resultName, argNames)) = ("add p " ++ show name) `D.trace`
      do uenv <- gets unitVarEnv
         let resultVar = fmap (lookupUnitByName uenv) resultName
             argVars = fmap (lookupUnitByName uenv) argNames
         procedureEnv << (map toUpper name, (resultVar, argVars))
    lookupUnitByName uenv v = maybe (UnitVariable 1) fst $ lookup (map toUpper v) uenv
    f (Decl a s d t) =
      do
        let BaseType _ _ attrs _ _ = arrayElementType t
        units <- sequence $ concatMap extractUnit attrs
        d' <- mapM (\(e1, e2, multiplier) -> do (e1', e2') <- dm units (e1, e2)
                                                return (e1', e2', multiplier)) d
        return $ Decl a s d' t
      where
        dm units (Var a s names, e) = do
          let (VarName _ v, es) = head names
          system <- gets linearSystem
          let m = ncols (fst system) + 1
          unitVarCats << unitVarCat v
          linearSystem =. extendConstraints units
          ms <- case toArrayType t es of
                  ArrayT _ bounds _ _ _ _ -> mapM (const $ fmap UnitVariable $ addCol Variable) bounds
                  _ -> return []
          prepend unitVarEnv (map toUpper v, (UnitVariable m, ms))
          uv <- inferExprUnits e
          mustEqual (UnitVariable m) uv
          return (Var a { unitVar = m } s names, e)
        unitVarCat v
          | Just (n, r, args) <- proc, v `elem` args = Argument
          | otherwise = Variable
    f x@(MeasureUnitDef a s d) =
      do
        mapM_ learnDerivedUnit d
        return x
      where
        learnDerivedUnit (name, spec) =
          do denv <- gets derivedUnitEnv
             when (isJust $ lookup (map toUpper name) denv) $ error "Redeclared unit of measure"
             unit <- convertUnit spec
             denv <- gets derivedUnitEnv
             when (isJust $ lookup (map toUpper name) denv) $ error "Recursive unit-of-measure definition"
             derivedUnitEnv << (map toUpper name, unit)
    f x = return x

inferUnits :: (Filename, Program Annotation) -> (Report, (Filename, Program Annotation))
inferUnits (fname, x) = (r, (fname, y))
  where (y, env) = runState (doInferUnits x) emptyUnitEnv
        r = concat [fname ++ ": " ++ r ++ "\n" | r <- Data.Label.get report env] ++ 
            "\n" ++ fname ++ ": checked/inferred " ++ (show $ length $ snd $ _linearSystem env) ++ " variables\n"

doInferUnits :: Program Annotation -> State UnitEnv (Program Annotation)
doInferUnits x = do y <- mapM inferUnitsInProgUnit x
                    z <- inferInterproceduralUnits y
                    mapM (descendBiM' insertUnitsInBlock) z

insertUnitsInBlock :: Block Annotation -> State UnitEnv (Block Annotation)
insertUnitsInBlock x = do env <- get
                          return $ transformBi (insertUnits env) x

removeUnitsInBlock :: Block Annotation -> Block Annotation
removeUnitsInBlock = transformBi deleteUnits

lookupUnit :: [UnitVarCategory] -> [Int] -> LinearSystem -> Int -> Maybe UnitConstant
lookupUnit ucats badCols system@(matrix, vector) m =
  maybe defaultUnit (lookupUnit' ucats badCols system m) n
  where n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
        defaultUnit = if ucats !! (m - 1) == Argument then Nothing else Just (Unitful [])

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
        order = Data.Label.get reorderedCols env
        ucats = Data.Label.get unitVarCats env
        badCols = Data.Label.get underdeterminedCols env
        unitVar' (Var a' _ _, _, _) = realColumn order $ UnitVariable $ unitVar a'
        sameUnits = (==) `on` (lookupUnit ucats badCols system . unitVar')
        groups = groupBy sameUnits d
        types = map (insertUnit ucats badCols system t . unitVar' . head) groups
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

inferInterproceduralUnits :: Program Annotation -> State UnitEnv (Program Annotation)
inferInterproceduralUnits x =
  do -- reorderColumns
     solveSystemM "inconsistent"
     system <- gets linearSystem
     inferInterproceduralUnits' x False system

inferInterproceduralUnits' :: Program Annotation -> Bool -> LinearSystem -> State UnitEnv (Program Annotation)
inferInterproceduralUnits' x haveAssumedLiterals system1 =
  do addInterproceduralConstraints x
     consistent <- solveSystemM "inconsistent"
     if not consistent then 
          do  linearSystem =: system1
              return x
      else do
        system2 <- gets linearSystem
        if system1 == system2
          then checkUnderdeterminedM >> nextStep
          else inferInterproceduralUnits' x haveAssumedLiterals system2
  where nextStep | haveAssumedLiterals = return x
                 | otherwise           = do consistent <- assumeLiteralUnits
                                            if not consistent
                                             then return x
                                             else do system3 <- gets linearSystem
                                                     inferInterproceduralUnits' x True system3

assumeLiteralUnits :: State UnitEnv Bool
assumeLiteralUnits =
  do system@(matrix, vector) <- gets linearSystem
     mapM_ assumeLiteralUnits' [1 .. ncols matrix]
     consistent <- solveSystemM "underdetermined"
     when (not consistent) $ linearSystem =: system
     return consistent
  where
    assumeLiteralUnits' m =
      do (matrix, vector) <- gets linearSystem
         ucats <- gets unitVarCats
         let n = find (\n -> matrix ! (n, m) /= 0) [1 .. nrows matrix]
             m' = n >>= (\n -> find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix])
             nonLiteral n m = matrix ! (n, m) /= 0 && ucats !! (m - 1) /= Literal
             m's = n >>= (\n -> find (nonLiteral n) [1 .. ncols matrix])
         when (ucats !! (m - 1) == Literal && (m' /= Just m || isJust m's)) $ do
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
         case lookup (map toUpper name) penv of
           Just (r, as) -> let (r1, r2) = decodeResult result r in handleArgs (args ++ r1) (as ++ r2)
           Nothing      -> return ()
    handleArgs actualVars dummyVars =
      do order <- gets reorderedCols
         let actual = map (realColumn order) actualVars
             dummy = map (realColumn order) dummyVars
         mapM_ (handleArg $ zip dummy actual) dummy
    handleArg dummyToActual dummy =
      do (matrix, vector) <- gets linearSystem
         let n = maybe 1 id $ find (\n -> matrix ! (n, dummy) /= 0) [1 .. nrows matrix]
             Just m = find (\m -> matrix ! (n, m) /= 0) [1 .. ncols matrix]
         when (m == dummy) $ do
           let ms = filter (\m -> matrix ! (n, m) /= 0) [m .. ncols matrix]
               m's = mapMaybe (flip lookup dummyToActual) ms
           when (length m's == length ms) $ do
             n' <- addRow' $ vector !! (n - 1)
             mapM_ (handleArgPair matrix n n') (zip ms m's)
    handleArgPair matrix n n' (m, m') = modify $ liftUnitEnv $ setElem (matrix ! (n, m)) (n', m')
    decodeResult (Just r1) (Just r2) = ([r1], [r2])
    decodeResult Nothing Nothing = ([], [])
    decodeResult (Just _) Nothing = error "Subroutine used as a function!"
    decodeResult Nothing (Just _) = error "Function used as a subroutine!"

realColumn :: [Int] -> UnitVariable -> Int
realColumn order (UnitVariable uv) = if null order then uv else order !! (uv - 1)

reorderColumns :: State UnitEnv ()
reorderColumns =
  do (matrix, vector) <- gets linearSystem
     reorderedCols =: [1 .. ncols matrix]
     reorderColumns' (ncols matrix) (ncols matrix)

reorderColumns' :: Int -> Int -> State UnitEnv ()
reorderColumns' m k
  | m < 1 = reorderColumns'' k k
  | otherwise =
      do (matrix, vector) <- gets linearSystem
         ucats <- gets unitVarCats
         k' <- if ucats !! (m - 1) == Argument
                 then do modify $ liftUnitEnv $ moveCol m k
                         unitVarCats =. moveElem m k
                         reorderedCols =. moveElem m k
                         return $ k - 1
                 else return k
         reorderColumns' (m - 1) k'

reorderColumns'' :: Int -> Int -> State UnitEnv ()
reorderColumns'' m k
  | m < 1 = reorderedCols =. inverse
  | otherwise =
      do (matrix, vector) <- gets linearSystem
         ucats <- gets unitVarCats
         k' <- if ucats !! (m - 1) == Literal
                 then do modify $ liftUnitEnv $ moveCol m k
                         unitVarCats =. moveElem m k
                         reorderedCols =. moveElem m k
                         return $ k - 1
                 else return k
         reorderColumns'' (m - 1) k'

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

addCol :: UnitVarCategory -> State UnitEnv Int
addCol category =
  do (matrix, vector) <- gets linearSystem
     let m = ncols matrix + 1
     linearSystem =: (extendTo 0 0 m matrix, vector)
     unitVarCats << category
     return m

addRow :: State UnitEnv Int
addRow = addRow' (Unitful [])

addRow' :: UnitConstant -> State UnitEnv Int
addRow' uc =
  do (matrix, vector) <- gets linearSystem
     let n = nrows matrix + 1
     linearSystem =: (extendTo 0 n 0 matrix, vector ++ [uc])
     return n

liftUnitEnv :: (Matrix Rational -> Matrix Rational) -> UnitEnv -> UnitEnv
liftUnitEnv f = Data.Label.modify linearSystem $ \(matrix, vector) -> (f matrix, vector)

mustEqual :: UnitVariable -> UnitVariable -> State UnitEnv UnitVariable
mustEqual (UnitVariable uv1) (UnitVariable uv2) = 
  do n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, uv1) . incrElem 1 (n, uv2)
     return $ UnitVariable uv1

mustAddUp :: UnitVariable -> UnitVariable -> Rational -> Rational -> State UnitEnv UnitVariable
mustAddUp (UnitVariable uv1) (UnitVariable uv2) k1 k2 =
  do m <- addCol Temporary
     n <- addRow
     modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem k1 (n, uv1) . incrElem k2 (n, uv2)
     return $ UnitVariable m

-- TODO: error handling in powerUnits

powerUnits :: Show a => UnitVariable -> Expr a -> State UnitEnv UnitVariable
powerUnits (UnitVariable uv) (Con _ _ powerString) =
  case fmap (fromInteger . fst) $ listToMaybe $ reads powerString of
    Just power -> do
      m <- addCol Temporary
      n <- addRow
      modify $ liftUnitEnv $ incrElem (-1) (n, m) . incrElem power (n, uv)
      return $ UnitVariable m
    Nothing -> mustEqual (UnitVariable uv) (UnitVariable 1)

powerUnits uv e =
  do mustEqual uv (UnitVariable 1)
     uv <- inferExprUnits e
     mustEqual uv (UnitVariable 1)

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

{-
class InferUnits t r where
    inferUnits :: t -> State UnitEnv r

instance InferUnits (Expr a) UnitVariable where
    inferUnits = inferExprUnits

instance InferUnits (Fortran a) () where
    inferUnits = inferStmtUnits
-}

inferExprUnits :: Show a => Expr a -> State UnitEnv UnitVariable
inferExprUnits (Con _ _ _) = anyUnits Literal
inferExprUnits (ConL _ _ _ _) = anyUnits Literal
inferExprUnits (ConS _ _ _) = anyUnits Literal
inferExprUnits (Var _ _ names) =
 do uenv <- gets unitVarEnv
    penv <- gets procedureEnv
    let (VarName _ v, args) = head names
    -- (v ++ " " ++  (show $ length args)) `D.trace`
    case lookup (map toUpper v) uenv of
       -- array variable?
       Just (uv, uvs@(_:_)) -> inferArgUnits' uvs >> return uv
       -- function call?
       Nothing | not (null args) -> do case (lookup (map toUpper v) intrinsicsDict) of
                                          Just fun -> (show v) `D.trace` fun (map toUpper v)
                                          Nothing  -> return ()
                                       uv <- anyUnits Temporary
                                       uvs <- inferArgUnits
                                       let uvs' = justArgUnits args uvs
                                       calls << (v, (Just uv, uvs'))
                                       return uv
       -- scalar variable or external function call?
       Just (uv, []) -> inferArgUnits >> return uv
       -- default specifier
       _ | v == "*" -> anyUnits Literal
       -- just bad code
       x -> case (lookup (map toUpper v) penv) of 
              Just (Just uv, argUnits) ->
                   if (null args) then (show uv ++ " - " ++ show argUnits) `D.trace` inferArgUnits' argUnits >> return uv
                   else  "blurg" `D.trace` 
                         do uv  <- anyUnits Temporary
                            uvs <- inferArgUnits 
                            let uvs' = justArgUnits args uvs
                            calls << (v, (Just uv, uvs'))
                            return uv

              Nothing -> error $ "Undefined variable " ++ show v ++ "(" ++ show x ++ ")"
  where inferArgUnits = sequence [mapM inferExprUnits exprs | (_, exprs) <- names]
        inferArgUnits' uvs = sequence [(inferExprUnits expr) >>= (\uv' -> mustEqual uv' uv) | ((_, exprs), uv) <- zip names uvs, expr <- exprs]
        justArgUnits [NullExpr _ _] _ = []  -- zero-argument function call
        justArgUnits _ uvs = head uvs
inferExprUnits (Bin _ _ op e1 e2) = do uv1 <- inferExprUnits e1
                                       uv2 <- inferExprUnits e2
                                       case binOpKind op of
                                         AddOp -> mustEqual uv1 uv2
                                         MulOp -> mustAddUp uv1 uv2 1 1
                                         DivOp -> mustAddUp uv1 uv2 1 (-1)
                                         PowerOp -> powerUnits uv1 e2
                                         LogicOp -> mustEqual uv1 uv2
                                         RelOp -> do mustEqual uv1 uv2
                                                     return $ UnitVariable 1
inferExprUnits (Unary _ _ _ e) = inferExprUnits e
inferExprUnits (CallExpr _ _ e1 (ArgList _ e2)) = do uv <- anyUnits Temporary
                                                     inferExprUnits e1
                                                     inferExprUnits e2
                                                     error "CallExpr not implemented"
                                                     return uv
inferExprUnits (NullExpr _ _) = anyUnits Temporary
inferExprUnits (Null _ _) = return $ UnitVariable 1
inferExprUnits (ESeq _ _ e1 e2) = do inferExprUnits e1
                                     inferExprUnits e2
                                     return $ error "ESeq units wanted"
inferExprUnits (Bound _ _ e1 e2) = do uv1 <- inferExprUnits e1
                                      uv2 <- inferExprUnits e2
                                      mustEqual uv1 uv2
inferExprUnits (Sqrt _ _ e) = do uv <- inferExprUnits e
                                 sqrtUnits uv
inferExprUnits (ArrayCon _ _ (e:exprs)) =
  do uv <- inferExprUnits e
     mapM_ (\e' -> do { uv' <- inferExprUnits e'; mustEqual uv uv'}) exprs
     return uv
inferExprUnits (AssgExpr _ _ _ e) = inferExprUnits e

inferExprSeqUnits :: Show a => Expr a -> State UnitEnv [UnitVariable]
inferExprSeqUnits (ESeq _ _ e1 e2) = liftM2 (++) (inferExprSeqUnits e1) (inferExprSeqUnits e2)
inferExprSeqUnits e = (:[]) `liftM` inferExprUnits e

handleExpr :: Show a => Expr a -> State UnitEnv (Expr a)
handleExpr x = do inferExprUnits x
                  return x

inferForHeaderUnits :: Show a => (Variable, Expr a, Expr a, Expr a) -> State UnitEnv ()
inferForHeaderUnits (v, e1, e2, e3) =
  do uenv <- gets unitVarEnv
     let Just (uv, []) = lookup (map toUpper v) uenv
     uv1 <- inferExprUnits e1
     mustEqual uv uv1
     uv2 <- inferExprUnits e2
     mustEqual uv uv2
     uv3 <- inferExprUnits e3
     mustEqual uv uv3
     return ()

inferSpecUnits :: (Show a, Data a) => [Spec a] -> State UnitEnv ()
inferSpecUnits = mapM_ $ descendBiM' handleExpr

inferStmtUnits :: (Show a, Data a) => Fortran a -> State UnitEnv ()
inferStmtUnits (Assg _ _ e1 e2) =
  do uv1 <- inferExprUnits e1
     uv2 <- inferExprUnits e2
     mustEqual uv1 uv2
     return ()
inferStmtUnits (For _ _ _ (NullExpr _ _) _ _ s) = inferStmtUnits s
inferStmtUnits (For _ _ (VarName _ v) e1 e2 e3 s) =
  do inferForHeaderUnits (v, e1, e2, e3)
     inferStmtUnits s
inferStmtUnits (FSeq _ _ s1 s2) = mapM_ inferStmtUnits [s1, s2]
inferStmtUnits (If _ _ e1 s1 elseifs ms2) =
  do inferExprUnits e1
     inferStmtUnits s1
     sequence_ [inferExprUnits e >> inferStmtUnits s | (e, s) <- elseifs]
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
inferStmtUnits (Where _ _ e s) =
  do inferExprUnits e
     inferStmtUnits s
inferStmtUnits (Write _ _ specs exprs) =
  do inferSpecUnits specs
     mapM_ inferExprUnits exprs
inferStmtUnits (PointerAssg _ _ e1 e2) =
  do uv1 <- inferExprUnits e1
     uv2 <- inferExprUnits e2
     mustEqual uv1 uv2
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

handleStmt :: (Show a, Data a) => Fortran a -> State UnitEnv (Fortran a)
handleStmt x = do inferStmtUnits x
                  return x
