{-

  Units of measure extension to Fortran

  Files: Units.hs
         UnitsEnvironment.hs


TODO:
 * Deal with variable shadowing in "contained" functions.
 * Better errors with line number info

-}


{-# LANGUAGE ScopedTypeVariables, ImplicitParams, DoAndIfThenElse, PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Camfort.Extensions.UnitsForpar
  ( runUnits
  , UnitState(..)
  , inferUnits
  , removeUnits
  , inferCriticalVariables
  , parameterise )
where

import Prelude hiding (EQ, GT, LT)
import Numeric
import Data.Ratio
import Data.Maybe
import Data.Matrix
import Data.List
import Data.Char (isNumber)
import qualified Data.Vector as V
import Data.Label.Mono (Lens)
import qualified Data.Label
import Data.Label.Monadic hiding (modify)
import Data.Function
import Data.Data
import Data.Char
import qualified Data.Map as M
import Control.Monad.State.Strict hiding (gets)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Generics.Uniplate.Operations

import Camfort.Helpers
import Camfort.Output
import Camfort.Analysis.Annotations
import Camfort.Analysis.Syntax
import Camfort.Analysis.Types
import Camfort.Extensions.UnitsEnvironment hiding (Unitless)
import Camfort.Extensions.UnitsSolve -- Solvers for the Gaussian matrix
import qualified Camfort.Extensions.UnitParser as P

import Forpar.AST
import Forpar.Util.Position
import Forpar.Analysis.Types
import Forpar.Analysis

import Text.PrettyPrint.GenericPretty
import Camfort.Transformation.Syntax

--------------------------------------------------

import qualified Debug.Trace as D
import qualified Forpar.Parser.Fortran77 as F77 -- temp

-- testparse "test1.f"
testparse f = do
  inp <- readFile f
  return $ forparse inp f
  where
    forparse :: SourceText -> Filename -> ProgramFile ()
    forparse contents f = F77.fortran77Parser contents f

--------------------------------------------------

data UnitState = UnitState { solver :: Solver, assumeLiterals :: AssumeLiterals }
  deriving (Show, Eq)
type UnitsM a = StateT UnitState Identity a

runUnits s0 m = runIdentity (evalStateT m s0)

--------------------------------------------------

-- | Annotate variable declarations based on unit declarations in
-- comments (must occur before variable renaming stage).
annotateDeclarations :: ProgramFile A -> ProgramFile A
annotateDeclarations = descendBi annotPU

annotPU :: ProgramUnit A -> ProgramUnit A
annotPU pu = descendBi annotBS pu -- find and replace the beginning of block lists

annotBS :: [Block A] -> [Block A]
annotBS bs = bs'
  where
    (_, bs') = mapAccumL fBlocks [] bs
    fBlocks uenv bl@(BlComment _ _ cmtStr) = case P.unitParser cmtStr of
      Nothing -> (uenv, bl)
      Just us -> (us:uenv, bl)
    fBlocks uenv bl@(BlStatement _ _ _ (StDeclaration {})) = (uenv, bl')
      where
        bl' = transformBi fV bl
        find n = fromMaybe (Undetermined n) $ undefined -- n `lookup` uenv
        fV (ValVariable a@(A { unitInfo = Nothing }) n) =
            ValVariable (a   { unitInfo = Just (find n) }) n
        fV (ValArray a@(A { unitInfo = Nothing }) n)    =
            ValArray (a   { unitInfo = Just (find n) }) n
        fV v                                            = v
    fBlocks uenv bl = (uenv, bl)

--------------------------------------------------

-- | Mark function and subroutine parameters as "Parametric" unit
parameterise :: ProgramFile A -> ProgramFile A
parameterise pf = transformBi fPU pf
  where
    fPU :: ProgramUnit A -> ProgramUnit A
    fPU pu
      | null params = pu
      | otherwise   = transformBi fV pu
      where
        params = case pu of
          PUFunction a s ty n params b -> (n, Parametric (n, 0)):zipWith (fP n) (aStrip params) [1..]
          PUSubroutine a s n params b  -> zipWith (fP n) (aStrip params) [1..]
          _                            -> []

        varName (ValVariable _ n) = n
        varName (ValArray _ n) = n

        fP fn (ValVariable _ n) i = (n, Parametric (fn, i))
        fP fn (ValArray _ n) i    = (n, Parametric (fn, i))

        fV v@(ValVariable a n) = case n `lookup` params of
          Just info -> ValVariable (a { unitInfo = Just info }) n
          Nothing   -> v
        fV v@(ValArray a n) = case n `lookup` params of
          Just info -> ValArray (a { unitInfo = Just info }) n
          Nothing   -> v
        fV v                = v

--------------------------------------------------

markUndetermined :: ProgramFile A -> ProgramFile A
markUndetermined pf = transformBi fV pf
  where
    fV (ValVariable a@(A { unitInfo = Nothing }) n) =
        ValVariable (a   { unitInfo = Just (Undetermined n) }) n
    fV (ValArray a@(A { unitInfo = Nothing }) n)    =
        ValArray (a   { unitInfo = Just (Undetermined n) }) n
    fV v                                            = v

--------------------------------------------------

test f = mapM_ print . map simplifyConstraints
                     . extractConstraints
                     . markUndetermined
                     . parameterise =<< fmap (const unitAnnotation) `fmap` testparse f

data Constraint = C [(Double, [UnitInfo])] -- polynomial summing to 0
  deriving (Eq, Ord, Data, Typeable)

instance Show Constraint where
  show (C poly) = concat . intersperse " + " $ map f poly
    where f (coef, units) = concat . intersperse "*" $ coefStr ++ map u units
            where coefStr = if coef `approxEq` 1 then [] else [showFFloat (Just 1) coef ""]
          u (Parametric (s,i))      = s ++ "[" ++ show i ++ "]"
          u (ParametricUse (s,i,j)) = s ++ "[" ++ show i ++ "," ++ show j ++ "]"
          u (UnitName s)            = s
          u (Undetermined s)        = "?" ++ s
          u Unitless                = "1"
          u (UnitMul u1 u2)         = u u1 ++ "*" ++ u u2
          u (UnitPow u1 p1)         = u u1 ++ if p1 `approxEq` 1
                                              then ""
                                              else "^" ++ showFFloat (Just 1) p1 ""

simplifyConstraints (C poly) = C $ map (\ (c, u) -> (c, concatMap flattenUnits u)) poly

type Constrainer = StateT [Int] (Writer [Constraint])
execConstrainer = execWriter . flip runStateT [1..]

getUniqNum :: Constrainer Int
getUniqNum = get >>= \ (n:ns) -> put ns >> return n

extractConstraints :: ProgramFile A -> [Constraint]
extractConstraints pf = execConstrainer $
  descendBiM ((fromJust `fmap`) . fS_PF) pf
  -- descendBiM fS_PF =<< transformBiM fE_PF pf

fS_PF :: Statement A -> Constrainer (Maybe (Statement A))
fS_PF st@(StExpressionAssign {}) = do
  st'@(StExpressionAssign a s e1 e2) <- transformBiM ((fromJust  `fmap`) . fE) st
  let r = rexpr e2
  unless (null r) $ tell [C (lexpr e1 ++ r)]
  return $ Just st'
  where
    lexpr (ExpValue _ _ (ValVariable a _)) = [(-1, maybeToList (unitInfo a))]
    lexpr (ExpSubscript _ _ (ExpValue _ _ (ValArray a _)) _) = [(-1, maybeToList (unitInfo a))]
    -- FIXME: more...

    rexpr e = case unitInfo (getAnnotation e) of
                Just ui -> [(1, [ui])]
                Nothing -> [] -- FIXME: for now...
fS_PF st@(StCall _ _ (ExpValue _ _ (ValSubroutineName _)) (Just _)) = do
  st'@(StCall a s (ExpValue _ _ (ValSubroutineName sn)) (Just args))
           <- transformBiM ((fromJust  `fmap`) . fE) st
  sncallId <- getUniqNum
  let f aexp i = tell [C [(-1, maybeToList (getUI aexp)), (1, [ParametricUse (sn, i, sncallId)])]]
  zipWithM_ f (aStrip args) [1..]
  return . Just $ st'
fS_PF st = Just `fmap` transformBiM ((fromJust `fmap`) . fE) st

-- might be useful to have unique names generated for undetermined units?

fE :: Expression A -> Constrainer (Maybe (Expression A))
fE e@(ExpValue _ _ (ValInteger i)) = return . Just $ setUI (Just (Undetermined i)) e
fE e@(ExpValue _ _ (ValReal i)) = return . Just $ setUI (Just (Undetermined i)) e
fE (ExpValue a s v@(ValVariable a' n)) = return . Just $ ExpValue (a { unitInfo = unitInfo a' }) s v
fE (ExpValue a s v@(ValArray a' n)) = return . Just $ ExpValue (a { unitInfo = unitInfo a' }) s v
fE e@(ExpBinary a s Addition e1 e2) = do
  tell [C [(-1, maybeToList (getUI e1)), (1, maybeToList (getUI e2))]]
  return . Just $ setUI (getUI e1) e
fE e@(ExpBinary a s Subtraction e1 e2) = do
  tell [C [(-1, maybeToList (getUI e1)), (1, maybeToList (getUI e2))]]
  return . Just $ setUI (getUI e1) e
fE e@(ExpBinary a s Multiplication e1 e2) = do
  return . Just $ setUI (Just (UnitMul (fromJust (getUI e1)) (fromJust (getUI e2)))) e
fE e@(ExpBinary a s Division e1 e2) = do
  return . Just $ setUI (Just (UnitMul (fromJust (getUI e1)) (UnitPow (fromJust (getUI e2)) (-1)))) e
fE e@(ExpFunctionCall _ _ (ExpValue _ _ (ValFunctionName fn)) args) = do
  fncallId <- getUniqNum
  let f aexp i = tell [C [(-1, maybeToList (getUI aexp)), (1, [ParametricUse (fn, i, fncallId)])]]
  zipWithM_ f (aStrip args) [1..]
  return . Just $ setUI (Just (ParametricUse (fn, 0, fncallId))) e
fE e = return . Just $ e

assignmentStatements :: Data a => ProgramFile a -> [Statement a]
assignmentStatements p = [ StExpressionAssign a s e1 e2 | StExpressionAssign a s e1 e2 <- universeBi p ]

getUI = unitInfo . getAnnotation
setUI ui x = setAnnotation ((getAnnotation x) { unitInfo = ui }) x

--------------------------------------------------

extractUnitInfo pf = [ (n, ui) | ValVariable (A { unitInfo = ui }) n <- universeBi pf ]

--------------------------------------------------

simplifyUnits :: UnitInfo -> UnitInfo
simplifyUnits = rewrite rw
  where
    rw (UnitMul (UnitMul u1 u2) u3)                          = Just $ UnitMul u1 (UnitMul u2 u3)
    rw (UnitMul u1 u2) | u1 == u2                            = Just $ UnitPow u1 2
    rw (UnitPow (UnitPow u1 p1) p2)                          = Just $ UnitPow u1 (p1 * p2)
    rw (UnitMul (UnitPow u1 p1) (UnitPow u2 p2)) | u1 == u2  = Just $ UnitPow u1 (p1 + p2)
    rw (UnitPow _ p) | p `approxEq` 0                        = Just $ Unitless
    rw u                                                     = Nothing

flattenUnits :: UnitInfo -> [UnitInfo]
flattenUnits = map (uncurry UnitPow)
             . M.toList . M.filter (not . (approxEq 0))
             . M.fromListWith (+)
             . map (\ (u, p) -> (simplifyUnits u, p))
             . flatten
  where
    flatten (UnitMul u1 u2) = flatten u1 ++ flatten u2
    flatten (UnitPow u p)   = map (\ (u', p') -> (u', p*p')) $ flatten u
    flatten u               = [(u, 1)]

approxEq a b = abs (b - a) < epsilon
epsilon = 0.001 -- arbitrary

--------------------------------------------------

inferUnits :: (Filename, ProgramFile Annotation) ->
              UnitsM (Report, (Filename, ProgramFile Annotation))
inferUnits (fn, p) = do
  let types = inferTypes p
  undefined

expressions :: Data a => ProgramFile a -> [Expression a]
expressions = universeBi

relOps = [ GT , GTE , LT , LTE , EQ , NE , Equivalent , NotEquivalent ]
relOp = (`elem` relOps)

relExpressions :: Data a => ProgramFile a -> [Expression a]
relExpressions = filter f . expressions
  where f :: Data a => Expression a -> Bool
        f (ExpBinary _ _ bop _ _) = relOp bop
        f _ = False

--------------------------------------------------

inferCriticalVariables :: (Filename, ProgramFile Annotation) ->
                          UnitsM (Report, (Filename, ProgramFile Annotation))
inferCriticalVariables (fn, p) = do
  let types = inferTypes p
  undefined

--------------------------------------------------

removeUnits = undefined

--------------------------------------------------

foo :: Value () -> Value ()
foo (ValVariable _ _) = ValVariable () "foo"
foo x = x

depth :: Data a => ProgramFile a -> Int
depth = para (\_ cs -> 1 + maximum (0:cs))

--------------------------------------------------

u = initSrcSpan
varGen :: String -> Expression ()
varGen str = ExpValue () u $ ValVariable () str
arrGen :: String -> Expression ()
arrGen str = ExpValue () u $ ValArray () str
intGen :: Integer -> Expression ()
intGen i = ExpValue () u $ ValInteger $ show i

ex3 = fmap (const unitAnnotation) $ ProgramFile [ ([ ], ex3pu1), ([ ], ex3pu2)] [ ]
ex3pu1 = PUMain () u (Just "main") ex3pu1bs
ex3pu1bs =
  [ BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u
      [ DeclVariable () u (varGen "a")
      , DeclArray () u (arrGen "b") (AList () u [ DimensionDeclarator () u Nothing (intGen 1) ])
      , DeclVariable () u (varGen "c")
      , DeclVariable () u (varGen "d") ]))
  , BlStatement () u Nothing (StDimension () u (AList () u
      [ DeclArray () u (arrGen "a") (AList () u [ DimensionDeclarator () u Nothing (intGen 1 ) ]) ]))
  , BlStatement () u Nothing (StDeclaration () u (TypeInteger () u) (AList () u
      [ DeclVariable () u (varGen "c") ]))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "a") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "b") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (ExpSubscript () u (arrGen "c") (AList () u [ intGen 1 ])) (intGen 1))
  , BlStatement () u Nothing (StExpressionAssign () u
      (varGen "d") (ExpBinary () u Addition (varGen "d") (intGen 1))) ]
ex3pu2 = PUFunction () u (Just $ TypeInteger () u) "f1" (AList () u [ValVariable () "d", ValVariable () "b"])
                    (ex3pu1bs ++
                      [ BlStatement () u Nothing (
                          StExpressionAssign () u (varGen "f1")
                            (ExpBinary () u Multiplication (varGen "d") (varGen "d"))
                      ) ])
