{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wall #-}

module Camfort.Specification.Hoare.CheckBackend
  ( AnnotatedProgramUnit(..)
  , HoareBackendError(..)
  , HoareCheckResult(..)
  , CheckHoare
  , runCheckHoare
  , checkPU
  ) where

import           Control.Applicative                    (Alternative (..))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Foldable                          (foldlM)
import           Data.Generics.Uniplate.Operations
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (isJust)
import           System.IO.Unsafe                       (unsafePerformIO)

import qualified Language.Fortran.Analysis              as F
-- import qualified Language.Fortran.Analysis.BBlocks      as F
-- import qualified Language.Fortran.Analysis.DataFlow     as F
import qualified Language.Fortran.AST                   as F
-- import qualified Language.Fortran.Util.Position         as F

import qualified Camfort.Analysis                       as CA
import           Camfort.Analysis.Annotations           (mkReport)
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate

import           Language.Expression.DSL
import           Language.Expression.Pretty
import           Language.Verification
import           Language.Verification.Conditions

--------------------------------------------------------------------------------
--  Data types
--------------------------------------------------------------------------------

data AnnotatedProgramUnit =
  AnnotatedProgramUnit
  { _apuPreconditions  :: [PrimFormula ()]
  , _apuPostconditions :: [PrimFormula ()]
  , _apuPU             :: F.ProgramUnit HA
  }

data HoareBackendError
  = VerifierError (VerifierError NamePair (Expr' FortranOps))
  | TranslateError0 (TranslateError ())
  -- ^ Unit errors come from translating annotation formulae
  | TranslateError1 (TranslateError HA)
  -- ^ HA errors come from translating actual source Fortran
  | UnsupportedBlock (F.Block HA)
  -- ^ Found a block that we don't know how to deal with
  | UnexpectedBlock (F.Block HA)
  -- ^ Found a block in an illegal place
  | ArgWithoutDecl SourceName
  -- ^ Found an argument that didn't come with a variable declaration
  | AnnotationError (F.Block HA)
  -- ^ The program was insufficiently annotated
  | VarNotInScope NamePair
  -- ^ The variable was referenced in an assignment but not in scope
  | MissingWhileInvariant (F.Block HA)
  -- ^ The while block had no associated invariant
  deriving (Show)

data HoareCheckResult = HoareCheckResult Bool
  deriving (Show)

data CheckHoareState =
  CheckHoareState
  { _vsImplicitVars :: Bool
  , _vsVarsInScope  :: Map SourceName (SomeVar NamePair)
  -- ^ The variables currently in scope. Keyed by source name, and values have
  -- unique names.
  }

initialState :: CheckHoareState
initialState =
  CheckHoareState
  { _vsImplicitVars = True
  , _vsVarsInScope = mempty
  }

data CheckHoareEnv = CheckHoareEnv

defaultEnv :: CheckHoareEnv
defaultEnv = CheckHoareEnv

makeLenses ''AnnotatedProgramUnit
makeLenses ''CheckHoareState
makeLenses ''CheckHoareEnv

--------------------------------------------------------------------------------
--  Check monad
--------------------------------------------------------------------------------

newtype CheckHoare a = CheckHoare (ExceptT HoareBackendError (RWS CheckHoareEnv String CheckHoareState) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError HoareBackendError
    , MonadState CheckHoareState
    , MonadReader CheckHoareEnv
    , MonadWriter String)

runCheckHoare :: CheckHoare a -> CA.SimpleAnalysis x (Either HoareBackendError a)
runCheckHoare (CheckHoare action) = do
  tell . mkReport $ "Test"

  let (result, logs) = evalRWS (runExceptT action) defaultEnv initialState

  tell (mkReport logs)

  return result

--------------------------------------------------------------------------------
--  Main work
--------------------------------------------------------------------------------

checkPU :: AnnotatedProgramUnit -> CheckHoare HoareCheckResult
checkPU = genVcsPU >=> verifyVcs >=> pure . HoareCheckResult


genVcsPU :: AnnotatedProgramUnit -> CheckHoare [TransFormula Bool]
genVcsPU apu = do
  let nm = apu ^. apuPU . to F.puName

  debugLog $ "APU with name " ++ show nm ++ ":"

  body <- initialSetup (apu ^. apuPU)
  let bodyInfoLines = body >>= blockInfoLines

  debugLog $ "Body:"
  mapM_ debugLog bodyInfoLines

  varsInScope <- Map.toList <$> use vsVarsInScope

  debugLog $ show (varsInScope & each . _2 %~ someTypeRep)

  preconds <- doTranslate $ traverse translateFormula (apu ^. apuPreconditions)
  postconds <- doTranslate $ traverse translateFormula (apu ^. apuPostconditions)

  debugLog $ "Preconditions: " ++ pretty preconds
  debugLog $ "Postconditions: " ++ pretty postconds

  let precond = foldr (*&&) (plit True) preconds
      postcond = foldr (*&&) (plit True) postconds

  genBlocksVcs precond postcond body


genBlocksVcs :: GenVCs CheckHoare (Expr' FortranOps) (Var NamePair) [F.Block HA]
genBlocksVcs precond postcond =
  blocksToSequence >=> sequenceVCs genBlockVcs precond postcond


genBlockVcs :: GenVCs CheckHoare (Expr' FortranOps) (Var NamePair) (F.Block HA)
genBlockVcs precond postcond bl = case bl of
  F.BlIf _ _ _ _ conds bodies _ -> do
    condsExprs <- traverse (traverse (doTranslate . translateExpression')) conds
    multiIfVCs genBlocksVcs expr precond postcond (zip condsExprs bodies)

  F.BlDoWhile _ _ _ _ cond body _ -> do
    primInvariant <-
      case body of
        b : _ | Just (Specification SpecInvariant f) <- getAnnSpec (F.getAnnotation b) -> return f
        _ -> throwError (MissingWhileInvariant bl)

    invariant <- doTranslate (translateFormula primInvariant)
    condExpr <- doTranslate (translateExpression' cond)

    whileVCs genBlocksVcs expr invariant precond postcond (condExpr, body)

  F.BlComment _ _ _ -> return []
  _ -> throwError (UnsupportedBlock bl)

combineBlockSequence
  :: AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA)
  -> F.Block HA
  -> CheckHoare (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
combineBlockSequence prevSeq bl = do
  blSeq <- blockToSequence bl

  case prevSeq `joinAnnSeq` blSeq of
    Just r -> return r
    Nothing -> throwError (AnnotationError bl)


blocksToSequence :: [F.Block HA] -> CheckHoare (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
blocksToSequence = foldlM combineBlockSequence emptyAnnSeq


blockToSequence :: F.Block HA -> CheckHoare (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
blockToSequence bl = do
  mAssign <- fmap (JustAssign . (: [])) <$> assignmentBlock bl

  let mSpec = getAnnSpec (F.getAnnotation bl)

  mMidcond <- case mSpec of
    Just (Specification SpecSeq m) -> do
      m' <- doTranslate $ translateFormula m
      return (Just (propAnnSeq m'))
    _ -> return Nothing

  let other = case bl of
        F.BlComment{} -> Just emptyAnnSeq
        _ -> Just (cmdAnnSeq bl)

  putBlockInfo bl

  case mAssign <|> mMidcond <|> other of
    Just x -> return x
    Nothing -> throwError $ AnnotationError bl


assignmentBlock :: F.Block HA -> CheckHoare (Maybe (Assignment (Expr' FortranOps) (Var NamePair)))
assignmentBlock = \case
  F.BlStatement _ _ _ (F.StExpressionAssign _ _ lvalue rvalue) -> do
    let lnames = varNames lvalue
    Some theVar <- varFromScope lnames

    theExpr <- doTranslate $ translateExpression' rvalue

    return (Just (Assignment theVar theExpr))

  _ -> return Nothing

--------------------------------------------------------------------------------
--  Other actions
--------------------------------------------------------------------------------

varNames :: F.Expression (F.Analysis x) -> NamePair
varNames e =
  let uniqueName = UniqueName $ F.varName e
      srcName = SourceName $ F.srcName e
  in NamePair uniqueName srcName


varFromScope :: NamePair -> CheckHoare (SomeVar NamePair)
varFromScope np = do
  let src = np ^. npSource
  mscoped <- use (vsVarsInScope . at src)
  case mscoped of
    Just v  -> return v
    Nothing -> throwError (VarNotInScope np)


-- | Sets up the environment for checking the program unit, including reading
-- past variable declarations. Returns the blocks after the variable declarations.
initialSetup :: F.ProgramUnit HA -> CheckHoare [F.Block HA]
initialSetup pu = do
  let body = childrenBi pu :: [F.Block HA]

  -- If the program unit is a function, we might need to treat its name as a
  -- variable with its return type.

  -- If the program is a function or subroutine, it might have arguments that we
  -- need to treat as variables.
  argNames <- maybe [] (map (SourceName . F.srcName) . F.aStrip) <$> case pu of
    F.PUFunction _ _ (Just rettype) _ _ funargs retvalue _ _ -> do
      rettype' <- doTranslate $ translateTypeSpec rettype

      case retvalue of
        Just rv -> newVar rv rettype'
        Nothing -> newFunctionVar pu rettype'

      return funargs

    F.PUSubroutine _ _ _ _ subargs _ _ -> return subargs
    _ -> return Nothing

  restBody <- readInitialBlocks body

  -- Verify that all argument names have types associated with them.
  forM_ argNames $ \argName -> do
    hasType <- isJust <$> use (vsVarsInScope . at argName)
    unless hasType $ throwError (ArgWithoutDecl argName)

  return restBody

-- | As part of the initial setup, reads setup blocks like declarations and
-- implicit statements. Updates the environment accordingly. Returns the rest of
-- the blocks, after the setup blocks.
readInitialBlocks :: [F.Block HA] -> CheckHoare [F.Block HA]
readInitialBlocks = dropWhileM readInitialBlock
  where
    -- This function should return 'True' if the block may be part of the setup,
    -- and 'False' otherwise.
    readInitialBlock bl = case bl of
      F.BlStatement _ _ _ st ->
        case st of
          F.StDeclaration _ _ declTy _ decls -> do
            declVars <- forM (F.aStrip decls) $ \case
              -- TODO: Deal with declarations that include assignments
              F.DeclVariable _ _ e Nothing Nothing -> return e
              _ -> throwError (UnsupportedBlock bl)

            declTy' <- doTranslate $ translateTypeSpec declTy

            forM_ declVars $ \v -> newVar v declTy'

            return True
          F.StImplicit _ _ Nothing -> do
            vsImplicitVars .= False
            return True
          F.StImplicit _ _ (Just _) -> throwError (UnsupportedBlock bl)
          _ -> return False

      F.BlComment{} -> return True
      _ -> return False

--------------------------------------------------------------------------------
--  Translation
--------------------------------------------------------------------------------

class ReportAnn a where reportTranslateError :: TranslateError a -> CheckHoare x
instance ReportAnn HA where reportTranslateError = throwError . TranslateError1
instance ReportAnn () where reportTranslateError = throwError . TranslateError0

doTranslate :: ReportAnn ann => MonadTranslate ann a -> CheckHoare a
doTranslate action = do
  env <- currentTranslateEnv
  case runMonadTranslate action env of
    Right x  -> return x
    Left err -> reportTranslateError err

currentTranslateEnv :: CheckHoare TranslateEnv
currentTranslateEnv = do
  st <- get
  return $ defaultTranslateEnv
         & teImplictVars .~ st ^. vsImplicitVars
         & teVarsInScope .~ st ^. vsVarsInScope

--------------------------------------------------------------------------------
--  Interacting with the theorem prover
--------------------------------------------------------------------------------

verifyVcs :: [TransFormula Bool] -> CheckHoare Bool
verifyVcs vcs = do
  let theQuery = and <$> traverse checkProp vcs
      theVerifier = query theQuery

  debugLog "Verifying Vcs:"
  debugLog (pretty vcs)

  -- TODO: get rid of unsafePerformIO
  -- TODO: SBV doesn't like something. Find out what it is!
  case unsafePerformIO (putPretty (pretty vcs) >> runVerifier theVerifier) of
    Right b -> return b
    Left e  -> throwError (VerifierError e)

--------------------------------------------------------------------------------
--  Variables
--------------------------------------------------------------------------------

-- TODO: Maybe report a warning when two variables have the same source name.

newVar :: F.Expression (F.Analysis x) -> SomeType -> CheckHoare ()
newVar e ty = do
  let names = varNames e
      theVar = mapSome (const (Var names)) ty

  vsVarsInScope . at (names ^. npSource) .= Just theVar

newFunctionVar :: F.ProgramUnit (F.Analysis x) -> SomeType -> CheckHoare ()
newFunctionVar pu ty = do
  let uniqueName = case F.puName pu of
        F.Named n -> UniqueName n
        _         -> error "Impossible: function has no name"
      srcName = case F.puSrcName pu of
        F.Named n -> SourceName n
        _         -> error "Impossible: function has no name"

      theVar = mapSome (const (Var (NamePair uniqueName srcName))) ty

  vsVarsInScope . at srcName .= Just theVar

--------------------------------------------------------------------------------
--  General actions
--------------------------------------------------------------------------------

debugLog :: String -> CheckHoare ()
debugLog = tell . (++ "\n")

dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM f (x : xs) = do
  continue <- f x
  if continue
    then dropWhileM f xs
    else return (x : xs)

statementInfo :: F.Statement HA -> String
statementInfo = \case
  F.StDeclaration _ _ _ _ _ -> "declaration"
  F.StImplicit _ _ _ -> "implicit"
  F.StExpressionAssign _ _ _ _ -> "expression assign"
  _ -> "unrecognised statement"

blockInfoLines :: F.Block HA -> [String]
blockInfoLines bl =
  let ann = F.getAnnotation bl
      hasSpec = isJust $ getAnnSpec ann

      (blName, blInternal) = case bl of
        F.BlStatement _ _ _ st -> (statementInfo st, [])
        F.BlComment{} -> ("comment", [])
        F.BlIf _ _ _ _ _ bodies _ -> ("if", do body <- bodies; block <- body; blockInfoLines block)
        F.BlDo _ _ _ _ _ _ body _ -> ("do", blockInfoLines =<< body)
        F.BlDoWhile _ _ _ _ _ body _ -> ("while", blockInfoLines =<< body)
        _ -> ("unrecognised block", [])

  in [blName ++ if hasSpec then " with spec" else ""] ++ map ("  " ++) blInternal

putBlockInfo :: F.Block HA -> CheckHoare ()
putBlockInfo = mapM_ debugLog . blockInfoLines

getAnnSpec :: HA -> Maybe (PrimSpec ())
getAnnSpec = view (to F.prevAnnotation . hoareSpec)
