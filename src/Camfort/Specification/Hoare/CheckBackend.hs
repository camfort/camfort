{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Camfort.Specification.Hoare.CheckBackend
  ( AnnotatedProgramUnit(..)
  , apuPreconditions
  , apuPostconditions
  , apuPU
  , HoareCheckResult(..)
  , HoareBackendError(..)
  , HoareBackendErrorKind(..)
  , checkPU
  ) where

import           Control.Exception                      (Exception (..))
import           Control.Lens
import           Data.Foldable (foldlM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Control.Monad.State.Strict
import           Control.Monad.RWS.Strict
import           Data.Generics.Uniplate.Operations
import           Data.Map                               (Map)
import           Data.Maybe                             (isJust)

import qualified Language.Fortran.Analysis              as F
import qualified Language.Fortran.AST                   as F
import qualified Language.Fortran.Util.Position         as F

import qualified Camfort.Analysis                       as CA
import           Camfort.Analysis.Annotations           (Report, mkReport)
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

data HoareBackendErrorKind
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
  -- ^ The program was insufficiently annotated (TODO: more detail)
  | VarNotInScope NamePair
  -- ^ The variable was referenced in an assignment but not in scope
  | MissingWhileInvariant (F.Block HA)
  -- ^ The while block had no associated invariant
  deriving (Show)

instance Exception HoareBackendErrorKind where
  displayException = \case
    VerifierError e -> "verifier error: " ++ displayException e
    TranslateError0 te -> "translation error: " ++ displayException te
    TranslateError1 te -> "translation error: " ++ displayException te
    UnsupportedBlock _ -> "encountered unsupported block"
    UnexpectedBlock _ -> "a block was found in an illegal location"
    ArgWithoutDecl nm -> "argument " ++ show nm ++ " doesn't have an associated type declaration"
    AnnotationError _ ->
      "the program was insufficiently annotated; " ++
      "`seq` annotations must appear before each command which is not an assignment"
    VarNotInScope nm -> "variable " ++ pretty nm ++ " is being assigned to but is not in scope"
    MissingWhileInvariant _ ->
      "found a `do while` block with no invariant; " ++
      "invariant annotations must appear at the start of every `do while` loop"

data HoareBackendError =
  HoareBackendError
  { hbeSpan :: Maybe F.SrcSpan
  , hbeKind :: HoareBackendErrorKind
  }
  deriving (Show)

instance Exception HoareBackendError where
  displayException HoareBackendError{ hbeSpan, hbeKind }
    | Just sp <- hbeSpan = "error at " ++ show sp ++ ": " ++ displayException hbeKind
    | otherwise = "error: " ++ displayException hbeKind

data HoareCheckResult = HoareCheckResult Bool
  deriving (Show)

data CheckHoareState =
  CheckHoareState
  { _hsCursor :: Maybe F.SrcSpan
  }

type ScopeVars = Map SourceName (SomeVar NamePair)

data CheckHoareEnv =
  CheckHoareEnv
  { _heImplicitVars :: Bool
  , _heVarsInScope :: ScopeVars
  -- ^ The variables in scope. Keyed by source name, and values have unique
  -- names.
  }

initialState :: CheckHoareState
initialState =
  CheckHoareState
  { _hsCursor = Nothing
  }

emptyEnv :: CheckHoareEnv
emptyEnv = CheckHoareEnv True mempty

makeLenses ''AnnotatedProgramUnit
makeLenses ''CheckHoareState
makeLenses ''CheckHoareEnv

--------------------------------------------------------------------------------
--  Main function
--------------------------------------------------------------------------------

checkPU :: AnnotatedProgramUnit -> CA.SimpleAnalysis x (Either HoareBackendError HoareCheckResult, Report)
checkPU apu = runCheckHoare $ do

  (bodyTriple, env) <- flip runStateT emptyEnv $ do
    logCheck $ " - Initial setup"

    body <- initialSetup (apu ^. apuPU)

    preconds <- readerOfState $ doTranslate $ traverse translateFormula (apu ^. apuPreconditions)
    postconds <- readerOfState $ doTranslate $ traverse translateFormula (apu ^. apuPostconditions)

    let precond = propAnd preconds
        postcond = propAnd postconds

    logCheck $ " - Found preconditions: " ++ pretty precond
    logCheck $ " - Found postconditions: " ++ pretty postcond

    return (precond, postcond, body)

  (_, vcs) <- runGenM env (genBody bodyTriple)

  logCheck $ " - Computed verification conditions:"

  results <- traverse verifyVc vcs

  forM_ (zip [1..] (zip results vcs)) $ \(i :: Int, (result, vc)) -> do
    logCheck $ "   " ++ show i ++ ". " ++ pretty vc
    unless result $ logCheck $ "    - Failed!"

  return $ HoareCheckResult $ and results

--------------------------------------------------------------------------------
--  Check Monad
--------------------------------------------------------------------------------

newtype CheckHoare a = CheckHoare (ExceptT HoareBackendError (WriterT String IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError HoareBackendError
    , MonadWriter String
    , MonadIO
    )

runCheckHoare :: CheckHoare a -> CA.SimpleAnalysis x (Either HoareBackendError a, Report)
runCheckHoare (CheckHoare action) = do
  (result, logs) <- liftIO $ runWriterT (runExceptT action)
  return (result, mkReport logs)

logCheck :: (MonadWriter String m) => String -> m ()
logCheck = tell . (++ "\n")

-- | Sets up the environment for checking the program unit, including reading
-- past variable declarations. Returns the blocks after the variable declarations.
initialSetup :: F.ProgramUnit HA -> StateT CheckHoareEnv CheckHoare [F.Block HA]
initialSetup pu = do
  let body = childrenBi pu :: [F.Block HA]

  -- If the program unit is a function, we might need to treat its name as a
  -- variable with its return type.

  -- If the program is a function or subroutine, it might have arguments that we
  -- need to treat as variables.
  mArgNames <- case pu of
    F.PUFunction _ _ (Just rettype) _ _ funargs retvalue _ _ -> do
      rettype' <- runReaderT (doTranslate (translateTypeSpec rettype)) emptyEnv

      heVarsInScope %= case retvalue of
        Just rv -> newVar rv rettype'
        Nothing -> newFunctionVar pu rettype'

      return funargs

    F.PUSubroutine _ _ _ _ subargs _ _ -> return subargs
    _ -> return Nothing

  let argNames = maybe [] (map (SourceName . F.srcName) . F.aStrip) mArgNames

  restBody <- readInitialBlocks body

  -- Verify that all argument names have types associated with them.
  forM_ argNames $ \argName -> do
    hasType <- isJust <$> use (heVarsInScope . at argName)
    unless hasType $ reportErrorAt pu (ArgWithoutDecl argName)

  return restBody

-- | As part of the initial setup, reads setup blocks like declarations and
-- implicit statements. Updates the environment accordingly. Returns the rest of
-- the blocks, after the setup blocks.
readInitialBlocks :: [F.Block HA] -> StateT CheckHoareEnv CheckHoare [F.Block HA]
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
              _ -> reportErrorAt bl (UnsupportedBlock bl)

            declTy' <- readerOfState $ doTranslate $ translateTypeSpec declTy

            forM_ declVars $ \v -> heVarsInScope %= newVar v declTy'

            return True

          F.StImplicit _ _ Nothing -> do
            -- TODO: Deal with implicits properly
            return True
          F.StImplicit _ _ (Just _) -> reportErrorAt bl (UnsupportedBlock bl)
          _ -> return False

      F.BlComment{} -> return True
      _ -> return False

-- TODO: Maybe report a warning when two variables have the same source name.

newVar :: F.Expression (F.Analysis x) -> SomeType -> ScopeVars -> ScopeVars
newVar e ty =
  let names = varNames e
      theVar = mapSome (const (Var names)) ty

  in at (names ^. npSource) .~ Just theVar

newFunctionVar :: F.ProgramUnit (F.Analysis x) -> SomeType -> ScopeVars -> ScopeVars
newFunctionVar pu ty =
  let uniqueName = case F.puName pu of
        F.Named n -> UniqueName n
        _         -> error "Impossible: function has no name"
      srcName = case F.puSrcName pu of
        F.Named n -> SourceName n
        _         -> error "Impossible: function has no name"

      theVar = mapSome (const (Var (NamePair uniqueName srcName))) ty

  in at srcName .~ Just theVar

reportErrorSimple :: (MonadError HoareBackendError m) => HoareBackendErrorKind -> m x
reportErrorSimple err = throwError (HoareBackendError Nothing err)

reportErrorAt :: (MonadError HoareBackendError m, F.Spanned a) => a -> HoareBackendErrorKind -> m x
reportErrorAt x err = throwError (HoareBackendError (Just (F.getSpan x)) err)

verifyVc :: TransFormula Bool -> CheckHoare Bool
verifyVc prop = do
  res <- liftIO . runVerifier . query . checkProp $ prop
  case res of
    Right b -> return b
    Left e -> reportErrorSimple (VerifierError e)

--------------------------------------------------------------------------------
--  Generation Monad
--------------------------------------------------------------------------------

newtype GenM a = GenM (RWST CheckHoareEnv [TransFormula Bool] CheckHoareState CheckHoare a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError HoareBackendError
    , MonadState CheckHoareState
    , MonadReader CheckHoareEnv
    , MonadWriter [TransFormula Bool]
    , MonadIO
    )

runGenM :: CheckHoareEnv -> GenM a -> CheckHoare (a, [TransFormula Bool])
runGenM env (GenM action) = evalRWST action env initialState

type FortranTriplet a = Triplet (Expr' FortranOps) (Var NamePair) a

logGen :: String -> GenM ()
logGen = GenM . lift . logCheck

genBody :: FortranTriplet [F.Block HA] -> GenM ()
genBody = traverseOf _3 bodyToSequence >=> void . sequenceVCs genBlock

genBlock :: FortranTriplet (F.Block HA) -> GenM ()
genBlock (precond, postcond, bl) = do
  setCursor bl
  case bl of
    F.BlIf _ _ _ _ conds bodies _ -> do
      condsExprs <- traverse (traverse (doTranslate . translateExpression')) conds
      multiIfVCs genBody expr (precond, postcond, (zip condsExprs bodies))

    F.BlDoWhile _ _ _ _ cond body _ -> do
      primInvariant <-
        case body of
          b : _ | Just (Specification SpecInvariant f) <- getAnnSpec (F.getAnnotation b) -> return f
          _ -> reportError (MissingWhileInvariant bl)

      invariant <- doTranslate (translateFormula primInvariant)
      condExpr <- doTranslate (translateExpression' cond)

      whileVCs genBody expr invariant (precond, postcond, (condExpr, body))

    F.BlComment _ _ _ -> return ()
    _ -> reportError (UnsupportedBlock bl)

bodyToSequence :: [F.Block HA] -> GenM (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
bodyToSequence blocks = do
  setCursor blocks
  foldlM combineBlockSequence emptyAnnSeq blocks

combineBlockSequence
  :: AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA)
  -> F.Block HA
  -> GenM (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
combineBlockSequence prevSeq bl = do
  setCursor bl
  blSeq <- blockToSequence bl

  case prevSeq `joinAnnSeq` blSeq of
    Just r  -> return r
    Nothing -> reportError (AnnotationError bl)


blockToSequence :: F.Block HA -> GenM (AnnSeq (Expr' FortranOps) (Var NamePair) (F.Block HA))
blockToSequence bl = do
  setCursor bl

  chooseFrom [assignment, sequenceSpec, other]
  where
    assignment = fmap (JustAssign . (: [])) <$> assignmentBlock bl

    sequenceSpec =
      case getAnnSpec (F.getAnnotation bl) of
        Just (Specification SpecSeq m) -> do
          m' <- doTranslate $ translateFormula m
          return $ Just $ propAnnSeq m'
        _ -> return Nothing

    other = return $ case bl of
      F.BlComment{} -> Just emptyAnnSeq
      _             -> Just $ cmdAnnSeq bl

    -- Tries each action in the list, using the first that works and otherwise
    -- reporting an error.
    chooseFrom :: [GenM (Maybe a)] -> GenM a
    chooseFrom [] = reportError $ AnnotationError bl
    chooseFrom (action : rest) = do
      m <- action
      case m of
        Just x -> return x
        Nothing -> chooseFrom rest


assignmentBlock :: F.Block HA -> GenM (Maybe (Assignment (Expr' FortranOps) (Var NamePair)))
assignmentBlock bl = do
  setCursor bl
  case bl of
    F.BlStatement _ _ _ (F.StExpressionAssign _ _ lvalue rvalue) -> do
      let lnames = varNames lvalue
      Some theVar <- varFromScope lnames

      theExpr <- doTranslate $ translateExpression' rvalue

      return (Just (Assignment theVar theExpr))

    _ -> return Nothing


varFromScope :: NamePair -> GenM (SomeVar NamePair)
varFromScope np = do
  let src = np ^. npSource
  mscoped <- view (heVarsInScope . at src)
  case mscoped of
    Just v  -> return v
    Nothing -> reportError (VarNotInScope np)


setCursor :: (F.Spanned a) => a -> GenM ()
setCursor x = hsCursor .= Just (F.getSpan x)


reportError :: HoareBackendErrorKind -> GenM x
reportError err = do
  cursor <- use hsCursor
  throwError (HoareBackendError cursor err)

--------------------------------------------------------------------------------
--  Translation
--------------------------------------------------------------------------------

class ReportAnn a where reportTranslateError :: (MonadError HoareBackendError m) => TranslateError a -> m x
instance ReportAnn HA where reportTranslateError = reportErrorSimple . TranslateError1
instance ReportAnn () where reportTranslateError = reportErrorSimple . TranslateError0

doTranslate
  :: (MonadReader CheckHoareEnv m, MonadError HoareBackendError m, ReportAnn ann)
  => MonadTranslate ann a -> m a
doTranslate action = do
  env <- asks toTranslateEnv
  case runMonadTranslate action env of
    Right x  -> return x
    Left err -> reportTranslateError err

toTranslateEnv :: CheckHoareEnv -> TranslateEnv
toTranslateEnv env =
  defaultTranslateEnv
    & teImplictVars .~ env ^. heImplicitVars
    & teVarsInScope .~ env ^. heVarsInScope

--------------------------------------------------------------------------------
--  Utility functions
--------------------------------------------------------------------------------

dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM f (x : xs) = do
  continue <- f x
  if continue
    then dropWhileM f xs
    else return (x : xs)


getAnnSpec :: HA -> Maybe (PrimSpec ())
getAnnSpec = view (to F.prevAnnotation . hoareSpec)


varNames :: F.Expression (F.Analysis x) -> NamePair
varNames e =
  let uniqueName = UniqueName $ F.varName e
      srcName = SourceName $ F.srcName e
  in NamePair uniqueName srcName


readerOfState :: (MonadState s m) => ReaderT s m a -> m a
readerOfState action = do
  st <- get
  runReaderT action st

-- -- statementInfo :: F.Statement HA -> String
-- -- statementInfo = \case
-- --   F.StDeclaration _ _ _ _ _ -> "declaration"
-- --   F.StImplicit _ _ _ -> "implicit"
-- --   F.StExpressionAssign _ _ _ _ -> "expression assign"
-- --   _ -> "unrecognised statement"

-- -- blockInfoLines :: F.Block HA -> [String]
-- -- blockInfoLines bl =
-- --   let ann = F.getAnnotation bl
-- --       hasSpec = isJust $ getAnnSpec ann

-- --       (blName, blInternal) = case bl of
-- --         F.BlStatement _ _ _ st -> (statementInfo st, [])
-- --         F.BlComment{} -> ("comment", [])
-- --         F.BlIf _ _ _ _ _ bodies _ -> ("if", do body <- bodies; block <- body; blockInfoLines block)
-- --         F.BlDo _ _ _ _ _ _ body _ -> ("do", blockInfoLines =<< body)
-- --         F.BlDoWhile _ _ _ _ _ body _ -> ("while", blockInfoLines =<< body)
-- --         _ -> ("unrecognised block", [])

-- --   in [blName ++ if hasSpec then " with spec" else ""] ++ map ("  " ++) blInternal

-- -- putBlockInfo :: F.Block HA -> CheckHoare ()
-- -- putBlockInfo = mapM_ logGen . blockInfoLines
