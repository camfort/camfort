{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- TODO: More precise error and logging origins

module Camfort.Specification.Hoare.CheckBackend
  ( AnnotatedProgramUnit(..)
  , apuPreconditions
  , apuPostconditions
  , apuPU
  , BackendAnalysis
  , HoareCheckResult(..)
  , HoareBackendError(..)
  , checkPU
  ) where

import           Control.Exception                      (Exception (..))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import           Data.Foldable                          (foldlM)
import           Data.Generics.Uniplate.Operations
import           Data.Map                               (Map)
import           Data.Maybe                             (isJust)
import           Data.Void                              (Void)

import           Data.SBV                               (SBool, defaultSMTCfg)

import qualified Language.Fortran.Analysis              as F
import qualified Language.Fortran.AST                   as F
import qualified Language.Fortran.Util.Position         as F

import           Camfort.Analysis
import           Camfort.Analysis.Logger                (Text, Builder)
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Vars

import           Language.Expression
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
  = VerifierError (VerifierError FortranVar (Expr' '[FortranOp, FLiftLogical]))
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

instance Describe HoareBackendError where
  describeBuilder = \case
    VerifierError e -> "verifier error: " <> describeBuilder (displayException e)
    TranslateError0 te -> "translation error: " <> describeBuilder (displayException te)
    TranslateError1 te -> "translation error: " <> describeBuilder (displayException te)
    UnsupportedBlock _ -> "encountered unsupported block"
    UnexpectedBlock _ -> "a block was found in an illegal location"
    ArgWithoutDecl nm -> "argument " <> describeBuilder (show nm) <> " doesn't have an associated type declaration"
    AnnotationError _ ->
      "the program was insufficiently annotated; " <>
      "`seq` annotations must appear before each command which is not an assignment"
    VarNotInScope nm -> "variable " <> describeBuilder (pretty nm) <> " is being assigned to but is not in scope"
    MissingWhileInvariant _ ->
      "found a `do while` block with no invariant; " <>
      "invariant annotations must appear at the start of every `do while` loop"

type BackendAnalysis = AnalysisT HoareBackendError Void IO

data HoareCheckResult = HoareCheckResult (F.ProgramUnit HA) Bool
  deriving (Show)

describePuName :: F.ProgramUnitName -> Builder
describePuName (F.Named n) = describeBuilder n
describePuName F.NamelessBlockData = "<nameless block data>"
describePuName F.NamelessComment = "<nameless comment>"
describePuName F.NamelessMain = "<nameless main>"

instance Describe HoareCheckResult where
  describeBuilder (HoareCheckResult pu result) =
    "Program unit '" <> describePuName (F.puSrcName pu) <> "': " <>
    (if result then "verified!" else "unverifiable!")

data CheckHoareState =
  CheckHoareState
  { _hsCursor :: F.SrcSpan
  }

type ScopeVars = Map SourceName SomeVar

data CheckHoareEnv =
  CheckHoareEnv
  { _heImplicitVars :: Bool
  , _heVarsInScope  :: ScopeVars
  -- ^ The variables in scope. Keyed by source name, and values have unique
  -- names.
  }

initialState :: F.SrcSpan -> CheckHoareState
initialState sp =
  CheckHoareState
  { _hsCursor = sp
  }

emptyEnv :: CheckHoareEnv
emptyEnv = CheckHoareEnv True mempty

makeLenses ''AnnotatedProgramUnit
makeLenses ''CheckHoareState
makeLenses ''CheckHoareEnv

--------------------------------------------------------------------------------
--  Main function
--------------------------------------------------------------------------------

checkPU :: AnnotatedProgramUnit -> BackendAnalysis HoareCheckResult
checkPU apu = do

  let pu = apu ^. apuPU

  (bodyTriple, env) <- flip runStateT emptyEnv $ do
    logInfo' pu $ " - Setting up"

    body <- initialSetup (apu ^. apuPU)

    let translateFormulae =
            readerOfState
          . doTranslate (lift . lift . failAnalysis' pu)
          . traverse translateFormula

    preconds <- translateFormulae (apu ^. apuPreconditions)
    postconds <- translateFormulae (apu ^. apuPostconditions)

    logInfo' pu $ " - Interpreting pre- and postconditions"

    let precond = propAnd preconds
        postcond = propAnd postconds

    logInfo' pu $ " - Found preconditions: "  <> describe (pretty precond)
    logInfo' pu $ " - Found postconditions: " <> describe (pretty postcond)

    return (precond, postcond, body)

  logInfo' pu $ " - Computing verification conditions"

  (_, vcs) <- runGenM env (F.getSpan pu) (genBody bodyTriple)

  logInfo' pu $ " - Verifying conditions:"

  let checkVcs _ [] = return True
      checkVcs i (vc : rest) = do
        logInfo' pu $ "   " <> describeShow i <> ". " <> describe (pretty vc)
        result <- verifyVc (failAnalysis' pu) vc
        if result
          then checkVcs (1 + i) rest
          else do
          logInfo' pu "    - Failed!"
          zipWithM_ printUnchecked [(1 + i)..] vcs
          return False

      printUnchecked i vc = do
        logInfo' pu $ "   " <> describeShow i <> ". " <> describe (pretty vc)
        logInfo' pu   "    - Unchecked"

  HoareCheckResult pu <$> checkVcs (1 :: Int) vcs

--------------------------------------------------------------------------------
--  Check Monad
--------------------------------------------------------------------------------

type CheckHoare = BackendAnalysis


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
      rettype' <- runReaderT (doTranslate (lift . lift . failAnalysis' pu) (translateTypeSpec rettype)) emptyEnv

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
    unless hasType $ lift $ failAnalysis' pu (ArgWithoutDecl argName)

  return restBody

-- | As part of the initial setup, reads setup blocks like declarations and
-- implicit statements. Updates the environment accordingly. Returns the rest of
-- the blocks, after the setup blocks.
readInitialBlocks :: [F.Block HA] -> StateT CheckHoareEnv CheckHoare [F.Block HA]
readInitialBlocks = dropWhileM readInitialBlock
  where
    -- This function should return 'True' if the block may be part of the setup,
    -- and 'False' otherwise.
    readInitialBlock :: F.Block HA -> StateT CheckHoareEnv CheckHoare Bool
    readInitialBlock bl = case bl of
      F.BlStatement _ _ _ st ->
        case st of
          F.StDeclaration _ _ declTy _ decls -> do
            declVars <- forM (F.aStrip decls) $ \case
              -- TODO: Deal with declarations that include assignments
              F.DeclVariable _ _ e Nothing Nothing -> return e
              _ -> lift $ failAnalysis' bl (UnsupportedBlock bl)

            declTy' <- readerOfState $ doTranslate (lift . lift . failAnalysis' bl) $ translateTypeSpec declTy

            forM_ declVars $ \v -> heVarsInScope %= newVar v declTy'

            return True

          F.StImplicit _ _ Nothing -> do
            -- TODO: Deal with implicits properly
            return True
          F.StImplicit _ _ (Just _) -> lift $ failAnalysis' bl (UnsupportedBlock bl)
          _ -> return False

      F.BlComment{} -> return True
      _ -> return False

-- TODO: Maybe report a warning when two variables have the same source name.

varOfType :: NamePair -> SomeType -> SomeVar
varOfType names (Some d _) = Some d (FortranVar d names)

-- | Create a variable from the given expression containing a variable value.
newVar :: F.Expression (F.Analysis x) -> SomeType -> ScopeVars -> ScopeVars
newVar e ty =
  let names = varNames e
  in at (names ^. npSource) .~ Just (varOfType (varNames e) ty)

-- | Create a variable from the return value of the given function.
newFunctionVar :: F.ProgramUnit (F.Analysis x) -> SomeType -> ScopeVars -> ScopeVars
newFunctionVar pu ty =
  let uniqueName = case F.puName pu of
        F.Named n -> UniqueName n
        _         -> error "impossible: function has no name"
      srcName = case F.puSrcName pu of
        F.Named n -> SourceName n
        _         -> error "impossible: function has no name"

  in at srcName .~ Just (varOfType (NamePair uniqueName srcName) ty)

verifyVc :: (HoareBackendError -> CheckHoare Bool) -> TransFormula Bool -> CheckHoare Bool
verifyVc handle prop = do
  let getSrProp :: SymRepr Bool -> SBool
      getSrProp (SRProp x) = x

  let debug = False
      cfg
        | debug = defaultSMTCfg { verbose = True, transcript = Just "transcript.smt2" }
        | otherwise = defaultSMTCfg

  res <- liftIO . runVerifierWith cfg . query . checkPropWith getSrProp id $ prop
  case res of
    Right b -> return b
    Left e  -> handle (VerifierError e)

--------------------------------------------------------------------------------
--  Generation Monad
--------------------------------------------------------------------------------

newtype GenM a = GenM (RWST CheckHoareEnv [TransFormula Bool] CheckHoareState CheckHoare a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState CheckHoareState
    , MonadReader CheckHoareEnv
    , MonadWriter [TransFormula Bool]
    , MonadIO
    )


runGenM :: CheckHoareEnv -> F.SrcSpan -> GenM a -> CheckHoare (a, [TransFormula Bool])
runGenM env initialSpan (GenM action) = evalRWST action env (initialState initialSpan)


type FortranTriplet a = Triplet FExpr FortranVar a


genBody :: FortranTriplet [F.Block HA] -> GenM ()
genBody = traverseOf _3 bodyToSequence >=> void . sequenceVCs genBlock


genBlock :: FortranTriplet (F.Block HA) -> GenM ()
genBlock (precond, postcond, bl) = do
  setCursor bl
  case bl of
    F.BlIf _ _ _ _ conds bodies _ -> do
      condsExprs <- traverse (traverse (doTranslate failAtCursor . translateBoolExpression)) conds
      multiIfVCs genBody expr (precond, postcond, (zip condsExprs bodies))

    F.BlDoWhile _ _ _ _ cond body _ -> do
      primInvariant <-
        case body of
         b : _ | Just (Specification SpecInvariant f) <- getAnnSpec (F.getAnnotation b) -> return f
         _ -> failAtCursor (MissingWhileInvariant bl)

      invariant <- doTranslate failAtCursor $ translateFormula primInvariant
      condExpr <- doTranslate failAtCursor $ translateBoolExpression cond

      whileVCs genBody expr invariant (precond, postcond, (condExpr, body))

    F.BlComment _ _ _ -> return ()
    _ -> failAtCursor (UnsupportedBlock bl)


bodyToSequence :: [F.Block HA] -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
bodyToSequence blocks = do
  setCursor blocks
  foldlM combineBlockSequence emptyAnnSeq blocks


combineBlockSequence
  :: AnnSeq FExpr FortranVar (F.Block HA)
  -> F.Block HA
  -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
combineBlockSequence prevSeq bl = do
  setCursor bl
  blSeq <- blockToSequence bl

  case prevSeq `joinAnnSeq` blSeq of
    Just r  -> return r
    Nothing -> failAtCursor (AnnotationError bl)


blockToSequence :: F.Block HA -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
blockToSequence bl = do
  setCursor bl

  chooseFrom [assignment, sequenceSpec, other]
  where
    assignment = fmap (JustAssign . (: [])) <$> assignmentBlock bl

    sequenceSpec =
      case getAnnSpec (F.getAnnotation bl) of
        Just (Specification SpecSeq m) -> do
          m' <- doTranslate failAtCursor $ translateFormula m
          return $ Just $ propAnnSeq m'
        _ -> return Nothing

    other = return $ case bl of
      F.BlComment{} -> Just emptyAnnSeq
      _             -> Just $ cmdAnnSeq bl

    -- Tries each action in the list, using the first that works and otherwise
    -- reporting an error.
    chooseFrom :: [GenM (Maybe a)] -> GenM a
    chooseFrom [] = failAtCursor $ AnnotationError bl
    chooseFrom (action : rest) = do
      m <- action
      case m of
        Just x  -> return x
        Nothing -> chooseFrom rest


assignmentBlock :: F.Block HA -> GenM (Maybe (Assignment FExpr FortranVar))
assignmentBlock bl = do
  setCursor bl
  case bl of
    -- TODO: Handle assigning to particular array indices
    F.BlStatement _ _ _ (F.StExpressionAssign _ _ lvalue rvalue) -> do
      let lnames = varNames lvalue
      Some varD theVar <- varFromScope lnames

      theExpr <- doTranslate failAtCursor $ translateExpression' varD rvalue

      let theExpr' = Expr' $ mapOperators (review chooseOp) theExpr

      return (Just (Assignment theVar theExpr'))

    _ -> return Nothing


varFromScope :: NamePair -> GenM SomeVar
varFromScope np = do
  let src = np ^. npSource
  mscoped <- view (heVarsInScope . at src)
  case mscoped of
    Just v  -> return v
    Nothing -> failAtCursor (VarNotInScope np)


setCursor :: (F.Spanned a) => a -> GenM ()
setCursor x = hsCursor .= F.getSpan x


failAtCursor :: HoareBackendError -> GenM x
failAtCursor err = do
  cursor <- use hsCursor
  GenM . lift $ failAnalysis' cursor err


infoAtCursor :: Text -> GenM ()
infoAtCursor msg = do
  cursor <- use hsCursor
  GenM . lift $ logInfo' cursor msg

--------------------------------------------------------------------------------
--  Translation
--------------------------------------------------------------------------------

class ReportAnn a where fromTranslateError :: TranslateError a -> HoareBackendError
instance ReportAnn HA where fromTranslateError = TranslateError1
instance ReportAnn () where fromTranslateError = TranslateError0


doTranslate
  :: (MonadReader CheckHoareEnv m, ReportAnn ann)
  => (HoareBackendError -> m a) -> MonadTranslate ann a -> m a
doTranslate handle action = do
  env <- asks toTranslateEnv
  case runMonadTranslate action env of
    Right x  -> return x
    Left err -> handle (fromTranslateError err)


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
