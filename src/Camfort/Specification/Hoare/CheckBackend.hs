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
import Data.Vinyl.Curry

import           Data.SBV                               (SBool, defaultSMTCfg)

import qualified Language.Fortran.Analysis              as F
import qualified Language.Fortran.AST                   as F
import qualified Language.Fortran.LValue                as F
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
  = VerifierError (VerifierError FortranVar FExpr)
  | TranslateErrorAnn TranslateError
  -- ^ Unit errors come from translating annotation formulae
  | TranslateErrorSrc TranslateError
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
  | WrongAssignmentType Text SomeType
  -- ^ Expected array type but got the given type instead
  | NonLValueAssignment
  -- ^ Assigning to an expression that isn't an lvalue
  | UnsupportedAssignment Text
  -- ^ Tried to assign to something that's valid Fortran but unsupported

instance Describe HoareBackendError where
  describeBuilder = \case
    VerifierError e -> "verifier error: " <> describeBuilder (displayException e)
    TranslateErrorAnn te -> "translation error in logic annotation: " <> describeBuilder te
    TranslateErrorSrc te -> "translation error in source code: " <> describeBuilder te
    UnsupportedBlock _ -> "encountered unsupported block"
    UnexpectedBlock _ -> "a block was found in an illegal location"
    ArgWithoutDecl nm ->
      "argument " <> describeBuilder (show nm) <>
      " doesn't have an associated type declaration"
    AnnotationError _ ->
      "the program was insufficiently annotated; " <>
      "`seq` annotations must appear before each command which is not an assignment"
    VarNotInScope nm -> "variable " <> describeBuilder (pretty nm) <>
      " is being assigned to but is not in scope"
    MissingWhileInvariant _ ->
      "found a `do while` block with no invariant; " <>
      "invariant annotations must appear at the start of every `do while` loop"
    WrongAssignmentType message gotType ->
      "unexpected variable type; expected " <> describeBuilder message <>
      "; got " <> describeBuilder (pretty gotType)
    NonLValueAssignment ->
      "assignment an expression which is not a valid lvalue"
    UnsupportedAssignment message ->
      "unsupported assignment; " <> describeBuilder message

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

type ScopeVars = Map SourceName SomeVar

data CheckHoareEnv =
  CheckHoareEnv
  { _heImplicitVars :: Bool
  , _heVarsInScope  :: ScopeVars
  -- ^ The variables in scope. Keyed by source name, and values have unique
  -- names.
  }

initialState :: CheckHoareState
initialState = CheckHoareState

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
          . traverse (tryTranslateFormula pu)

    preconds <- translateFormulae (apu ^. apuPreconditions)
    postconds <- translateFormulae (apu ^. apuPostconditions)

    logInfo' pu $ " - Interpreting pre- and postconditions"

    let precond = propAnd preconds
        postcond = propAnd postconds

    logInfo' pu $ " - Found preconditions: "  <> describe (pretty precond)
    logInfo' pu $ " - Found postconditions: " <> describe (pretty postcond)

    return (precond, postcond, body)

  logInfo' pu $ " - Computing verification conditions"

  (_, vcs) <- runGenM env (genBody bodyTriple)

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
      rettype' <- runReaderT (tryTranslateTypeInfo (typeInfo rettype)) emptyEnv

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
    unless hasType $ failAnalysis' pu (ArgWithoutDecl argName)

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
          F.StDeclaration _ _ astTypeSpec attrs decls -> do
            -- This is the part of the type info that applies to every variable
            -- in the declaration list.
            let topTypeInfo = tiWithAttributes attrs $ typeInfo astTypeSpec

            -- Each variable may have extra information that modifies its type info
            declVarsTis <- forM (F.aStrip decls) $ \case
              -- TODO: Deal with declarations that include assignments
              F.DeclVariable _ _ nameExp declLength Nothing ->
                return (nameExp, tiWithDeclLength declLength topTypeInfo)
              F.DeclArray _ _ nameExp declDims declLength Nothing ->
                return (nameExp, tiWithDeclLength declLength .
                                 tiWithDimensionDecls (Just declDims) $
                                 topTypeInfo)

              _ -> failAnalysis' bl (UnsupportedBlock bl)

            forM_ declVarsTis $ \(varName, varTypeInfo) -> do
              varType <- readerOfState $ tryTranslateTypeInfo varTypeInfo
              heVarsInScope %= newVar varName varType

            return True

          F.StImplicit _ _ Nothing -> do
            -- TODO: Deal with implicits properly
            return True
          F.StImplicit _ _ (Just _) -> failAnalysis' bl (UnsupportedBlock bl)
          _ -> return False

      F.BlComment{} -> return True
      _ -> return False

-- TODO: Maybe report a warning when two variables have the same source name.

varOfType :: NamePair -> SomeType -> SomeVar
varOfType names (Some d) = Some (FortranVar d names)

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
    , MonadLogger HoareBackendError Void
    , MonadAnalysis HoareBackendError Void
    )


runGenM :: CheckHoareEnv -> GenM a -> CheckHoare (a, [TransFormula Bool])
runGenM env (GenM action) = evalRWST action env initialState


type FortranTriplet a = Triplet FExpr FortranVar a


genBody :: FortranTriplet [F.Block HA] -> GenM ()
genBody = traverseOf _3 bodyToSequence >=> void . sequenceVCs genBlock


genBlock :: FortranTriplet (F.Block HA) -> GenM ()
genBlock (precond, postcond, bl) = do
  case bl of
    F.BlIf _ _ _ _ conds bodies _ -> do
      condsExprs <- traverse (traverse tryTranslateBoolExpr) conds
      multiIfVCs genBody expr (precond, postcond, (zip condsExprs bodies))

    F.BlDoWhile _ _ _ _ cond body _ -> do
      primInvariant <-
        case body of
         b : _ | Just (Specification SpecInvariant f) <- getAnnSpec (F.getAnnotation b) -> return f
         _ -> failAnalysis' bl $ MissingWhileInvariant bl

      invariant <- tryTranslateFormula body primInvariant
      condExpr <- tryTranslateBoolExpr cond

      whileVCs genBody expr invariant (precond, postcond, (condExpr, body))

    F.BlComment _ _ _ -> return ()
    _ -> failAnalysis' bl $ UnsupportedBlock bl


bodyToSequence :: [F.Block HA] -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
bodyToSequence blocks = do
  foldlM combineBlockSequence emptyAnnSeq blocks


combineBlockSequence
  :: AnnSeq FExpr FortranVar (F.Block HA)
  -> F.Block HA
  -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
combineBlockSequence prevSeq bl = do
  blSeq <- blockToSequence bl

  case prevSeq `joinAnnSeq` blSeq of
    Just r  -> return r
    Nothing -> failAnalysis' bl $ AnnotationError bl


blockToSequence :: F.Block HA -> GenM (AnnSeq FExpr FortranVar (F.Block HA))
blockToSequence bl = do
  chooseFrom [assignment, sequenceSpec, other]
  where
    assignment =
      assignmentBlock bl >>= \case
        Just a -> return $ Just $ JustAssign [a]
        Nothing -> return $ Nothing

    sequenceSpec =
      case getAnnSpec (F.getAnnotation bl) of
        Just (Specification SpecSeq m) -> do
          m' <- tryTranslateFormula bl m
          return $ Just $ propAnnSeq m'
        _ -> return Nothing

    other = return $ case bl of
      F.BlComment{} -> Just emptyAnnSeq
      _             -> Just $ cmdAnnSeq bl

    -- Tries each action in the list, using the first that works and otherwise
    -- reporting an error.
    chooseFrom :: [GenM (Maybe a)] -> GenM a
    chooseFrom [] = failAnalysis' bl $ AnnotationError bl
    chooseFrom (action : rest) = do
      m <- action
      case m of
        Just x  -> return x
        Nothing -> chooseFrom rest

--------------------------------------------------------------------------------
-- Handling assignments

primAssignment
  :: (ReportAnn ann)
  => NamePair
  -> F.Expression ann
  -> GenM (Assignment FExpr FortranVar)
primAssignment nm rvalAst = do
  Some varV@(FortranVar varD _) <- varFromScope rvalAst nm

  case varD of
    DPrim _ -> do
      rvalExpr <- tryTranslateExpr varD rvalAst
      return (Assignment varV (fortranToFExpr rvalExpr))
    _ -> failAnalysis' rvalAst $ WrongAssignmentType "primitive value" (Some varD)

arrayAssignment
  :: (ReportAnn ann)
  => NamePair
  -> F.Expression ann
  -> F.Expression ann
  -> GenM (Assignment FExpr FortranVar)
arrayAssignment arrName ixAst rvalAst = do
  Some varV@(FortranVar varD _) <- varFromScope rvalAst arrName

  case varD of
    DArray ixIndex valAv -> do
      let ixD = dIndex ixIndex
          valD = dArrValue valAv

      ixExpr   <- tryTranslateExpr ixD ixAst
      rvalExpr <- tryTranslateExpr valD rvalAst

      -- Replace instances of the array variable with the same array, but with
      -- the new value written at the given index.
      let arrExpr = EVar varV
          mkOp = rcurry $ FortranOp OpWriteArr (ORWriteArr varD)
          arrExpr' = EOp (mkOp arrExpr ixExpr rvalExpr)

      return (Assignment varV (fortranToFExpr arrExpr'))

    _ -> failAnalysis' rvalAst $ WrongAssignmentType "array type" (Some varD)


assignmentBlock :: F.Block HA -> GenM (Maybe (Assignment FExpr FortranVar))
assignmentBlock bl = do
  case bl of
    F.BlStatement _ _ _ stAst@(F.StExpressionAssign _ _ lexp rvalue) ->
      Just <$> do
        lvalue <- maybe (failAnalysis' lexp NonLValueAssignment)
                  return (F.toLValue lexp)

        case lvalue of
          F.LvSimpleVar {} -> primAssignment (varNames lexp) rvalue
          F.LvSubscript _ _ lvar@(F.LvSimpleVar {}) ixs ->
            case ixs of
              F.AList _ _ [F.IxSingle _ _ _ ixExpr] ->
                arrayAssignment (lvVarNames lvar) ixExpr rvalue

              _ -> failAnalysis' ixs $
                   UnsupportedAssignment "only simple indices are supported for now"
          _ -> failAnalysis' stAst $
               UnsupportedAssignment "complex assignment"

    _ -> return Nothing


--------------------------------------------------------------------------------

varFromScope :: (F.Spanned a) => a -> NamePair -> GenM SomeVar
varFromScope loc np = do
  let src = np ^. npSource
  mscoped <- view (heVarsInScope . at src)
  case mscoped of
    Just v  -> return v
    Nothing -> failAnalysis' loc $ VarNotInScope np

--------------------------------------------------------------------------------
--  Translation
--------------------------------------------------------------------------------

class ReportAnn a where fromTranslateError :: proxy a -> TranslateError -> HoareBackendError
instance ReportAnn HA where fromTranslateError _ = TranslateErrorSrc
instance ReportAnn () where fromTranslateError _ = TranslateErrorAnn


doTranslate
  :: (MonadReader CheckHoareEnv m, ReportAnn ann)
  => (HoareBackendError -> m a) -> (f ann -> TranslateT m a) -> f ann -> m a
doTranslate handle trans ast = do
  env <- asks toTranslateEnv
  transResult <- runTranslateT (trans ast) env
  case transResult of
    Right x  -> return x
    Left err -> handle (fromTranslateError ast err)


toTranslateEnv :: CheckHoareEnv -> TranslateEnv
toTranslateEnv env =
  defaultTranslateEnv
    & teImplictVars .~ env ^. heImplicitVars
    & teVarsInScope .~ env ^. heVarsInScope

--------------------------------------------------------------------------------
-- Shorthands for translating expressions and failing the analysis if the
-- translation fails

tryTranslateExpr
  :: (ReportAnn ann,
      MonadReader CheckHoareEnv m,
      MonadAnalysis HoareBackendError w m)
  => D a -> F.Expression ann -> m (FortranExpr a)
tryTranslateExpr d e = doTranslate (failAnalysis' e) (translateExpression' d) e

tryTranslateTypeInfo
  :: (ReportAnn ann,
      MonadReader CheckHoareEnv m,
      MonadAnalysis HoareBackendError w m)
  => TypeInfo ann -> m SomeType
tryTranslateTypeInfo ti = doTranslate (failAnalysis' ti) translateTypeInfo ti

tryTranslateBoolExpr
  :: (ReportAnn ann,
      MonadReader CheckHoareEnv m,
      MonadAnalysis HoareBackendError w m)
  => F.Expression ann -> m (FExpr FortranVar Bool)
tryTranslateBoolExpr e = doTranslate (failAnalysis' e) translateBoolExpression e

tryTranslateFormula
  :: (F.Spanned o, ReportAnn ann,
      MonadReader CheckHoareEnv m,
      MonadAnalysis HoareBackendError w m)
  => o -> PrimFormula ann -> m (TransFormula Bool)
tryTranslateFormula loc e = doTranslate (failAnalysis' loc) translateFormula e

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

lvVarNames :: F.LValue (F.Analysis x) -> NamePair
lvVarNames e =
  let uniqueName = UniqueName $ F.lvVarName e
      srcName = SourceName $ F.lvSrcName e
  in NamePair uniqueName srcName


readerOfState :: (MonadState s m) => ReaderT s m a -> m a
readerOfState action = do
  st <- get
  runReaderT action st
