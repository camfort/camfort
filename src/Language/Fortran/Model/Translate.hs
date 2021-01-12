{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE PolyKinds                  #-}

-- TODO: Implement translation for more unsupported language parts

{-|

Provides translation from a subset of the dynamically typed Fortran syntax
("Language.Fortran.AST") to the strongly typed expression language
("Language.Fortran.Model").

-}
module Language.Fortran.Model.Translate
  (
    -- * Types
    -- ** Fortran Expressions
    FortranExpr
    -- ** Existentials
  , Some(..)
  , SomeVar
  , SomeExpr
  , SomeType
    -- ** Semantics
  , KindSelector(..)
  , FortranSemantics(..)
  , defaultSemantics

    -- * Translation Monad
    -- ** Environment
  , TranslateEnv(..)
  , defaultTranslateEnv
    -- ** Errors
  , TranslateError(..)
    -- ** Monad
  , TranslateT(..)
  , runTranslateT

    -- * Translating Expressions
  , translateExpression
  , translateExpression'
  , translateCoerceExpression

    -- * Translating Types
    -- ** 'TypeInfo'
  , TypeInfo
  , typeInfo
    -- ** Translation
  , translateTypeInfo

    -- * Lenses
    -- ** 'FortranSemantics'
  , fsIntegerKinds
  , fsRealKinds
  , fsLogicalKinds
  , fsCharacterKinds
  , fsDoublePrecisionKinds
    -- * 'TranslateEnv'
  , teVarsInScope
  , teImplicitVars
  , teSemantics
    -- ** 'TypeInfo'
  , tiSrcSpan
  , tiBaseType
  , tiSelectorLength
  , tiSelectorKind
  , tiDeclaratorLength
  , tiDimensionDeclarators
  , tiAttributes
  ) where

import           Prelude                              hiding (span)

import           Control.Applicative                  ((<|>))
import           Data.Char                            (toLower)
import           Data.List                            (intersperse)
import           Data.Maybe                           (catMaybes)
import           Data.Typeable                        (Typeable)
import           Text.Read                            (readMaybe)

import           Control.Lens                         hiding (Const (..),
                                                       indices, op, rmap, (.>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Fail hiding            (fail)
import           Data.Map                             (Map)

import           Data.Singletons
import           Data.Singletons.Prelude.List         (Length)

import           Data.Vinyl
import           Data.Vinyl.Functor                   (Const (..))

-- TODO: unable to figure out typeclass constraints on Some (Rec D), so using
-- old Vinyl functions
import qualified Data.Vinyl.Recursive                 as VinylRec

import qualified Language.Fortran.Analysis            as F
import qualified Language.Fortran.AST                 as F
import qualified Language.Fortran.Util.Position       as F

import           Language.Expression
import           Language.Expression.Pretty

import           Camfort.Analysis.Logger
import           Camfort.Helpers.TypeLevel
import           Language.Fortran.Model.Op.Core
import           Language.Fortran.Model.Op.Meta
import           Language.Fortran.Model.Op.Core.Match
import           Language.Fortran.Model.Singletons
import           Language.Fortran.Model.Types
import           Language.Fortran.Model.Types.Match
import           Language.Fortran.Model.Vars

--------------------------------------------------------------------------------
--  General types
--------------------------------------------------------------------------------

-- | The type of strongly-typed Fortran expressions.
type FortranExpr = HFree CoreOp FortranVar

-- | A Fortran variable with an existential type.
type SomeVar = Some FortranVar

-- | A Fortran expression with an existential type.
type SomeExpr = Some (PairOf D FortranExpr)

-- | An existential Fortran type.
type SomeType = Some D

--------------------------------------------------------------------------------
--  Semantics
--------------------------------------------------------------------------------

-- | A function mapping numeric kind annotations from Fortran programs to actual
-- precision, for a particular basic type `bt`.
newtype KindSelector = KindSelector { selectKind :: Integer -> Maybe Precision }

{-|

A (currently very incomplete) specification of the semantics of a particular
version of Fortran, needed when translating.

-}
data FortranSemantics =
  FortranSemantics
  { _fsIntegerKinds         :: KindSelector
  , _fsRealKinds            :: KindSelector
  , _fsCharacterKinds       :: KindSelector
  , _fsLogicalKinds         :: KindSelector
  , _fsDoublePrecisionKinds :: Maybe KindSelector
  }

makeLenses ''FortranSemantics

{-|

== /Kinds/

The default semantics has sensible defaults for kind 0 (unspecified). Otherwise,
the kind is the number of bytes used for the type's representation. Only
power-of-two values up to 8 are valid. Characters only allow single byte
precision. Reals only allow 4- or 8-byte precision.

-}
defaultSemantics :: FortranSemantics
defaultSemantics =
  FortranSemantics
  { _fsIntegerKinds = KindSelector $ \case
      0 -> Just P64
      1 -> Just P8
      2 -> Just P16
      4 -> Just P32
      8 -> Just P64
      _ -> Nothing
  , _fsRealKinds = KindSelector $ \case
      0 -> Just P32
      4 -> Just P32
      8 -> Just P64
      _ -> Nothing
  , _fsCharacterKinds = KindSelector $ \case
      0 -> Just P8
      _ -> Nothing
  , _fsLogicalKinds = KindSelector $ \case
      0 -> Just P8
      1 -> Just P8
      2 -> Just P16
      4 -> Just P32
      8 -> Just P64
      _ -> Nothing
  , _fsDoublePrecisionKinds = Nothing
  }


--------------------------------------------------------------------------------
--  Translate Monad
--------------------------------------------------------------------------------

-- | In order to translate Fortran expressions, we require some information
-- about the environment. That information is capture in this record.
data TranslateEnv =
  TranslateEnv
  { _teImplicitVars :: Bool
    -- ^ Are implicit variable types enabled? (TODO: this currently does
    -- nothing)
  , _teVarsInScope  :: Map UniqueName SomeVar
    -- ^ A map of the variables in scope, including their types
  , _teSemantics    :: FortranSemantics
    -- ^ The version of Fortran's semantics to use when translating code.
  }

defaultTranslateEnv :: TranslateEnv
defaultTranslateEnv =
  TranslateEnv
  { _teImplicitVars = True
  , _teVarsInScope = mempty
  , _teSemantics = defaultSemantics
  }

makeLenses ''TranslateEnv

newtype TranslateT m a =
  TranslateT
  { getTranslateT
    :: ReaderT TranslateEnv (ExceptT TranslateError m) a
  }
  deriving ( Functor, Applicative, Monad
           , MonadError TranslateError
           , MonadReader TranslateEnv
           , MonadLogger e w
           , MonadFail
           )

runTranslateT
  :: (Monad m, MonadFail m)
  => TranslateT m a
  -> TranslateEnv
  -> m (Either TranslateError a)
runTranslateT (TranslateT action) env = runExceptT $ runReaderT action env

--------------------------------------------------------------------------------
--  Errors
--------------------------------------------------------------------------------

data TranslateError
  = ErrUnsupportedItem Text
  -- ^ Tried to translate a part of the language that is not (yet) supported.
  | ErrBadLiteral
  -- ^ Found a literal value that we didn't know how to translate. May or may
  -- not be valid Fortran.
  | ErrUnexpectedType Text SomeType SomeType
  -- ^ @'ErrUnexpectedType' message expected actual@: tried to translate a
  -- Fortran language part into the wrong expression type, and it wasn't
  -- coercible to the correct type.
  | ErrInvalidOpApplication (Some (Rec D))
  -- ^ Tried to apply an operator to arguments with the wrong types.
  | ErrVarNotInScope F.Name
  -- ^ Reference to a variable that's not currently in scope
  | ErrInvalidKind Text Integer
  -- ^ @'ErrInvalidKind' baseTypeName givenKind@: tried to interpret a type with
  -- the given kind which is not valid under the semantics.
  deriving (Typeable)

instance Describe TranslateError where
  describeBuilder = \case
    ErrUnsupportedItem message ->
      "unsupported " <> describeBuilder message

    ErrBadLiteral ->
      "encountered a literal value that couldn't be translated; " <>
      "it might be invalid Fortran or it might use unsupported language features"

    ErrUnexpectedType message expected actual ->
      "unexpected type in " <> describeBuilder message <>
      "; expected type was '" <> describeBuilder (show expected) <>
      "'; actual type was '" <> describeBuilder (show actual) <> "'"

    ErrInvalidOpApplication (Some (argTypes :: Rec D a)) ->
      let descTypes :: [Builder]
          descTypes = VinylRec.recordToList descTypesRec
          descTypesRec :: Rec (Const Builder) a
          descTypesRec = VinylRec.rmap (Const . surround "'" . describeBuilder . pretty1) argTypes
          surround s x = s <> x <> s
      in "tried to apply operator to arguments of the wrong type; arguments had types " <>
         mconcat (intersperse ", " descTypes)

    ErrVarNotInScope nm ->
      "reference to variable '" <> describeBuilder nm <> "' which is not in scope"

    ErrInvalidKind bt k ->
      "type with base '" <> describeBuilder bt <> "' specified a kind '" <>
      describeBuilder (show k) <> "' which is not valid under the current semantics"

unsupported :: (MonadError TranslateError m) => Text -> m a
unsupported = throwError . ErrUnsupportedItem

--------------------------------------------------------------------------------
--  Translating Types
--------------------------------------------------------------------------------

{-|

The different ways of specifying Fortran types are complicated. This record
contains information about all the different things that might contribute to a
type.

-}
data TypeInfo ann =
  TypeInfo
  { _tiSrcSpan              :: F.SrcSpan
  , _tiBaseType             :: F.BaseType
  , _tiSelectorLength       :: Maybe (F.Expression ann)
    -- ^ The length expression from a 'F.Selector' associated with a
    -- 'F.TypeSpec'.
  , _tiSelectorKind         :: Maybe (F.Expression ann)
    -- ^ The kind expression from a 'F.Selector' associated with a 'F.TypeSpec'.
  , _tiDeclaratorLength     :: Maybe (F.Expression ann)
    -- ^ The length expression from a 'F.Declarator' associated with an instance
    -- of 'F.StDeclaration'.
  , _tiDimensionDeclarators :: Maybe (F.AList F.DimensionDeclarator ann)
    -- ^ The list of dimension declarators from an instance of 'F.DeclArray'
    -- associated with an instance of 'F.StDeclaration'.
  , _tiAttributes           :: Maybe (F.AList F.Attribute ann)
    -- ^ The list of attributes from an instance of 'F.StDeclaration'.
  }
  deriving (Functor, Show)

makeLenses ''TypeInfo

instance F.Spanned (TypeInfo ann) where
  getSpan = view tiSrcSpan
  setSpan = set tiSrcSpan

-- | Create a simple 'TypeInfo' from an 'F.TypeSpec'. Many use cases will need
-- to add more information to fully specify the type.
typeInfo :: F.TypeSpec ann -> TypeInfo ann
typeInfo ts@(F.TypeSpec _ _ bt mselector) =
  let selectorLength (F.Selector _ _ l _) = l
      selectorKind (F.Selector _ _ _ k) = k
  in TypeInfo
     { _tiSrcSpan = F.getSpan ts
     , _tiBaseType = bt
     , _tiSelectorLength = mselector >>= selectorLength
     , _tiSelectorKind = mselector >>= selectorKind
     , _tiDeclaratorLength = Nothing
     , _tiDimensionDeclarators = Nothing
     , _tiAttributes = Nothing
     }


-- | Convert a 'TypeInfo' to its corresponding strong type.
translateTypeInfo
  :: (Monad m, MonadFail m, Show ann)
  => TypeInfo ann
  -> TranslateT m SomeType
translateTypeInfo ti = do
  -- TODO: Derived data types
  SomePrimD basePrim <- translateBaseType (ti ^. tiBaseType) (ti ^. tiSelectorKind)

  let
    -- If an attribute corresponds to a dimension declaration which contains a
    -- simple length dimension, get the expression out.
    attrToLength (F.AttrDimension _ _ declarators) = dimensionDeclaratorsToLength declarators
    attrToLength _                           = Nothing

    attrsToLength (F.AList _ _ attrs) =
      case catMaybes (attrToLength <$> attrs) of
        [e] -> Just e
        _   -> Nothing

    -- If a list of dimension declarators corresponds to a simple one
    -- dimensional length, get the expression out. We don't handle other cases
    -- yet.
    dimensionDeclaratorsToLength (F.AList _ _ [F.DimensionDeclarator _ _ e1 e2]) = e1 <|> e2
    dimensionDeclaratorsToLength _ = Nothing

    mLengthExp =
      (ti ^. tiSelectorLength) <|>
      (ti ^. tiDeclaratorLength) <|>
      (ti ^. tiDimensionDeclarators >>= dimensionDeclaratorsToLength) <|>
      (ti ^. tiAttributes >>= attrsToLength)

  case mLengthExp of
    Just lengthExp -> do
      -- If a length expression could be found, this variable is an array

      -- TODO: If the length expression is malformed, throw an error.
      -- TODO: Use information about the length.
      -- maybe (unsupported "type spec") void (exprIntLit lengthExp)
      case basePrim of
        DPrim bp -> return (Some (DArray (Index PInt64) (ArrPrim bp)))
    Nothing ->
      return (Some basePrim)


data SomePrimD where
  SomePrimD :: D (PrimS a) -> SomePrimD

translateBaseType
  :: (Monad m, MonadFail m)
  => F.BaseType
  -> Maybe (F.Expression ann) -- ^ Kind
  -> TranslateT m SomePrimD
translateBaseType bt mkind = do

  kindInt <- case mkind of
    Nothing -> return 0
    Just (F.ExpValue _ _ (F.ValInteger s)) ->
      case readLitInteger s of
        Just k  -> return k
        Nothing -> throwError ErrBadLiteral
    _ -> unsupported "kind which isn't an integer literal"

  let getKindPrec btName ksl = do
        mks <- preview (teSemantics . ksl)
        case mks >>= (`selectKind` kindInt) of
          Just p  -> return p
          Nothing -> throwError $ ErrInvalidKind btName kindInt

  -- Get value-level representations of the type's basic type and precision
  (basicType, prec) <- case bt of
    F.TypeInteger     -> (BTInt     ,) <$> getKindPrec "integer"   fsIntegerKinds
    F.TypeReal        -> (BTReal    ,) <$> getKindPrec "real"      fsRealKinds
    F.TypeCharacter{} -> (BTChar    ,) <$> getKindPrec "character" fsCharacterKinds
    F.TypeLogical     -> (BTLogical ,) <$> getKindPrec "logical"   fsLogicalKinds
    -- Double precision is special because it's not always supported as its own
    -- basic type, being subsumed by the `REAL` basic type.
    F.TypeDoublePrecision ->
      (BTReal,) <$> getKindPrec "double precision" (fsDoublePrecisionKinds . _Just)
    _ -> unsupported "type spec"

  -- Lift the value-level representations to the type level and get a primitive
  -- type with those properties.
  case (toSing basicType, toSing prec) of
    (SomeSing sbt, SomeSing sprec) -> case makePrim sprec sbt of
      Just (MakePrim prim) -> return (SomePrimD (DPrim prim))
      Nothing              -> unsupported "type spec"

--------------------------------------------------------------------------------
--  Translating Expressions
--------------------------------------------------------------------------------

-- | Translate an expression with an unknown type. The return value
-- existentially captures the type of the result.
translateExpression :: (Monad m, MonadFail m) => F.Expression (F.Analysis ann) -> TranslateT m SomeExpr
translateExpression = \case
  e@(F.ExpValue ann span val) -> translateValue e
  F.ExpBinary ann span bop e1 e2 -> translateOp2App e1 e2 bop
  F.ExpUnary ann span uop operand -> translateOp1App operand uop

  F.ExpSubscript ann span lhs (F.AList _ _ indices) -> translateSubscript lhs indices

  F.ExpDataRef ann span e1 e2           -> unsupported "data reference"
  F.ExpFunctionCall ann span fexpr args -> unsupported "function call"
  F.ExpImpliedDo ann span es spec       -> unsupported "implied do expression"
  F.ExpInitialisation ann span es       -> unsupported "intitialization expression"
  F.ExpReturnSpec ann span rval         -> unsupported "return spec expression"


-- | Translate an expression with a known type. Fails if the actual type does
-- not match.
translateExpression'
  :: (Monad m, MonadFail m) => D a -> F.Expression (F.Analysis ann)
  -> TranslateT m (FortranExpr a)
translateExpression' targetD ast = do
  SomePair sourceD expr <- translateExpression ast

  case dcast sourceD targetD expr of
    Just y -> return y
    Nothing -> throwError $ ErrUnexpectedType "expression" (Some sourceD) (Some targetD)


-- | Translate an expression and try to coerce it to a particular type. Fails if
-- the actual type cannot be coerced to the given type.
translateCoerceExpression
  :: (Monad m, MonadFail m) => D a -> F.Expression (F.Analysis ann)
  -> TranslateT m (HFree MetaOp FortranExpr a)
translateCoerceExpression targetD ast = do
  SomePair sourceD expr <- translateExpression ast

  -- First check if it's already the right type
  case dcast sourceD targetD expr of
    Just y -> return (HPure y)
    Nothing -> case (matchPrimD sourceD, matchPrimD targetD) of
      (Just (MatchPrimD _ sourcePrim), Just (MatchPrimD _ targetPrim)) ->
        return (HWrap (MopCoercePrim targetPrim (HPure expr)))
      _ -> throwError $ ErrUnexpectedType "expression" (Some sourceD) (Some targetD)


translateSubscript
  :: (Monad m, MonadFail m)
  => F.Expression (F.Analysis ann) -> [F.Index (F.Analysis ann)] -> TranslateT m SomeExpr
translateSubscript arrAst [F.IxSingle _ _ _ ixAst] = do
  SomePair arrD arrExp <- translateExpression arrAst
  SomePair ixD ixExp <- translateExpression ixAst

  case matchOpSpec OpLookup (arrD :& ixD :& RNil) of
    Just (MatchOpSpec opResult resultD) ->
      return $ SomePair resultD $ HWrap $ CoreOp OpLookup opResult (arrExp :& ixExp :& RNil)
    Nothing ->
      case arrD of
        -- If the LHS is indeed an array, the index type must not have matched
        DArray (Index requiredIx) _ ->
          throwError $
          ErrUnexpectedType "array indexing"
          (Some (DPrim requiredIx)) (Some ixD)
        -- If the LHS is not an array, tell the user we expected some specific
        -- array type; in reality any array type would have done.
        _ -> throwError $
          ErrUnexpectedType "array indexing"
          (Some (DArray (Index PInt64) (ArrPrim PInt64)))
          (Some arrD)

translateSubscript lhs [F.IxRange {}] =
  unsupported "range indices"
translateSubscript _ _ =
  unsupported "multiple indices"


-- | Translate a source 'F.Value' to a strongly-typed expression. Accepts an
-- 'F.Expression' which is expected to be an 'F.ExpValue' because it needs
-- access to annotations to get unique names, and 'F.Value' doesn't have any
-- annotations of its own.
--
-- Do not call on an expression that you don't know to be an 'F.ExpValue'!
translateValue :: (Monad m, MonadFail m) => F.Expression (F.Analysis ann) -> TranslateT m SomeExpr
translateValue e = case e of
  F.ExpValue _ _ v -> case v of
    F.ValInteger s -> translateLiteral v PInt64 (fmap fromIntegral . readLitInteger) s
    F.ValReal    s -> translateLiteral v PFloat (fmap realToFrac . readLitReal) s

    -- TODO: Auxiliary variables
    F.ValVariable nm -> do
      let uniq = UniqueName (F.varName e)
      theVar <- view (teVarsInScope . at uniq)
      case theVar of
        Just (Some v'@(FortranVar d _)) -> return (SomePair d (HPure v'))
        _                               -> throwError $ ErrVarNotInScope nm

    F.ValLogical s ->
      let intoBool = fmap (\b -> if b then Bool8 1 else Bool8 0) . readLitBool
      in translateLiteral v PBool8 intoBool s

    F.ValComplex r c  -> unsupported "complex literal"
    F.ValString s     -> unsupported "string literal"
    F.ValHollerith s  -> unsupported "hollerith literal"
    F.ValIntrinsic nm -> unsupported $ "intrinsic " <> describe nm
    F.ValOperator s   -> unsupported "user-defined operator"
    F.ValAssignment   -> unsupported "interface assignment"
    F.ValType s       -> unsupported "type value"
    F.ValStar         -> unsupported "star value"
    F.ValColon        -> unsupported "colon value"
  _ -> fail "impossible: translateValue called on a non-value"


translateLiteral
  :: (Monad m, MonadFail m)
  => F.Value ann
  -> Prim p k a -> (s -> Maybe a) -> s
  -> TranslateT m SomeExpr
translateLiteral v pa readLit
  = maybe (throwError ErrBadLiteral) (return . SomePair (DPrim pa) . flit pa)
  . readLit
  where
    flit px x = HWrap (CoreOp OpLit (OSLit px x) RNil)


translateOp1 :: F.UnaryOp -> Maybe (Some (Op 1))
translateOp1 = \case
  F.Minus -> Just (Some OpNeg)
  F.Plus -> Just (Some OpPos)
  F.Not -> Just (Some OpNot)
  _ -> Nothing


translateOp2 :: F.BinaryOp -> Maybe (Some (Op 2))
translateOp2 = \case
  F.Addition -> Just (Some OpAdd)
  F.Subtraction -> Just (Some OpSub)
  F.Multiplication -> Just (Some OpMul)
  F.Division -> Just (Some OpDiv)

  F.LT -> Just (Some OpLT)
  F.GT -> Just (Some OpGT)
  F.LTE -> Just (Some OpLE)
  F.GTE -> Just (Some OpGE)

  F.EQ -> Just (Some OpEq)
  F.NE -> Just (Some OpNE)

  F.And -> Just (Some OpAnd)
  F.Or -> Just (Some OpOr)
  F.Equivalent -> Just (Some OpEquiv)
  F.NotEquivalent -> Just (Some OpNotEquiv)

  _ -> Nothing


data HasLength n as where
  HasLength :: Length as ~ n => HasLength n as

-- | Given a record of 'Some' functorial types, return 'Some' record over the
-- list of those types.
--
-- In the return value, @'Some' ('PairOf' ('HasLength' n) ('Rec' f))@ is a record over
-- an unknown list of types, with the constraint that the unknown list has
-- length @n@.
recSequenceSome :: Rec (Const (Some f)) xs -> Some (PairOf (HasLength (Length xs)) (Rec f))
recSequenceSome RNil = SomePair HasLength RNil
recSequenceSome (x :& xs) = case (x, recSequenceSome xs) of
  (Const (Some y), Some (PairOf HasLength ys)) -> SomePair HasLength (y :& ys)


-- This is way too general for its own good but it was fun to write.
translateOpApp
  :: (Monad m, MonadFail m)
  => (Length xs ~ n)
  => Op n ok
  -> Rec (Const (F.Expression (F.Analysis ann))) xs -> TranslateT m SomeExpr
translateOpApp operator argAsts = do
  someArgs <- recSequenceSome <$> rtraverse (fmap Const . translateExpression . getConst) argAsts

  case someArgs of
    SomePair HasLength argsTranslated -> do
      let argsD = VinylRec.rmap (\(PairOf d _) -> d) argsTranslated
          argsExpr = VinylRec.rmap (\(PairOf _ e) -> e) argsTranslated

      MatchOpSpec opResult resultD <- case matchOpSpec operator argsD of
        Just x  -> return x
        Nothing -> throwError $ ErrInvalidOpApplication (Some argsD)

      return $ SomePair resultD $ HWrap $ CoreOp operator opResult argsExpr


translateOp2App
  :: (Monad m, MonadFail m)
  => F.Expression (F.Analysis ann) -> F.Expression (F.Analysis ann) -> F.BinaryOp
  -> TranslateT m SomeExpr
translateOp2App e1 e2 bop = do
  Some operator <- case translateOp2 bop of
    Just x  -> return x
    Nothing -> unsupported "binary operator"
  translateOpApp operator (Const e1 :& Const e2 :& RNil)


translateOp1App
  :: (Monad m, MonadFail m)
  => F.Expression (F.Analysis ann) -> F.UnaryOp
  -> TranslateT m SomeExpr
translateOp1App e uop = do
  Some operator <- case translateOp1 uop of
    Just x  -> return x
    Nothing -> unsupported "unary operator"
  translateOpApp operator (Const e :& RNil)

--------------------------------------------------------------------------------
--  Readers for things that are strings in the AST
--------------------------------------------------------------------------------

readLitInteger :: String -> Maybe Integer
readLitInteger = readMaybe

readLitReal :: String -> Maybe Double
readLitReal = readMaybe

readLitBool :: String -> Maybe Bool
readLitBool l = case map toLower l of
  ".true."  -> Just True
  ".false." -> Just False
  _         -> Nothing
