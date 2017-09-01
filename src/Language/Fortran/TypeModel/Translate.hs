{-# LANGUAGE TupleSections #-}
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

-- TODO: Implement translation for more unsupported language parts

{-|

Provides translation from a subset of the dynamically typed Fortran syntax
("Language.Fortran.AST") to the strongly typed expression language
("Language.Fortran.TypeModel").

-}
module Language.Fortran.TypeModel.Translate
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

import           Prelude                          hiding (span)

import           Control.Applicative              ((<|>))
import           Data.Char                        (toLower)
import           Data.List                        (intersperse)
import           Data.Maybe                       (catMaybes)
import           Data.Typeable                    (Typeable)
import           Text.Read                        (readMaybe)

import           Control.Lens                     hiding (Const (..), indices,
                                                   op, rmap, (.>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Map                         (Map)

import           Data.Singletons
import           Data.Singletons.Prelude.List     (Length)

import           Data.Vinyl
import           Data.Vinyl.Functor               (Const (..))

import qualified Language.Fortran.AST             as F
import qualified Language.Fortran.Util.Position   as F

import           Language.Expression
import           Language.Expression.Pretty

import           Camfort.Analysis.Logger
import           Camfort.Helpers.TypeLevel
import           Language.Fortran.TypeModel
import           Language.Fortran.TypeModel.Singletons
import           Language.Fortran.TypeModel.Match
import           Language.Fortran.TypeModel.Vars

--------------------------------------------------------------------------------
--  General types
--------------------------------------------------------------------------------

-- | The type of strongly-typed Fortran expressions.
type FortranExpr = Expr FortranOp FortranVar

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
  { _fsIntegerKinds   :: KindSelector
  , _fsRealKinds      :: KindSelector
  , _fsCharacterKinds :: KindSelector
  , _fsLogicalKinds   :: KindSelector
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
  , _teVarsInScope :: Map SourceName SomeVar
    -- ^ A map of the variables in scope, including their types and unique
    -- names.
  , _teSemantics :: FortranSemantics
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
           )

runTranslateT
  :: (Monad m)
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

    ErrInvalidOpApplication (Some argTypes) ->
      let descTypes
            = recordToList
            . rmap (Const . surround "'" . describeBuilder . pretty1)
            $ argTypes
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
  :: (Monad m, Show ann)
  => TypeInfo ann
  -> TranslateT m SomeType
translateTypeInfo ti = do
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
        DPrim bp -> return (Some (DArray (Index PInt64) (ArrValue bp)))
    Nothing ->
      return (Some basePrim)


data SomePrimD where
  SomePrimD :: D (PrimS a) -> SomePrimD

translateBaseType
  :: (Monad m)
  => F.BaseType
  -> Maybe (F.Expression ann) -- ^ Kind
  -> TranslateT m SomePrimD
translateBaseType bt mkind = do

  kindInt <- case mkind of
    Nothing -> return 0
    Just (F.ExpValue _ _ (F.ValInteger s)) ->
      case readLitInteger s of
        Just k -> return k
        Nothing -> throwError ErrBadLiteral
    _ -> unsupported "kind which isn't an integer literal"

  let getKindPrec btName ksl = do
        mks <- preview (teSemantics . ksl)
        case mks >>= (`selectKind` kindInt) of
          Just p -> return p
          Nothing -> throwError $ ErrInvalidKind btName kindInt

  -- Get value-level representations of the type's basic type and precision
  (basicType, prec) <- case bt of
    F.TypeInteger   -> (BTInt     ,) <$> getKindPrec "integer"   fsIntegerKinds
    F.TypeReal      -> (BTReal    ,) <$> getKindPrec "real"      fsRealKinds
    F.TypeCharacter -> (BTChar    ,) <$> getKindPrec "character" fsCharacterKinds
    F.TypeLogical   -> (BTLogical ,) <$> getKindPrec "logical"   fsLogicalKinds
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
      Nothing -> unsupported "type spec"

--------------------------------------------------------------------------------
--  Translating Expressions
--------------------------------------------------------------------------------

-- | Translate an expression with an unknown type. The return value
-- existentially captures the type of the result.
translateExpression :: (Monad m) => F.Expression ann -> TranslateT m SomeExpr
translateExpression = \case
  (F.ExpValue ann span val) -> translateValue val
  (F.ExpBinary ann span bop e1 e2) -> translateOp2App e1 e2 bop
  (F.ExpUnary ann span uop operand) -> translateOp1App operand uop

  (F.ExpSubscript ann span lhs (F.AList _ _ indices)) -> translateSubscript lhs indices

  (F.ExpDataRef ann span e1 e2)           -> unsupported "data reference"
  (F.ExpFunctionCall ann span fexpr args) -> unsupported "function call"
  (F.ExpImpliedDo ann span es spec)       -> unsupported "implied do expression"
  (F.ExpInitialisation ann span es)       -> unsupported "intitialization expression"
  (F.ExpReturnSpec ann span rval)         -> unsupported "return spec expression"


-- | Translate an expression with a known type. Fails if the actual type does
-- not match.
translateExpression'
  :: (Monad m) => D a -> F.Expression ann
  -> TranslateT m (FortranExpr a)
translateExpression' d = translateAtType "expression" d translateExpression


translateAtType
  :: (Monad m)
  => Text
  -> D b
  -> (a -> TranslateT m SomeExpr)
  -> a -> TranslateT m (FortranExpr b)
translateAtType langPart db translate x =
  do SomePair da someY <- translate x
     case dcast da db someY of
       Just y  -> return y
       Nothing -> throwError $ ErrUnexpectedType langPart (Some da) (Some db)


translateSubscript :: (Monad m) => F.Expression ann -> [F.Index ann] -> TranslateT m SomeExpr
translateSubscript arrAst [F.IxSingle _ _ _ ixAst] = do
  SomePair arrD arrExp <- translateExpression arrAst
  SomePair ixD ixExp <- translateExpression ixAst

  case matchOpR OpLookup (arrD :& ixD :& RNil) of
    Just (MatchOpR opResult resultD) ->
      return $ SomePair resultD $ EOp $ FortranOp OpLookup opResult (arrExp :& ixExp :& RNil)
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
          (Some (DArray (Index PInt64) (ArrValue PInt64)))
          (Some arrD)

translateSubscript lhs [F.IxRange {}] =
  unsupported "range indices"
translateSubscript _ _ =
  unsupported "multiple indices"

translateValue :: (Monad m) => F.Value ann -> TranslateT m SomeExpr
translateValue = \case
  v@(F.ValInteger s) -> translateLiteral v PInt64 (fmap fromIntegral . readLitInteger) s
  v@(F.ValReal    s) -> translateLiteral v PFloat (fmap realToFrac . readLitReal) s

  -- TODO: Auxiliary variables
  v@(F.ValVariable nm) -> do
    theVar <- view (teVarsInScope . at (SourceName nm))
    case theVar of
      Just (Some v'@(FortranVar d _)) -> return (SomePair d (EVar v'))
      _                               -> throwError $ ErrVarNotInScope nm

  v@(F.ValLogical s) ->
    let intoBool = fmap (\b -> if b then Bool8 1 else Bool8 0) . readLitBool
    in translateLiteral v PBool8 intoBool s

  v@(F.ValComplex r c)  -> unsupported "complex literal"
  v@(F.ValString s)     -> unsupported "string literal"
  v@(F.ValHollerith s)  -> unsupported "hollerith literal"
  v@(F.ValIntrinsic nm) -> unsupported $ "intrinsic " <> describe nm
  v@(F.ValOperator s)   -> unsupported "user-defined operator"
  v@F.ValAssignment     -> unsupported "interface assignment"
  v@(F.ValType s)       -> unsupported "type value"
  v@F.ValStar           -> unsupported "star value"


translateLiteral
  :: (Monad m)
  => F.Value ann
  -> Prim p k a -> (s -> Maybe a) -> s
  -> TranslateT m SomeExpr
translateLiteral v pa readLit
  = maybe (throwError ErrBadLiteral) (return . SomePair (DPrim pa) . flit pa)
  . readLit
  where
    flit px x = EOp (FortranOp OpLit (ORLit px x) RNil)


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


data SameLength as bs where
  SameLength :: Length as ~ Length bs => SameLength as bs

recSequenceSome :: Rec (Const (Some f)) xs -> Some (PairOf (SameLength xs) (Rec f))
recSequenceSome RNil = SomePair SameLength RNil
recSequenceSome (x :& xs) = case (x, recSequenceSome xs) of
  (Const (Some y), Some (PairOf SameLength ys)) -> SomePair SameLength (y :& ys)


-- This is way too general for its own good but it was fun to write.
translateOpApp
  :: (Monad m)
  => (Length xs ~ n)
  => Op n ok
  -> Rec (Const (F.Expression ann)) xs -> TranslateT m SomeExpr
translateOpApp operator argAsts = do
  someArgs <- rtraverse (fmap Const . translateExpression . getConst) argAsts

  case recSequenceSome someArgs of
    Some (PairOf SameLength argsTranslated) -> do
      let argsD = rmap (\(PairOf d _) -> d) argsTranslated
          argsExpr = rmap (\(PairOf _ e) -> e) argsTranslated

      MatchOpR opResult resultD <- case matchOpR operator argsD of
        Just x  -> return x
        Nothing -> throwError $ ErrInvalidOpApplication (Some argsD)

      return $ SomePair resultD $ EOp $ FortranOp operator opResult argsExpr


translateOp2App
  :: (Monad m)
  => F.Expression ann -> F.Expression ann -> F.BinaryOp
  -> TranslateT m SomeExpr
translateOp2App e1 e2 bop = do
  Some operator <- case translateOp2 bop of
    Just x  -> return x
    Nothing -> unsupported "binary operator"
  translateOpApp operator (Const e1 :& Const e2 :& RNil)


translateOp1App
  :: (Monad m)
  => F.Expression ann -> F.UnaryOp
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
