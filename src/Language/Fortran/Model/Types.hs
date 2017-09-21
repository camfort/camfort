{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wall #-}

-- TODO: Complex Numbers

{-|

Embeds Fortran's type system in Haskell via the 'D' GADT.

== Note: Phantom Types and GADTs

Lots of the data types in this module are parameterised by phantom types. These
are types which appear at the type-level, but not at the value level. They are
there to make things more type-safe.

In addition, a lot of the data types are GADTs. In a phantom-type-indexed GADT,
the phantom type often restricts which GADT constructors a particular value may
be an instance of. This is very useful for restricting value-level terms based
on type-level information.

-}

module Language.Fortran.Model.Types where

import           Data.Int                              (Int16, Int32, Int64,
                                                        Int8)
import           Data.List                             (intersperse)
import           Data.Monoid                           (Endo (..))
import           Data.Typeable                         (Typeable)
import           Data.Word                             (Word8)

import           Data.Singletons.TypeLits

import           Data.Vinyl                            hiding (Field)
import           Data.Vinyl.Functor

import           Language.Expression.Pretty

import           Language.Fortran.Model.Singletons

--------------------------------------------------------------------------------
-- * Fortran Types

{-|

This is the main embedding of Fortran types. A value of type @D a@ represents
the Fortran type which corresponds to the Haskell type @a@. @a@ is a phantom
type parameter. There is at most one instance of @D a@ for each @a@. This means
that a value of type @D a@ acts as a kind of proof that it possible to have a
Fortran type corresponding to the Haskell type @a@ -- and that when you match on
@D a@ knowing the particular @a@ you have, you know which constructor you will
get. This is a nice property because it means that GHC (with
@-fwarn-incomplete-patterns@) will not warn when you match on an impossible
case. It eliminates situations where you'd otherwise write @error "impossible:
..."@.

* @'DPrim' p :: D ('PrimS' a)@ is for primitive types. It contains a value @p@
  of type @'Prim' p k a@ for some @p@, @k@, @a@. When matching on something of
  type @D ('PrimS' a)@, you know it can only contain a primitive type.

* @'DArray' i v :: D ('Array' i v)@ is for arrays. It contains instances of @'Index'
  i@ and @'ArrValue' a@. @'Index' i@ is a proof that @i@ can be used as an index,
  and @'ArrValue' a@ is a proof that @a@ can be stored in arrays.

* @'DData' s xs :: D ('Record' name fs)@ is for user-defined data types. The
  type has a name, represented at the type level by the type parameter @name@ of
  kind 'Symbol'. The constructor contains @s :: 'SSymbol' name@, which acts as a
  sort of value-level representation of the name. 'SSymbol' is from the
  @singletons@ library. It also contains @xs :: 'Rec' ('Field' D) fs@. @fs@ is a
  type-level list of pairs, pairing field names with field types. @'Field' D
  '(fname, b)@ is a value-level pair of @'SSymbol' fname@ and @D b@. The vinyl
  record is a list of fields, one for each pair in @fs@.

-}
data D a where
  DPrim :: Prim p k a -> D (PrimS a)
  DArray :: Index i -> ArrValue a -> D (Array i a)
  DData :: SSymbol name -> Rec (Field D) fs -> D (Record name fs)

--------------------------------------------------------------------------------
-- * Semantic Types

newtype Bool8  = Bool8 { getBool8 :: Int8 } deriving (Show, Num, Eq, Typeable)
newtype Bool16 = Bool16 { getBool16 :: Int16 } deriving (Show, Num, Eq, Typeable)
newtype Bool32 = Bool32 { getBool32 :: Int32 } deriving (Show, Num, Eq, Typeable)
newtype Bool64 = Bool64 { getBool64 :: Int64 } deriving (Show, Num, Eq, Typeable)
newtype Char8  = Char8 { getChar8 :: Word8 } deriving (Show, Num, Eq, Typeable)

{-|

This newtype wrapper is used in 'DPrim' for semantic primitive types. This means
that when matching on something of type @'D' ('PrimS' a)@, we know it can't be
an array or a record.

-}
newtype PrimS a = PrimS { getPrimS :: a }
  deriving (Show, Eq, Typeable)

--------------------------------------------------------------------------------
-- * Primitive Types

{-|

Lists the allowed primitive Fortran types. For example, @'PInt8' :: 'Prim' 'P8
''BTInt' 'Int8'@ represents 8-bit integers. 'Prim' has three phantom type
parameters: precision, base type and semantic Haskell type. Precision is the
number of bits used to store values of that type. The base type represents the
corresponding Fortran base type, e.g. @integer@ or @real@. Constructors are only
provided for those Fortran types which are semantically valid, so for example no
constructor is provided for a 16-bit real. A value of type @'Prim' p k a@ can be
seen as a proof that there is some Fortran primitive type with those parameters.

-}
data Prim p k a where
  PInt8          :: Prim 'P8   'BTInt     Int8
  PInt16         :: Prim 'P16  'BTInt     Int16
  PInt32         :: Prim 'P32  'BTInt     Int32
  PInt64         :: Prim 'P64  'BTInt     Int64

  PBool8         :: Prim 'P8   'BTLogical Bool8
  PBool16        :: Prim 'P16  'BTLogical Bool16
  PBool32        :: Prim 'P32  'BTLogical Bool32
  PBool64        :: Prim 'P64  'BTLogical Bool64

  PFloat         :: Prim 'P32  'BTReal    Float
  PDouble        :: Prim 'P64  'BTReal    Double

  PChar          :: Prim 'P8   'BTChar    Char8

--------------------------------------------------------------------------------
-- * Arrays

-- | Specifies which types can be used as array indices.
data Index a where
  Index :: Prim p 'BTInt a -> Index (PrimS a)

-- | Specifies which types can be stored in arrays. Currently arrays of arrays
-- are not supported.
data ArrValue a where
  ArrPrim :: Prim p k a -> ArrValue (PrimS a)
  ArrData :: SSymbol name -> Rec (Field ArrValue) fs -> ArrValue (Record name fs)


-- | An array with a phantom index type. Mostly used at the type-level to
-- constrain instances of @'D' (Array i a)@ etc.
newtype Array i a = Array [a]

--------------------------------------------------------------------------------
-- * Records

-- | A field over a pair of name and value type.
data Field f field where
  Field :: SSymbol name -> f a -> Field f '(name, a)

-- | A type of records with the given @name@ and @fields@. Mostly used at the
-- type level to constrain instances of @'D' (Record name fields)@ etc.
data Record name fields where
  Record :: SSymbol name -> Rec (Field Identity) fields -> Record name fields

--------------------------------------------------------------------------------
-- * Combinators

-- | Any Fortran index type is a valid Fortran type.
dIndex :: Index i -> D i
dIndex (Index p) = DPrim p

-- | Anything that can be stored in Fortran arrays is a valid Fortran type.
dArrValue :: ArrValue a -> D a
dArrValue (ArrPrim p) = DPrim p
dArrValue (ArrData nameSym fieldArrValues) =
  DData nameSym (rmap (overField' dArrValue) fieldArrValues)

-- | Given a field with known contents, we can change the functor and value
-- type.
overField :: (f a -> g b) -> Field f '(name, a) -> Field g '(name, b)
overField f (Field n x) = Field n (f x)

-- | Given a field with unknown contents, we can change the functor but not the
-- value type.
overField' :: (forall a. f a -> g a) -> Field f nv -> Field g nv
overField' f (Field n x) = Field n (f x)

traverseField' :: (Functor t) => (forall a. f a -> t (g a)) -> Field f nv -> t (Field g nv)
traverseField' f (Field n x) = Field n <$> f x

-- | Combine two fields over the same name-value pair but (potentially)
-- different functors.
zipFieldsWith :: (forall a. f a -> g a -> h a) -> Field f nv -> Field g nv -> Field h nv
zipFieldsWith f (Field _ x) (Field n y) = Field n (f x y)

zip3FieldsWith
  :: (forall a. f a -> g a -> h a -> i a)
  -> Field f nv
  -> Field g nv
  -> Field h nv
  -> Field i nv
zip3FieldsWith f (Field _ x) (Field _ y) (Field n z) = Field n (f x y z)

--------------------------------------------------------------------------------
--  Pretty Printing

instance Pretty1 (Prim p k) where
  prettys1Prec p = \case
    PInt8   -> showString "integer8"
    PInt16  -> showString "integer16"
    PInt32  -> showString "integer32"
    PInt64  -> showString "integer64"
    PFloat  -> showString "real"
    PDouble -> showParen (p > 8) $ showString "double precision"
    PBool8  -> showString "logical8"
    PBool16 -> showString "logical16"
    PBool32 -> showString "logical32"
    PBool64 -> showString "logical64"
    PChar   -> showString "character"

instance Pretty1 ArrValue where
  prettys1Prec p = prettys1Prec p . dArrValue

instance (Pretty1 f) => Pretty1 (Field f) where
  prettys1Prec _ = \case
    Field fname x ->
      prettys1Prec 0 x .
      showString " " .
      withKnownSymbol fname (showString (symbolVal fname))

-- | e.g. "type custom_type { character a, integer array b }"
instance Pretty1 D where
  prettys1Prec p = \case
    DPrim px -> prettys1Prec p px
    DArray _ pv -> prettys1Prec p pv . showString " array"
    DData rname fields ->
        showParen (p > 8)
      $ showString "type "
      . withKnownSymbol rname (showString (symbolVal rname))
      . showString "{ "
      . appEndo ( mconcat
              . intersperse (Endo $ showString ", ")
              . recordToList
              . rmap (Const . Endo . prettys1Prec 0)
              $ fields)
      . showString " }"
