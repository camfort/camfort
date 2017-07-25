{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}

module Camfort.Specification.Hoare.Syntax where

import           Control.Monad                     (ap, join)
import           Data.Data
import           GHC.Generics

import           Data.Generics.Uniplate.Operations

import qualified Language.Fortran.AST              as F
import qualified Language.Fortran.Util.Position    as F

import           Language.While.Prop


data Value l =
    ValInteger           String
  | ValReal              String
  | ValComplex           (Expression l) (Expression l)
  | ValString            String
  | ValHollerith         String
  | ValVariable          l
  | ValIntrinsic         F.Name
  | ValLogical           String
  | ValOperator          String
  | ValAssignment
  | ValType              String
  | ValStar
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data Index l =
    IxSingle (Maybe String) (Expression l)
  | IxRange (Maybe (Expression l)) -- Lower index
              (Maybe (Expression l)) -- Upper index
              (Maybe (Expression l)) -- Stride
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data Argument l = Argument (Maybe String) (Expression l)
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data Expression l
  = ExpValue        (Value l)
  | ExpBinary       F.BinaryOp (Expression l) (Expression l)
  | ExpUnary        F.UnaryOp (Expression l)
  | ExpSubscript    (Expression l) [Index l]
  | ExpDataRef      (Expression l) (Expression l)
  | ExpFunctionCall (Expression l) (Maybe [Argument l])
  | ExpInitialisation [Expression l]
  | ExpReturnSpec   (Expression l)
  deriving (Eq, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

instance Applicative Value where
  pure = return
  (<*>) = ap

instance Monad Value where
  return = ValVariable
  ValVariable x >>= f = f x
  ValComplex e1 e2 >>= f = ValComplex (e1 >>= ExpValue . f) (e2 >>= ExpValue . f)

instance Applicative Expression where
  pure = return
  (<*>) = ap

instance Monad Expression where
  return x = ExpValue (return x)
  exp >>= f =
    case exp of
      ExpValue (ValVariable x) -> f x
      ExpBinary op e1 e2 -> ExpBinary op (e1 >>= f) (e2 >>= f)
      ExpUnary op e -> ExpUnary op (e >>= f)
      ExpSubscript e is -> ExpSubscript (e >>= f) (fmap bindIndex is)
        where bindIndex (IxSingle x e') = IxSingle x (e' >>= f)
              bindIndex (IxRange e1 e2 e3) =
                IxRange (fmap (>>= f) e1)
                        (fmap (>>= f) e2)
                        (fmap (>>= f) e3)
      ExpDataRef e1 e2 -> ExpDataRef (e1 >>= f) (e2 >>= f)
      ExpFunctionCall e args -> ExpFunctionCall (e >>= f) (fmap (fmap bindArg) args)
        where bindArg (Argument x e') = Argument x (e' >>= f)
      ExpInitialisation es -> ExpInitialisation (fmap (>>= f) es)
      ExpReturnSpec e -> ExpReturnSpec (e >>= f)


data SpecKind
  = SpecPre
  | SpecPost
  | SpecSeq
  | SpecInvariant


data SpecOp a
  = OpEq a a
  | OpLT a a
  | OpLE a a
  | OpGT a a
  | OpGE a a
  deriving (Show, Data, Typeable, Functor, Foldable, Traversable)


newtype SpecFormula a =
  SpecFormula { unSpecFormula :: Prop (SpecOp (Expression a)) }
  deriving (Show, Data, Typeable, Functor, Foldable, Traversable)


bindFormula
  :: (a -> Expression b)
  -> SpecFormula a -> SpecFormula b
bindFormula f = SpecFormula . (fmap (fmap (>>= f))) . unSpecFormula


data SpecVar
  = VarProg String
  | VarAux String


data Specification =
  Specification
  { _specType :: SpecKind
  , _specFormula :: SpecFormula SpecVar
  }



convertExpression :: (Data a) => F.Expression a -> Maybe (Expression String)
convertExpression = para _
