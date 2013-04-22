> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Types where

> import Syntax

> import Data.List
> import Data.Data
> import Control.Monad.State.Lazy

> import Data.Generics.Uniplate.Operations

> import Language.Fortran

> type TypeEnv t = [(VarName t, Type t)]
> type TypeEnvStack t = [TypeEnv t] -- stack of environments     

> gtypes :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [(String, Type a)] 
> gtypes x = concat [map (\v -> (v, t)) (vars d) | (Decl _ d t) <- (universeBi x)::[Decl a]]
>            where vars d = nub $ [v | (VarName _ v) <- (universeBi d)::[VarName a]] 