> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Types where

> import Syntax

> import Data.List
> import Data.Data
> import Control.Monad.State.Lazy

> import Data.Generics.Uniplate.Operations

> import Language.Fortran

> type TypeEnv t = [(Variable, Type t)]
> type TypeEnvStack t = [TypeEnv t] -- stack of environments     

> gtypes :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [(String, Type a)] 
> gtypes x = concat [map (\v -> (v, t)) (vars d) | (Decl _ d t) <- (universeBi x)::[Decl a]]
>            where vars d = nub $ [v | (VarName _ v) <- (universeBi d)::[VarName a]] 

> typeAnnotations :: (Typeable a, Data a) => [Program a] -> State (TypeEnv a) [Program a]
> typeAnnotations = mapM (descendBiM buildTypeEnv)

> buildTypeEnv :: (Typeable a, Data a) => Fortran a -> State (TypeEnv a) (Fortran a)
> buildTypeEnv x = do tenv <- get
>                     tenv' <- return $ gtypes x
>                     put (tenv ++ tenv')
>                     return x

 isArray :: Variable -> State (TypeEnv t) Bool
 isArray v = do tenv <- get
                case (lookup v tenv) of
                  Nothing -> error "Variable not found: " ++ v
                  Just t -> case t where
                              

 isArrayType :: 

                  
