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
> gtypes x = let decAndTypes :: [([(Expr a, Expr a)], Type a)]
>                decAndTypes = [(d, t) | (Decl _ d t) <- (universeBi x)::[Decl a]]
>            in concatMap (\(d, t) -> 
>                              [(v, toArrayType t es) | (Var _ [(VarName _ v, es)], _) <- d]) decAndTypes

> typeAnnotations :: (Typeable a, Data a) => [Program a] -> State (TypeEnv a) [Program a]
> typeAnnotations = mapM (descendBiM buildTypeEnv)

> buildTypeEnv :: (Typeable a, Data a) => Fortran a -> State (TypeEnv a) (Fortran a)
> buildTypeEnv x = do tenv <- get
>                     tenv' <- return $ gtypes x
>                     put (tenv ++ tenv')
>                     return x

> isArrayTypeP :: Variable -> State (TypeEnv t) Bool
> isArrayTypeP v = do tenv <- get
>                     case (lookup v tenv) of
>                        Nothing -> error $ "Variable not found: " ++ v
>                        Just t -> case t of
>                                    (ArrayT _ _ _ _ _ _) -> return $ True
>                                    _                    -> return $ False
                              
> toArrayType (BaseType x b a e1 e2) es 
>                   | boundsP es = ArrayT x (bounds es) b a e1 e2
>                   | otherwise = BaseType x b a e1 e2
> toArrayType t es = t
>  

> boundsP [] = True
> boundsP ((Bound _ _ _):es) = True && (boundsP es)
> boundsP _ = False

> bounds [] = []                
> bounds ((Bound _ e1 e2):es) = (e1, e2) : (bounds es)
> bounds _ = error "Bound expression is of the wrong form"
                  
