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

> typeAnnotations :: (Typeable a, Data a) => [Program a] -> State (TypeEnv a) [Program a]
> typeAnnotations = mapM (descendBiM buildTypeEnv)

> typeEnv x = snd $ runState (buildTypeEnv x) []

> buildTypeEnv :: (Typeable a, Data a) => Block a -> State (TypeEnv a) (Block a)
> buildTypeEnv x = do tenv <- get
>                     tenv' <- return $ gtypes x
>                     put (tenv ++ tenv')
>                     return x


> gtypes :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [(String, Type a)] 
> gtypes x = let decAndTypes :: [([(Expr a, Expr a)], Type a)]
>                decAndTypes = [(d, t) | (Decl _ d t) <- (universeBi x)::[Decl a]]
>            in concatMap (\(d, t) -> 
>                              [(v, toArrayType t es) | (Var _ _ [(VarName _ v, es)], _) <- d]) decAndTypes

> isArrayTypeP :: Variable -> State (TypeEnv t) Bool
> isArrayTypeP v = do tenv <- get
>                     case (lookup v tenv) of
>                        Nothing -> error $ "Variable not found: " ++ v
>                        Just t -> case t of
>                                    (ArrayT _ _ _ _ _ _) -> return $ True
>                                    _                    -> return $ False

> isArrayTypeP' :: (TypeEnv t) -> Variable -> Bool
> isArrayTypeP' env v = case (lookup v env) of
>                        Nothing -> False -- probably a primitive 
>                        Just t -> case t of
>                                    (ArrayT _ _ _ _ _ _) -> True
>                                    _                    -> False
                              
> toArrayType (BaseType x b a e1 e2) es 
>                   | boundsP es = ArrayT x (bounds es) b a e1 e2
>                   | otherwise = BaseType x b a e1 e2
> toArrayType t es = t

> arrayElementType :: Type p -> Type p
> arrayElementType (ArrayT a dims t attrs kind len) = BaseType a t attrs kind len
> arrayElementType t = t

> boundsP [] = False
> boundsP ((Bound _ _ _ _):es) = True || (boundsP es)
> boundsP _ = False

> bounds [] = []                
> bounds ((Bound _ _ e1 e2):es) = (e1, e2) : (bounds es)
> bounds _ = error "Bound expression is of the wrong form"
                  
 predBounds [] = False
 predBounds [(Bound _ _ _)] = True
 predBounds ((Bound _ _ _):bs) = True || predBounds bs
 predBounds _ = False

 declsWithBounds :: forall a .  (Data a, Typeable a) => [Program a] -> [String]
 declsWithBounds x = [v | (VarName _ v, b) <- (universeBi ((universeBi x)::[Decl a]))::[(VarName a, [Expr a])], predBounds b]

