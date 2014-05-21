{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Types where

import Data.List
import Data.Char
import Data.Data
import Control.Monad.State.Lazy

import Data.Generics.Uniplate.Operations

import Analysis.Syntax
import Language.Fortran

type TypeEnv t = [(Variable, Type t)]
type TypeEnvStack t = [TypeEnv t] -- stack of environments     

typeAnnotations :: (Typeable a, Data a) => Program a -> State (TypeEnv a) (Program a)
typeAnnotations = mapM (descendBiM buildTypeEnv)

typeEnv :: (Typeable a, Data a) => Block a -> TypeEnv a
typeEnv x = snd $ runState (buildTypeEnv x) []

tenvLookup :: Variable -> TypeEnv t -> Maybe (Type t)
tenvLookup v = lookup (lowercase v) 


buildTypeEnv :: (Typeable a, Data a) => Block a -> State (TypeEnv a) (Block a)
buildTypeEnv x = do tenv <- get
                    tenv' <- return $ gtypes x
                    put (tenv ++ tenv')
                    return x

eqType :: Variable -> Variable -> TypeEnv t -> Bool
eqType v1 v2 vs = case lookup v1 vs of
                    Nothing -> False
                    Just t1 -> case lookup v2 vs of 
                                 Nothing -> False
                                 Just t2 -> (AnnotationFree t1 == AnnotationFree t2)
                      

gtypes :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> TypeEnv a
gtypes x = let decAndTypes :: [([(Expr a, Expr a, Maybe Int)], Type a)]
               decAndTypes = [(d, t) | (Decl _ _ d t) <- (universeBi x)::[Decl a]]
           in concatMap (\(d, t) -> 
                             [(lowercase v, toArrayType t es) | (Var _ _ [(VarName _ v, es)], _, _) <- d]) decAndTypes

lowercase = map toLower 

isArrayTypeP :: Variable -> State (TypeEnv t) Bool
isArrayTypeP v = do tenv <- get
                    case (lookup v tenv) of
                       Nothing -> error $ "Variable not found: " ++ v
                       Just t -> case t of
                                   (ArrayT _ _ _ _ _ _) -> return $ True
                                   _                    -> return $ False

isArrayTypeP' :: (TypeEnv t) -> Variable -> Bool
isArrayTypeP' env v = case (lookup v env) of
                       Nothing -> False -- probably a primitive 
                       Just t -> case t of
                                   (ArrayT _ _ _ _ _ _) -> True
                                   _                    -> False
                              
toArrayType (BaseType x b a e1 e2) es 
                  | boundsP es = ArrayT x (bounds es) b a e1 e2
                  | otherwise = BaseType x b a e1 e2
toArrayType t es = t

arrayElementType :: Type p -> Type p
arrayElementType (ArrayT a dims t attrs kind len) = BaseType a t attrs kind len
arrayElementType t = t

boundsP [] = False
boundsP ((Bound _ _ _ _):es) = True || (boundsP es)
boundsP _ = False

bounds [] = []                
bounds ((Bound _ _ e1 e2):es) = (e1, e2) : (bounds es)
bounds _ = error "Bound expression is of the wrong form"
                  

{- OLD 
 predBounds [] = False
 predBounds [(Bound _ _ _)] = True
 predBounds ((Bound _ _ _):bs) = True || predBounds bs
 predBounds _ = False

 declsWithBounds :: forall a .  (Data a, Typeable a) => [Program a] -> [String]
 declsWithBounds x = [v | (VarName _ v, b) <- (universeBi ((universeBi x)::[Decl a]))::[(VarName a, [Expr a])], predBounds b]

-}