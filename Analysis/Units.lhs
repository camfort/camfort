> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> module Analysis.Units where

> import Data.List
> import Data.Data
> import Control.Monad.State.Lazy

> import Data.Generics.Uniplate.Operations

> import Analysis.Syntax
> import Analysis.Types
> import Language.Fortran

> type UnitEnv t = [(Variable, MeasureUnit t)]
> type UnitEnvStack t = [UnitEnv t] -- stack of environments

> unitAnnotations :: (Typeable a, Data a) => Program a -> State (UnitEnv a) (Program a)
> unitAnnotations = mapM (descendBiM buildUnitEnv)

> unitEnv :: (Typeable a, Data a) => Block a -> UnitEnv a
> unitEnv x = snd $ runState (buildUnitEnv x) []

> buildUnitEnv :: (Typeable a, Data a) => Block a -> State (UnitEnv a) (Block a)
> buildUnitEnv x = do uenv <- get
>                     uenv' <- return $ gunits x
>                     put (uenv ++ uenv')
>                     return x

> eqUnit :: (Eq t) => Variable -> Variable -> UnitEnv t -> Bool
> eqUnit v1 v2 vs = case lookup v1 vs of
>                     Nothing -> False
>                     Just u1 -> case lookup v2 vs of
>                                  Nothing -> False
>                                  Just u2 -> (u1 == u2)

> gunits :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [(String, MeasureUnit a)]
> gunits x = let extractUnit :: Attr a -> [MeasureUnit a]
>                extractUnit attr = case attr of
>                                     MeasureUnit _ unit -> [unit]
>                                     _ -> []
>            in [(name, last $ foldr (++) [] $ map extractUnit attrs) | (name, BaseType _ _ attrs _ _) <- gtypes x]
