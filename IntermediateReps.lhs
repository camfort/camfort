> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module IntermediateReps where

> import Data.Data

> import Language.Fortran
> import Language.Fortran.Pretty

> data Access = VarA String | ArrayA String [Expr ()] deriving (Eq, Typeable, Data)

> instance Show Access where
>     show (VarA s) = s
>     show (ArrayA v es) = let ?variant = Alt1 in v ++ "(" ++ (showList (map outputF es)) ++ ")"
>                            where showList []  = ""
>                                  showList [x] = x
>                                  showList (x:xs) = x ++ ", " ++ showList xs

