> {-# LANGUAGE ImplicitParams #-}
> {-# LANGUAGE DeriveDataTypeable #-}

> module IntermediateReps where

> import Data.Data

> import Language.Fortran
> import Language.Fortran.Pretty

> data AccessP p = VarA String | ArrayA String [Expr p] deriving (Eq, Typeable, Data)

> type Access = AccessP ()

> accessToVarName (VarA v) = v
> accessToVarName (ArrayA v _) = v

> instance Show (AccessP ()) where
>     show (VarA s) = s
>     show (ArrayA v es) = let ?variant = Alt1 in v ++ "(" ++ (showList (map outputF es)) ++ ")"
>                            where showList []  = ""
>                                  showList [x] = x
>                                  showList (x:xs) = x ++ ", " ++ showList xs

