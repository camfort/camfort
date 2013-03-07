> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> module Main where

> import Language.Fortran.Parser
> import Language.Fortran
> import System.Environment
> import System.IO

> import Data.Generics.Uniplate.Operations

> import Output

> main :: IO ()
> main = do return ()

> quickAnnotateDo :: Fortran String -> Fortran String
> quickAnnotateDo (For _ v@(VarName _ s) e1 e2 e3 body) = For s v e1 e2 e3 body
> quickAnnotateDo t = t

> go s = do f <- readFile s
>           let f' = parse f
>           let f'' = map ((transformBi quickAnnotateDo) . (fmap (const ""))) f'
>           writeFile (s ++ ".html") (concatMap outputHTML f'')
>           return ()

