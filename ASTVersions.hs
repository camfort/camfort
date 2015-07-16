{-# LANGUAGE GADTs, EmptyDataDecls, DataKinds, KindSignatures, ExistentialQuantification #-}

data Version = F77 | F90

data AST (v :: Version) where
    Assign :: String -> AST F77
    Module :: String -> Int -> AST F90
        

data TopLevel = forall v . AST v