> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}

> module Syntax where

> import Traverse

> import Data.Data
> import Data.List
> import Data.Generics.Uniplate.Operations
> import Control.Monad.State.Lazy

> import Annotations
> import Language.Fortran

> import Language.Haskell.Syntax (SrcLoc(..))

Denotes terms which should be treated "annotation free", for example, for annotation
free equality

> data AnnotationFree t = AnnotationFree { annotationBound :: t }
> af = AnnotationFree

> instance Eq (AnnotationFree (Expr a)) where
>     -- Compute variable equality modulo annotations and spans
>     (AnnotationFree (Var _ _ vs)) == (AnnotationFree (Var _ _ vs'))
>           = cmp vs vs' where cmp [] [] = True
>                              cmp ((v,es):vs) ((v',es'):vs') =
>                                   if (fmap (const ()) v) == (fmap (const ()) v') then
>                                          (and (map (\(e, e') -> (af e) == (af e')) (zip es es'))) && (cmp vs vs')
>                                   else False
>                              cmp _ _ = False
>     e == e' = error "Annotation free equality not implemented" --  False

Helpers to do with source locations

> refactorSpan :: SrcSpan -> SrcSpan
> refactorSpan (SrcLoc f ll cl, SrcLoc _ lu cu) = (SrcLoc f (lu+1) 0, SrcLoc f lu cu)

dropLine extends a span to the start of the next line
This is particularly useful if a whole line is being redacted from a source file

> dropLine :: SrcSpan -> SrcSpan
> dropLine (s1, SrcLoc f l c) = (s1, SrcLoc f (l+1) 0)

> srcLineCol :: SrcLoc -> (Int, Int)
> srcLineCol (SrcLoc _ l c) = (l, c)

Accessors

> class Successors t where
>     successors :: t -> [t]

> instance Successors (Fortran t) where
>     successors (FSeq _ _ f1 f2)         = f2 : successors f1
>     successors (For _ _ _ _ _ _ f)        = [f]
>     successors (If _ _ _ f efs Nothing)   = f : map snd efs
>     successors (If _ _ _ f efs (Just f')) = [f, f'] ++ map snd efs
>     successors (Forall _ _ _ f)           = [f]
>     successors (Where _ _ _ f)            = [f]
>     successors (Label _ _ _ f)            = [f]
>     successors _                        = []


Number statements (for analysis output)

> numberStmts :: Program Annotation -> Program Annotation
> numberStmts x = let 
>                   numberF :: Fortran Annotation -> State Int (Fortran Annotation)
>                   numberF = descendBiM number'

>                   number' :: Annotation -> State Int Annotation
>                   number' x = do n <- get 
>                                  put (n + 1)
>                                  return $ x { number = n }
>          
>                 in fst $ runState (descendBiM numberF x) 0

All variables from a Fortran syntax tree

> variables :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> variables f = nub $
>                  [v | (AssgExpr _ _ v _) <- (universeBi f)::[Expr a]]
>               ++ [v | (VarName _ v) <- (universeBi f)::[VarName a]] 
               
Free-variables in a piece of Fortran syntax

> freeVariables :: (Data (t a), Data a) => t a -> [String]
> freeVariables f = (variables f) \\ (binders f)

All variables from binders

> binders :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> binders f = nub $
>                [v | (ArgName _ v) <- (universeBi f)::[ArgName a]] 
>             ++ [v | (VarName _ v) <- (universeBi ((universeBi f)::[Decl a]))::[VarName a]]
>             ++ [v | (For _ _ (VarName _ v) _ _ _ _) <- (universeBi f)::[Fortran a]]


 fi :: Fortran Annotation -> [Expr Annotation]
 fi t@(Assg _ e1 e2) = (universeBi e2)::[Expr Annotation]
 fi t = (concatMap fi ((children t)::[Fortran Annotation])) ++ ((childrenBi t) :: [Expr Annotation])

> lhsExpr :: Fortran Annotation -> [Expr Annotation]
> lhsExpr (Assg x sp e1 e2)        = (universeBi e2)::[Expr Annotation]
> lhsExpr (For x sp v e1 e2 e3 fs) = ((universeBi e1)::[Expr Annotation]) ++
>                              ((universeBi e2)::[Expr Annotation]) ++
>                              ((universeBi e3)::[Expr Annotation]) ++ 
>                             lhsExpr fs
> lhsExpr (FSeq x sp f1 f2)        = lhsExpr f1 ++ lhsExpr f2
> lhsExpr (If x sp e f1 fes f3)    = ((universeBi e)::[Expr Annotation]) ++
>                              (lhsExpr f1) ++ 
>                              (concatMap (\(e, f) -> ((universeBi e)::[Expr Annotation]) ++ lhsExpr f) fes) ++
>                               (case f3 of 
>                                  Nothing -> []
>                                  Just x' -> lhsExpr x')
>                             
> lhsExpr (Allocate x sp e1 e2)    = ((universeBi e1)::[Expr Annotation]) ++
>                              ((universeBi e2)::[Expr Annotation])
> lhsExpr (Call x sp e as)         = (universeBi e)::[Expr Annotation] 
> lhsExpr (Deallocate x sp es e)   = (concatMap (\e -> (universeBi e)::[Expr Annotation]) es) ++
>                              ((universeBi e)::[Expr Annotation])
> lhsExpr (Forall x sp (es, e) f)  = concatMap (\(_, e1, e2, e3) -> -- TODO: maybe different here
>                                            ((universeBi e1)::[Expr Annotation]) ++
>                                             ((universeBi e2)::[Expr Annotation]) ++
>                                             ((universeBi e3)::[Expr Annotation])) es ++
>                             ((universeBi e)::[Expr Annotation]) ++
>                             lhsExpr f 
> lhsExpr (Nullify x sp es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (Inquire x sp s es)      = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (Stop x sp e)            = (universeBi e)::[Expr Annotation]
> lhsExpr (Where x sp e f)         = ((universeBi e)::[Expr Annotation]) ++ lhsExpr f
> lhsExpr (Write x sp s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr (PointerAssg x sp e1 e2) = ((universeBi e1)::[Expr Annotation]) ++
>                             ((universeBi e2)::[Expr Annotation])
> lhsExpr (Return x sp e)          = (universeBi e)::[Expr Annotation]
> lhsExpr (Label x sp s f)         = lhsExpr f
> lhsExpr (Print x sp e es)        = ((universeBi e)::[Expr Annotation]) ++ 
>                              (concatMap (\e -> (universeBi e)::[Expr Annotation]) es)
> lhsExpr (ReadS x sp s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> lhsExpr _                     = []

> rhsExpr :: Fortran Annotation -> [Expr Annotation]
> rhsExpr (Assg x sp e1 e2)        = (universeBi e1)::[Expr Annotation]
> rhsExpr t                     = concatMap rhsExpr ((children t)::[Fortran Annotation])

 rhsExpr (For x v e1 e2 e3 fs) = rhsExpr fs
 rhsExpr (FSeq x f1 f2)        = rhsExpr f1 ++ rhsExpr f2
 rhsExpr (If x e f1 fes f3)    = (rhsExpr f1) ++ 
                              (concatMap (\(e, f) -> rhsExpr f) fes) ++
                               (case f3 of 
                                  Nothing -> []
                                  Just x -> rhsExpr x)
                             
 rhsExpr (Forall x (es, e) f)  = rhsExpr f -- TODO: maybe different here
 rhsExpr (Where x e f)         = rhsExpr f
 rhsExpr (Label x s f)         = rhsExpr f
 rhsExpr t                     = [] -- concatMap rhsExpr ((universeBi t)::[Fortran Annotation])

> affineMatch (Bin _ _ (Plus _) (Var _ _ [(VarName _ v, _)]) (Con _ _ n)) = Just (v, read n)
> affineMatch (Bin _ _ (Plus _) (Con _ _ n) (Var _ _ [(VarName _ v, _)]))   = Just (v, read n)
> affineMatch (Bin _ _ (Minus _) (Var _ _ [(VarName _ v, _)]) (Con _ _ n))    = Just (v, - read n)
> affineMatch (Bin _ _ (Minus _) (Con _ _ n) (Var _  _ [(VarName _ v, _)])) = Just (v, - read n)
> affineMatch (Var _ _  [(VarName _ v, _)])                               = Just (v, 0)
> affineMatch _                                                           = Nothing


 indexVariables :: Program Annotation -> Program Annotation
 indexVariables = descendBi indexVariables'

 indexVariables' :: Block Annotation -> Block Annotation
 indexVariables' x = 
     let typeEnv = snd $ runState (buildTypeEnv x) []

         indexVars :: Fortran Annotation -> Annotation
         indexVars y = let is = [e | (Var _ [(VarName _ v, e)]) <- (universeBi y)::[Expr Annotation], length e > 0, isArrayTypeP' typeEnv v]
                           indices = [v | (VarName _ v) <- (universeBi is)::[VarName Annotation]]
                       in setIndices (nub indices) (extract y) 
     in extendBi indexVars x