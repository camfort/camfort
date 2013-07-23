> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}

> module Syntax where

Standard imports 

> import Data.Char
> import Data.List
> import Control.Monad.State.Lazy

Data-type generics imports

> import Data.Data
> import Data.Generics.Uniplate.Operations
> import Data.Generics.Zipper

CamFort specific functionality

> import Annotations
> import IntermediateReps
> import Traverse
> import Language.Fortran
> import Language.Haskell.Syntax (SrcLoc(..))



General helpers

> lookups :: Eq a => a -> [(a, b)] -> [b]
> lookups _ [] = []
> lookups x ((a, b):xs) = if (x == a) then b : lookups x xs
>                                     else lookups x xs

Helpers to do with source locations and parsing

> refactorSpan :: SrcSpan -> SrcSpan
> refactorSpan (SrcLoc f ll cl, SrcLoc _ lu cu) = (SrcLoc f (lu+1) 0, SrcLoc f lu cu)

> toCol0 (SrcLoc f l c) = SrcLoc f l 0

dropLine extends a span to the start of the next line
This is particularly useful if a whole line is being redacted from a source file

> dropLine :: SrcSpan -> SrcSpan
> dropLine (s1, SrcLoc f l c) = (s1, SrcLoc f (l+1) 0)


> srcLineCol :: SrcLoc -> (Int, Int)
> srcLineCol (SrcLoc _ l c) = (l, c)


Comparisons

"AnnotationFree" Denotes terms which should be compared for equality modulo
their annotaitons (and source span information)

> data AnnotationFree t = AnnotationFree { annotationBound :: t }
> af = AnnotationFree -- short constructor
> unaf = annotationBound

-- Used for testing 

> instance Eq (AnnotationFree Int) where
>     x == y = (unaf x) == (unaf y)

> instance Eq (AnnotationFree (AccessP ())) where
>     x == y = (unaf x) == (unaf y)

> instance (Eq (AnnotationFree a), Eq (AnnotationFree b)) => Eq (AnnotationFree (a, b)) where
>     (AnnotationFree (x, y)) == (AnnotationFree (x', y')) = ((af x) == (af x')) && ((af y) == (af y'))

> instance Eq (AnnotationFree (Expr a)) where
>     -- Compute variable equality modulo annotations and spans
>     (AnnotationFree (Var _ _ vs)) == (AnnotationFree (Var _ _ vs'))
>           = cmp vs vs' where cmp [] [] = True
>                              cmp ((VarName _ v,es):vs) ((VarName _ v',es'):vs') =
>                                   if (lower v) == (lower v') then
>                                          (and (map (\(e, e') -> (af e) == (af e'))
>                                                  (zip es es'))) && (cmp vs vs')
>                                   else False
>                              cmp _ _ = False
>     e == e' = error "Annotation free equality not implemented" --  False


Accessors

> getSubName :: Program p -> Maybe String
> getSubName (Main _ _ (SubName _ s) _ _ _) = Just s
> getSubName (Sub _ _ _ (SubName _ s) _ _) = Just s
> getSubName (Function _ _ _ (SubName _ s) _ _) = Just s
> getSubName (Module _ _ (SubName _ s) _ _ _ _) = Just s
> getSubName (BlockData _ _ (SubName _ s) _ _ _) = Just s
> getSubName _ = Nothing


Compute successors for certain node types

> class Successors t where
>     successorsRoot :: t a -> [t a]
>     successors :: (Eq a, Typeable a) => Zipper (Program a) -> [t a]  

> instance Successors Fortran where
>     successorsRoot (FSeq _ _ f1 f2)          = [f1]
>     successorsRoot (For _ _ _ _ _ _ f)       = [f]
>     successorsRoot (If _ _ _ f efs f')       = [f]
>     successorsRoot (Forall _ _ _ f)          = [f]
>     successorsRoot (Where _ _ _ f)           = [f]
>     successorsRoot (Label _ _ _ f)           = [f]
>     successorsRoot _                         = []

>     successors = successorsF

> successorsF :: forall a . (Eq a, Typeable a) => Zipper (Program a) -> [Fortran a]
> successorsF z = maybe [] id 
>                 (do f <- (getHole z)::(Maybe (Fortran a))
>                     ss <- return $ successorsRoot f
>                     uz <- up z
>                     uf <- (getHole uz)::(Maybe (Fortran a))
>                     return $ ss ++ case uf of
>                       (FSeq _ _ f1 f2)    -> if (f == f1) then [f2] else []
>                       (For _ _ _ _ _ _ f) -> [f]
>                       (If _ _ _ gf efs f')   -> if (f == gf) then (maybe [] (:[]) f') ++ (map snd efs) else []
>                       (Forall _ _ _ f)    -> [f]
>                       (Where _ _ _ f)     -> [f]
>                       (Label _ _ _ f)     -> []
>                       _                   -> []) 

Number statements (for analysis output)

> numberStmts :: Program Annotation -> Program Annotation
> numberStmts x = let 
>                   numberF :: Fortran Annotation -> State Int (Fortran Annotation)
>                   numberF = descendBiM number'

>                   number' :: Annotation -> State Int Annotation
>                   -- actually numbers more than just statements, but this doesn't matter 
>                   number' x = do n <- get 
>                                  put (n + 1)
>                                  return $ x { number = n }
>          
>                 in fst $ runState (descendBiM numberF x) 0


> lower = map toLower

EDSL for describing syntax tree queries

> data Tag t where
>     Exprs :: Tag (Expr Annotation)
>     Blocks :: Tag (Block Annotation)
>     Locs :: Tag Access
>     Vars :: Tag (Expr Annotation)

> from :: forall t synTyp . (Data t, Data synTyp) => Tag synTyp -> t -> [synTyp]
> from Locs x = accesses x
> from Vars x = [v | v@(Var _ _ _) <- (universeBi x)::[Expr Annotation]]
> from _ x = (universeBi x)::[synTyp]

> topFrom :: forall t synTyp . (Data t, Data synTyp) => Tag synTyp -> t -> [synTyp]
> topFrom Locs x = accesses x
> topFrom _ x = (childrenBi x)::[synTyp]


All accessors (variables and array indexing)

> accesses f = nub $  [VarA (lower v) | (AssgExpr _ _ v _) <- (universeBi f)::[Expr Annotation]]
>                      ++ concat [varExprToAccesses ve | ve@(Var _ _ _) <- (universeBi f)::[Expr Annotation]]
>                

> varExprToAccesses :: Expr a -> [Access]
> varExprToAccesses (Var _ _ ves) = [mkAccess v es | (VarName _ v, es) <- ves, all isConstant es] 
>                                      where mkAccess v [] = VarA v
>                                            mkAccess v es = ArrayA v (map (fmap (const ())) es)
> varExprToAccesses _             = [] 

> isConstant :: Expr p -> Bool
> isConstant (Con _ _ _)  = True
> isConstant (ConL _ _ _ _) = True
> isConstant (ConS _ _ _) = True
> isConstant _            = False

All variables from a Fortran syntax tree

> variables f = nub $ map (map toLower) $ [v | (AssgExpr _ _ v _) <- (universeBi f)::[Expr Annotation]]
>                  ++ [v | (VarName _ v) <- (universeBi f)::[VarName Annotation]] 
               
Free-variables in a piece of Fortran syntax

> freeVariables :: (Data (t a), Data a) => t a -> [String]
> freeVariables f = (variables f) \\ (binders f)

All variables from binders

> binders :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
> binders f = nub $
>                [v | (ArgName _ v) <- (universeBi f)::[ArgName a]] 
>             ++ [v | (VarName _ v) <- (universeBi ((universeBi f)::[Decl a]))::[VarName a]]
>             ++ [v | (For _ _ (VarName _ v) _ _ _ _) <- (universeBi f)::[Fortran a]]



> rhsExpr :: Fortran Annotation -> [Expr Annotation]
> rhsExpr (Assg _ _ _ e2)        = (universeBi e2)::[Expr Annotation]

> rhsExpr (For _ _ v e1 e2 e3 _) = ((universeBi e1)::[Expr Annotation]) ++
>                                   ((universeBi e2)::[Expr Annotation]) ++
>                                   ((universeBi e3)::[Expr Annotation])

> rhsExpr (If _ _ e f1 fes f3)    = ((universeBi e)::[Expr Annotation])
>                             
> rhsExpr (Allocate x sp e1 e2)   = ((universeBi e1)::[Expr Annotation]) ++
>                                    ((universeBi e2)::[Expr Annotation])

> rhsExpr (Call _ _ e as)         = ((universeBi e)::[Expr Annotation]) ++ 
>                                    ((universeBi as)::[Expr Annotation])

> rhsExpr (Deallocate _ _ es e)   = (concatMap (\e -> (universeBi e)::[Expr Annotation]) es) ++
>                                     ((universeBi e)::[Expr Annotation])

> rhsExpr (Forall _ _ (es, e) f)  = concatMap (\(_, e1, e2, e3) -> -- TODO: maybe different here
>                                                ((universeBi e1)::[Expr Annotation]) ++
>                                                ((universeBi e2)::[Expr Annotation]) ++
>                                                ((universeBi e3)::[Expr Annotation])) es ++
>                                     ((universeBi e)::[Expr Annotation])

> rhsExpr (Nullify _ _ es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es

> rhsExpr (Inquire _ _ s es)      = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> rhsExpr (Stop _ _ e)            = (universeBi e)::[Expr Annotation]
> rhsExpr (Where _ _ e f)         = (universeBi e)::[Expr Annotation]

> rhsExpr (Write _ _ s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es

> rhsExpr (PointerAssg _ _ _ e2)  = (universeBi e2)::[Expr Annotation]

> rhsExpr (Return _ _ e)          = (universeBi e)::[Expr Annotation]
> rhsExpr (Print _ _ e es)        = ((universeBi e)::[Expr Annotation]) ++ 
>                                    (concatMap (\e -> (universeBi e)::[Expr Annotation]) es)
> rhsExpr (ReadS _ _ s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
> -- rhsExpr (Label x sp s f)        = rhsExpr f
> rhsExpr _                     = []

> lhsExpr :: Fortran Annotation -> [Expr Annotation]
> lhsExpr (Assg _ _ e1 e2)        = ((universeBi e1)::[Expr Annotation])
> lhsExpr (For x sp v e1 e2 e3 fs) = [Var x sp [(v, [])]]
> lhsExpr (PointerAssg _ _ e1 e2) = ((universeBi e1)::[Expr Annotation])
> lhsExpr t                        = [] --  concatMap lhsExpr ((children t)::[Fortran Annotation])


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


Orderings

> instance Ord (AccessP ()) where
>     (VarA s1) <= (VarA s2)           = s1 <= s2
>     (ArrayA s1 e1) <= (ArrayA s2 e2) = if (s1 == s2) then e1 <= e2 else s1 <= s2 
>     (VarA s1) <= (ArrayA s2 e1)      = True
>     _ <= _                           = False

Partial-ordering for expressions, ignores annotations

> instance Eq p => Ord (Expr p) where
>     (Con _ _ c) <= (Con  _ _ c') = c <= c'