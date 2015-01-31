{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             FlexibleContexts, GADTs, DeriveGeneric #-}

{-|

This module provides a number of helper functions for working with Fortran syntax that are useful
between different analyses and transformations.

-}
module Analysis.Syntax where

-- Standard imports 
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Debug.Trace

-- Data-type generics imports
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Zipper
import Data.Typeable

-- CamFort specific functionality
import Analysis.Annotations
import Analysis.IntermediateReps
import Traverse
import Language.Fortran

-- * Comparison and ordering

{-|  'AnnotationFree' is a data type that wraps other types and denotes terms which should 
     be compared for equality modulo their annotations and source location information -}
data AnnotationFree t = AnnotationFree { annotationBound :: t } deriving Show

{-| short-hand constructor for 'AnnotationFree' -}
af = AnnotationFree
{-| short-hand deconstructor for 'AnnotationFree' -}
unaf = annotationBound

{-| A helpful function, used by the 'Eq AnnotationFree' instance that resets and source 
    location information -}
eraseSrcLocs :: (Typeable (t a), Data (t a)) => t a -> t a
eraseSrcLocs = transformBi erase' 
                    where erase' :: SrcLoc -> SrcLoc
                          erase' _ = SrcLoc { srcFilename = "", srcLine = 0, srcColumn = 0 }

{-| Sets the @SrcLoc@ information to have the filename "compact" which triggers a special 
  compact form of pretty printing in the @Show SrcLoc@ instances -}
setCompactSrcLocs :: (Typeable (t a), Data (t a)) => t a -> t a
setCompactSrcLocs = transformBi cmpact' 
                    where cmpact' :: SrcLoc -> SrcLoc
                          cmpact' (SrcLoc _ l c) = SrcLoc { srcFilename = "compact", srcLine = l, srcColumn = c }

lower = map toLower

-- Here begins varioous 'Eq' instances for instantiations of 'AnnotationFree'

instance Eq (AnnotationFree a) => Eq (AnnotationFree [a]) where
    (AnnotationFree xs) == (AnnotationFree xs') =
               if (length xs == length xs')
               then foldl (\b -> \(x, x') -> ((af x) == (af x')) && b) True (zip xs xs')
               else False 

instance Eq (AnnotationFree Int) where
    x == y = (unaf x) == (unaf y)

instance Eq (AnnotationFree Char) where
    x == y = (unaf x) == (unaf y)

instance Eq (AnnotationFree (AccessP ())) where
    x == y = (unaf x) == (unaf y)

instance (Eq (AnnotationFree a), Eq (AnnotationFree b)) => Eq (AnnotationFree (a, b)) where
    (AnnotationFree (x, y)) == (AnnotationFree (x', y')) = ((af x) == (af x')) && ((af y) == (af y'))

instance Eq (AnnotationFree (Expr a)) where
    -- Compute variable equality modulo annotations and spans
    (AnnotationFree (Var _ _ vs)) == (AnnotationFree (Var _ _ vs'))
          = cmp vs vs' where cmp [] [] = True
                             cmp ((VarName _ v,es):vs) ((VarName _ v',es'):vs') =

                                  -- Since whether variable names are upper or lower case is irrelevant
                                  -- in Fortran, we must compare variables for equality by normalising first
                                  -- (here to lower case)

                                  if (lower v) == (lower v') then 
                                         (and (map (\(e, e') -> (af e) == (af e'))
                                                 (zip es es'))) && (cmp vs vs')
                                  else False

                             cmp _ _ = False

    -- For other expressions we can get away with reseting their annotations are erasing their source locs
    (AnnotationFree e1) == (AnnotationFree e2) = (eraseSrcLocs $ fmap (const ()) e1) == 
                                                 (eraseSrcLocs $ fmap (const ()) e2)


instance Eq (AnnotationFree (Type a)) where
    (AnnotationFree (BaseType _ b attrs e1 e2)) == (AnnotationFree (BaseType _ b' attrs' e1' e2')) = 
       (af b == af b') && (af attrs == af attrs') && (af e1 == af e1') && (af e2 == af e2')

    (AnnotationFree (ArrayT _ eps b attrs e1 e2)) == (AnnotationFree (ArrayT _ eps' b' attrs' e1' e2')) =
       (af eps == af eps') && (af b == af b') && (af attrs == af attrs') && (af e1 == af e1') && (af e2 == af e2')

instance Eq (AnnotationFree (Attr p)) where
    (AnnotationFree (Dimension _ es)) == (AnnotationFree (Dimension _ es')) = af es == af es'
    (AnnotationFree x) == (AnnotationFree y) = (fmap (const ()) x) == (fmap (const ()) y)

instance Eq (AnnotationFree (BaseType p)) where
    (AnnotationFree (DerivedType _ s)) == (AnnotationFree (DerivedType _ s')) = (af s) == (af s')
    (AnnotationFree x) == (AnnotationFree y) = (fmap (const ()) x) == (fmap (const ()) y)
    

instance Eq (AnnotationFree (SubName p)) where
    (AnnotationFree (SubName _ s)) == (AnnotationFree (SubName _ s')) = (lower s) == (lower s')
    (AnnotationFree (NullSubName _)) == (AnnotationFree (NullSubName _)) = True
    _ == _ = False

instance Eq (AnnotationFree (IntentAttr p)) where
    (AnnotationFree x) == (AnnotationFree y) = (fmap (const ()) x) == (fmap (const ()) y)


instance Eq (AnnotationFree (MeasureUnitSpec p)) where
    (AnnotationFree (UnitProduct _ u)) == (AnnotationFree (UnitProduct _ u')) = (af u) == (af u')
    (AnnotationFree (UnitQuotient _ u1 u2)) == (AnnotationFree (UnitQuotient _ u1' u2')) =
       (af u1 == af u1') && (af u2 == af u2')
    (AnnotationFree (UnitNone _)) == (AnnotationFree (UnitNone _)) = True
    _ == _ = False

instance Eq (AnnotationFree (Fraction p)) where
    (AnnotationFree (IntegerConst _ n)) == (AnnotationFree (IntegerConst _ n')) = (af n) == (af n')
    (AnnotationFree (FractionConst _ p q)) == (AnnotationFree (FractionConst _ p' q')) =
       (af p == af p') && (af q == af q')
    (AnnotationFree (NullFraction _)) == (AnnotationFree (NullFraction _)) = True
    _ == _ = False


{-| Ordering on accessor syntax -}
instance Ord (AccessP ()) where
    (VarA s1) <= (VarA s2)           = s1 <= s2
    (ArrayA s1 e1) <= (ArrayA s2 e2) = if (s1 == s2) then e1 <= e2 else s1 <= s2 
    (VarA s1) <= (ArrayA s2 e1)      = True
    _ <= _                           = False

{-| Partial-ordering for expressions (constructors only so far), ignores annotations -}
instance Eq p => Ord (Expr p) where
    (Con _ _ c) <= (Con  _ _ c') = c <= c'
    e <= e'                      = error "Ordering on expressions only for constructors so far"

-- * Accessor functions for extracting various pieces of information out of syntax trees

{-| Extracts the subprocedure name from a program unit -}
getSubName :: ProgUnit p -> Maybe String
getSubName (Main _ _ (SubName _ s) _ _ _)       = Just s
getSubName (Sub _ _ _ (SubName _ s) _ _)        = Just s
getSubName (Function _ _ _ (SubName _ s) _ _ _) = Just s
getSubName (Module _ _ (SubName _ s) _ _ _ _)   = Just s
getSubName (BlockData _ _ (SubName _ s) _ _ _)  = Just s
getSubName _                                    = Nothing

{-| Extracts all accessors (variables and array indexing) from a piece of syntax -}
accesses f = nub $  [VarA (lower v) | (AssgExpr _ _ v _) <- (universeBi f)::[Expr Annotation]]
                     ++ concat [varExprToAccesses ve | ve@(Var _ _ _) <- (universeBi f)::[Expr Annotation]]
               

{-| Extracts a string of the (root) variable name from a variable expression (if it is indeed a variable
    expression -}
varExprToVariable :: Expr a -> Maybe Variable
varExprToVariable (Var _ _ ((VarName _ v, es):_)) = Just v
varExprToVariable _                               = Nothing

{-| Extracts an 'accessor' form a variable from a variable expression -}
varExprToAccess :: Expr a -> Maybe Access
varExprToAccess v = varExprToVariable v >>= (Just . VarA)

{-| Extracts all 'accessors' from a variable expression e.g.,
     @varExprToAccess@ on the syntax tree coming from @a(i, j)@ returns a list of @[VarA "a", VarA "i", VarA "j"]@ -}
varExprToAccesses :: Expr a -> [Access]
varExprToAccesses (Var _ _ ves) = [mkAccess v es | (VarName _ v, es) <- ves, all isConstant es] 
                                     where mkAccess v [] = VarA v
                                           mkAccess v es = ArrayA v (map (fmap (const ())) es)
varExprToAccesses _             = [] 


class Successors t where
    {-| Computes the 'root' successor from the current -}
    successorsRoot :: t a -> [t a]
    {-| Computes the successors nodes of a CFG (described by a zipper) for certain node types -}
    successors :: (Eq a, Typeable a) => Zipper (ProgUnit a) -> [t a]  

instance Successors Fortran where
    successorsRoot (FSeq _ _ f1 f2)          = [f1]
    successorsRoot (For _ _ _ _ _ _ f)       = [f]
    successorsRoot (If _ _ _ f efs f')       = [f]
    successorsRoot (Forall _ _ _ f)          = [f]
    successorsRoot (Where _ _ _ f Nothing)   = [f]
    successorsRoot (Where _ _ _ f (Just f')) = [f, f']
    successorsRoot (Label _ _ _ f)           = [f]
    successorsRoot _                         = []

    successors = 
        successorsF
         where
          successorsF :: forall a . (Eq a, Typeable a) => Zipper (ProgUnit a) -> [Fortran a]
          successorsF z = maybe [] id 
                           (do f <- (getHole z)::(Maybe (Fortran a))
                               ss <- return $ successorsRoot f
                               return $ ss ++ seekUp f (Just z))

          seekUp :: forall a . (Eq a, Typeable a) => Fortran a -> Maybe (Zipper (ProgUnit a)) -> [Fortran a]
          seekUp f z = case (z >>= up >>= getHole)::(Maybe (Fortran a)) of
                         Just uf -> 
                             case uf of
                               (FSeq _ _ f1 f2)     -> if (f == f1) then [f2]
                                                       else seekUp uf (z >>= up)
                               (For _ _ _ _ _ _ f') -> seekUp uf (z >>= up)
                               (If _ _ _ gf efs f') -> if (f == gf) then (maybe [] (:[]) f') ++ (map snd efs) 
                                                       else seekUp uf (z >>= up) 
                               (Forall _ _ _ f')    -> seekUp uf (z >>= up)
                               (Where _ _ _ f' _)   -> seekUp uf (z >>= up)
                               (Label _ _ _ f')     -> seekUp uf (z >>= up)
                               _                    -> []
                         Nothing -> [] 
 

{-| extract all 'right-hand side' expressions e.g. 
      @rhsExpr (parse "x = e") = parse "e"@ -}
rhsExpr :: Fortran Annotation -> [Expr Annotation]
rhsExpr (Assg _ _ _ e2)        = (universeBi e2)::[Expr Annotation]

rhsExpr (For _ _ v e1 e2 e3 _) = ((universeBi e1)::[Expr Annotation]) ++
                                  ((universeBi e2)::[Expr Annotation]) ++
                                  ((universeBi e3)::[Expr Annotation])

rhsExpr (If _ _ e f1 fes f3)    = ((universeBi e)::[Expr Annotation])
                            
rhsExpr (Allocate x sp e1 e2)   = ((universeBi e1)::[Expr Annotation]) ++
                                   ((universeBi e2)::[Expr Annotation])

rhsExpr (Call _ _ e as)         = ((universeBi e)::[Expr Annotation]) ++ 
                                   ((universeBi as)::[Expr Annotation])

rhsExpr (Deallocate _ _ es e)   = (concatMap (\e -> (universeBi e)::[Expr Annotation]) es) ++
                                    ((universeBi e)::[Expr Annotation])

rhsExpr (Forall _ _ (es, e) f)  = concatMap (\(_, e1, e2, e3) -> -- TODO: maybe different here
                                               ((universeBi e1)::[Expr Annotation]) ++
                                               ((universeBi e2)::[Expr Annotation]) ++
                                               ((universeBi e3)::[Expr Annotation])) es ++
                                    ((universeBi e)::[Expr Annotation])

rhsExpr (Nullify _ _ es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es

rhsExpr (Inquire _ _ s es)      = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
rhsExpr (Stop _ _ e)            = (universeBi e)::[Expr Annotation]
rhsExpr (Where _ _ e f _)       = (universeBi e)::[Expr Annotation]

rhsExpr (Write _ _ s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es

rhsExpr (PointerAssg _ _ _ e2)  = (universeBi e2)::[Expr Annotation]

rhsExpr (Return _ _ e)          = (universeBi e)::[Expr Annotation]
rhsExpr (Print _ _ e es)        = ((universeBi e)::[Expr Annotation]) ++ 
                                   (concatMap (\e -> (universeBi e)::[Expr Annotation]) es)
rhsExpr (ReadS _ _ s es)        = concatMap (\e -> (universeBi e)::[Expr Annotation]) es
-- rhsExpr (Label x sp s f)        = rhsExpr f
rhsExpr _                     = []

{-| extract all 'left-hand side' expressions e.g. 
      @rhsExpr (parse "x = e") = parse "x"@ -}
lhsExpr :: Fortran Annotation -> [Expr Annotation]
lhsExpr (Assg _ _ e1 e2)        = ((universeBi e1)::[Expr Annotation])
lhsExpr (For x sp v e1 e2 e3 fs) = [Var x sp [(v, [])]]
lhsExpr (PointerAssg _ _ e1 e2) = ((universeBi e1)::[Expr Annotation])
lhsExpr t                        = [] --  concatMap lhsExpr ((children t)::[Fortran Annotation])


-- * Various simple analyses

{-| Set a default monoid instances for Int -}
instance Monoid Int where
    mempty = 0
    mappend = (+)

{-| Counts the number of declarations (of variables) in a whole program -}
countVariableDeclarations :: Program Annotation -> Int
countVariableDeclarations x = sum [length xs | (Decl _ _ xs _) <- (universeBi x)::[Decl Annotation]]
                               

{-| Numbers all the statements in a program unit (successively) which is useful for analysis output -}
numberStmts :: ProgUnit Annotation -> ProgUnit Annotation
numberStmts x = let 
                  numberF :: Fortran Annotation -> State Int (Fortran Annotation)
                  numberF = descendBiM number'

                  number' :: Annotation -> State Int Annotation
                  -- actually numbers more than just statements, but this doesn't matter 
                  number' x = do n <- get 
                                 put (n + 1)
                                 return $ x { number = n }
         
                in fst $ runState (descendBiM numberF x) 0

{-| All variables from a Fortran syntax tree -}
variables f = nub $ map (map toLower) $ [v | (AssgExpr _ _ v _) <- (universeBi f)::[Expr Annotation]]
                 ++ [v | (VarName _ v) <- (universeBi f)::[VarName Annotation]] 

{-| A predicate on whether an expression is actually a constant constructor -}
isConstant :: Expr p -> Bool
isConstant (Con _ _ _)  = True
isConstant (ConL _ _ _ _) = True
isConstant (ConS _ _ _) = True
isConstant _            = False
               
{-| Free-variables in a piece of Fortran syntax -}
freeVariables :: (Data (t a), Data a) => t a -> [String]
freeVariables f = (variables f) \\ (binders f)

{-| All variables from binders -}
binders :: forall a t . (Data (t a), Typeable (t a), Data a, Typeable a) => t a -> [String]
binders f = nub $
               [v | (ArgName _ v) <- (universeBi f)::[ArgName a]] 
            ++ [v | (VarName _ v) <- (universeBi ((universeBi f)::[Decl a]))::[VarName a]]
            ++ [v | (For _ _ (VarName _ v) _ _ _ _) <- (universeBi f)::[Fortran a]]


{-| Tests whether an expression is an affine transformation (without scaling) 
  on some variable, if so returns the variable and the translation factor -}
affineMatch (Bin _ _ (Plus _) (Var _ _ [(VarName _ v, _)]) (Con _ _ n)) = Just (v, read n)
affineMatch (Bin _ _ (Plus _) (Con _ _ n) (Var _ _ [(VarName _ v, _)]))   = Just (v, read n)
affineMatch (Bin _ _ (Minus _) (Var _ _ [(VarName _ v, _)]) (Con _ _ n))    = Just (v, - read n)
affineMatch (Bin _ _ (Minus _) (Con _ _ n) (Var _  _ [(VarName _ v, _)])) = Just (v, - read n)
affineMatch (Var _ _  [(VarName _ v, _)])                               = Just (v, 0)
affineMatch _                                                           = Nothing


-- * An embedded domain-specific language for describing syntax tree queries 

{-| 'QueryCmd' provides 'commands' of which pieces of syntax to find -}

data QueryCmd t where
    Exprs  :: QueryCmd (Expr Annotation)
    Blocks :: QueryCmd (Block Annotation)
    Decls  :: QueryCmd (Decl Annotation)
    Locs   :: QueryCmd Access
    Vars   :: QueryCmd (Expr Annotation)

{-| 'from' takes a command as its first parameter, a piece of syntax as its second, and
     returns all pieces of syntax matching the query request.
     
     For example: @from Decls x@ returns a list of all declarations in @x@, of type @[Decl Annotation]@ 
     If @x@ is itself a declaration then this is returned as well (so be careful with recursive functions
     over things defined in turns of 'from'. See 'topFrom' for a solution to this.
-}
from :: forall t synTyp . (Data t, Data synTyp) => QueryCmd synTyp -> t -> [synTyp]
from Locs x = accesses x
from Vars x = [v | v@(Var _ _ _) <- (universeBi x)::[Expr Annotation]]
from _ x = (universeBi x)::[synTyp]

{-| 'topFrom' takes a command as first parameter, a piece of syntax as its second, and
     returns all pieces of syntax matching the query request that are *children* of the current
     piece of syntax. This means that it will not return itself. -}

topFrom :: forall t synTyp . (Data t, Data synTyp) => QueryCmd synTyp -> t -> [synTyp]
topFrom Locs x = accesses x
topFrom _ x = (childrenBi x)::[synTyp]


