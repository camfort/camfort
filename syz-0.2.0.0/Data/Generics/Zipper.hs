-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Zipper
-- Copyright   :  (c) Michael D. Adams, 2010
-- License     :  BSD-style (see the LICENSE file)
-- 
-- ``Scrap Your Zippers: A Generic Zipper for Heterogeneous Types.
-- Michael D. Adams.  WGP '10: Proceedings of the 2010 ACM SIGPLAN
-- workshop on Generic programming, 2010.''
-- 
-- See <http://www.cs.indiana.edu/~adamsmd/papers/scrap_your_zippers/>
-- 
-----------------------------------------------------------------------------

{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types, GADTs #-}

module Data.Generics.Zipper (
  -- * Core types
  Zipper(),

  -- * Core interface
  -- ** Injection and projection
  toZipper, fromZipper,
  -- ** Basic movement
  left, right, down, down', up,

  -- ** Basic hole manipulation
  query,
  trans,
  transM,

  -- * Convenience hole manipulation interface
  getHole,
  setHole,
  setHole',

  -- * Generic zipper traversals
  -- ** Traversal helpers
  -- *** Query
  moveQ, leftQ, rightQ, downQ, upQ,
  -- *** Transform
  moveT, leftT, rightT, downT, upT,
  -- *** Monadic Transform
  moveM, rightM, downM, upM,

  -- *** Movement
  leftmost, rightmost,
  -- ** Map traversals
  zmapQ,
  zmapT,
  zmapM,
  zmapMp,

  -- ** Tree traversals
  zeverywhere, zeverywhere', zsomewhere, zreduce,

  -- ** Contextual/comonadic operation
  transC, zextend, transCR, zextendR

) where

import Data.Generics
import Control.Monad ((<=<), MonadPlus, mzero, mplus, liftM)
import Data.Maybe (fromJust)

-- Core types

-- newtype PZipper f a = PZipper (Zipper (f a))


-- | A generic zipper with a root object of type @root@.
data Zipper root =
  forall hole . (Data hole) =>
    Zipper hole (Context hole root)

---- Internal types and functions
data Context hole root where
    CtxtNull :: Context a a
    CtxtCons ::
      forall hole root rights parent. (Data parent) =>
        Left (hole -> rights)
        -> Right rights parent
        -> Context parent root
        -> Context hole root

combine :: Left (hole -> rights)
         -> hole
         -> Right rights parent
         -> parent
combine lefts hole rights =
  fromRight ((fromLeft lefts) hole) rights

data Left expects
  = LeftUnit expects
  | forall b. (Data b) => LeftCons (Left (b -> expects)) b

toLeft :: (Data a) => a -> Left a
toLeft a = gfoldl LeftCons LeftUnit a

fromLeft :: Left r -> r
fromLeft (LeftUnit a)   = a
fromLeft (LeftCons f b) = fromLeft f b

data Right provides parent where
  RightNull :: Right parent parent
  RightCons ::
    (Data b) => b -> Right a t -> Right (b -> a) t

fromRight :: r -> Right r parent -> parent
fromRight f (RightNull)     = f
fromRight f (RightCons b r) = fromRight (f b) r

-- Core interface

---- Injection and projection

-- | Move up a zipper to the root and return the root object.
fromZipper :: Zipper a -> a
fromZipper (Zipper hole CtxtNull) = hole
fromZipper (Zipper hole (CtxtCons l r ctxt)) =
  fromZipper (Zipper (combine l hole r) ctxt)

-- | Create a zipper.  The focus starts at the root of the object.
toZipper :: (Data a) => a -> Zipper a
toZipper x = Zipper x CtxtNull

---- Basic movement

-- | Move left.  Returns 'Nothing' iff already at leftmost sibling.
left  :: Zipper a -> Maybe (Zipper a)
left (Zipper _ CtxtNull) = Nothing
left (Zipper _ (CtxtCons (LeftUnit _) _ _)) = Nothing
left (Zipper h (CtxtCons (LeftCons l h') r c)) =
  Just (Zipper h' (CtxtCons l (RightCons h r) c))

-- | Move right.  Returns 'Nothing' iff already at rightmost sibling.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ CtxtNull) = Nothing
right (Zipper _ (CtxtCons _ RightNull _)) = Nothing
right (Zipper h (CtxtCons l (RightCons h' r) c)) =
  Just (Zipper h' (CtxtCons (LeftCons l h) r c))

-- | Move down.  Moves to rightmost immediate child.  Returns 'Nothing' iff at a leaf and thus no children exist.
down  :: Zipper a -> Maybe (Zipper a)
down (Zipper hole ctxt) =
  case toLeft hole of
    LeftUnit _ -> Nothing
    LeftCons l hole' ->
      Just (Zipper hole' (CtxtCons l RightNull ctxt))

-- | Move down. Move to the leftmost immediate child.  Returns 'Nothing' iff at a leaf and thus no children exist.
down' :: Zipper a -> Maybe (Zipper a)
down' z = liftM leftmost (down z)

-- | Move up.  Returns 'Nothing' iff already at root and thus no parent exists.
up    :: Zipper a -> Maybe (Zipper a)
up (Zipper _ CtxtNull) = Nothing
up (Zipper hole (CtxtCons l r ctxt)) =
  Just (Zipper (combine l hole r) ctxt)

---- Basic hole manipulation

-- | Apply a generic query to the hole.
query :: GenericQ b -> Zipper a -> b
query  f (Zipper hole _ctxt) = f hole

-- | Apply a generic transformation to the hole.
trans :: GenericT -> Zipper a -> Zipper a
trans  f (Zipper hole ctxt) = Zipper (f hole) ctxt

-- | Apply a generic monadic transformation to the hole
transM :: (Monad m) => GenericM m -> Zipper a -> m (Zipper a)
transM f (Zipper hole ctxt) = do
  hole' <- f hole
  return (Zipper hole' ctxt)

type GenericCT d = forall a . Data a => Zipper (d a) -> a

newtype CT d x = CT { unCT :: d x -> x }

--mkCT :: (Typeable a, Typeable b) => (Zipper (d b) -> b) -> (Zipper (d a) -> a)
--mkCT f = extCT 

--extCT :: (Typeable a, Typeable b) => (Zipper (d a) -> a) -> (Zipper (d b) -> b) -> (Zipper (d a) -> a)
--extCT def ext = unCT ((CT def) `ext0` (CT ext))

-- | Apply a generic contextual transformation at the current context
transC :: (forall b . Zipper (d a) -> b) -> Zipper (d a) -> Zipper (d a)
transC f (Zipper hole ctxt) = Zipper (f (Zipper hole ctxt)) ctxt

-- | Apply a generic contextual transformation at the current context
transCR :: (forall b . (Zipper (d a), b) -> b) -> Zipper (d a) -> Zipper (d a)
transCR f (Zipper hole ctxt) = Zipper (f (Zipper hole ctxt, hole)) ctxt

transC' :: (Data a, Typeable a) => GenericCT d -> Zipper (d a) -> Zipper (d a)
transC' f z = setHole (f z) z

--transC' :: Data (d a) => (forall b . (Data b, Typeable b) => Zipper (d a) -> b) -> Zipper (d a) -> Zipper (d a)
--transC' f z = setHole (f z) z

-- Convenience hole manipulation interface

-- | Get the value in the hole.  Returns 'Nothing' iff @a@ is not the type of the value in the hole.
getHole :: (Typeable b) => Zipper a -> Maybe b
getHole = query cast

-- | Set the value in the hole.  Does nothing iff @a@ is not the type of the value in the hole.
setHole :: (Typeable a) => a -> Zipper b -> Zipper b
setHole h z = trans (mkT (const h)) z

-- | Set the value in the hole.  Returns 'Nothing' iff @a@ is not the type of the value in the hole.
setHole' :: (Typeable a) => a -> Zipper b -> Maybe (Zipper b)
setHole' h z = transM (mkMp (const (return h))) z

-- Generic zipper traversals
---- Traversal helpers
-- | A movement operation such as 'left', 'right', 'up', or 'down'.
type Move a = Zipper a -> Maybe (Zipper a)

-- | Apply a generic query using the specified movement operation.
moveQ :: Move a -- ^ Move operation
      -> b -- ^ Default if can't move
      -> (Zipper a -> b) -- ^ Query if can move
      -> Zipper a -- ^ Zipper
      -> b
moveQ move b f z = case move z of
                     Nothing -> b
                     Just z' -> f z'

-- | Apply a generic transformer using the specified movement operations.
moveT :: Move a -- ^ Move to
      -> Move a -- ^ Move back
      -> Zipper a -- ^ Default if can't move
      -> (Zipper a -> Zipper a) -- ^ Transformer if can move
      -> Zipper a -- ^ Zipper
      -> Zipper a
moveT move1 move2 b f z =
  moveQ move1 b (moveQ move2 b id . f) z

-- | Apply a generic monadic transformer using the specified movement operations.
moveM :: (Monad m)
      => Move a -- ^ Move to
      -> Move a -- ^ Move back
      -> m (Zipper a) -- ^ Default if can't move
      -> (Zipper a -> m (Zipper a)) -- ^ Monadic transformer if can move
      -> Zipper a -- ^ Zipper
      -> m (Zipper a)
moveM move1 move2 b f z = moveQ move1 b (moveQ move2 b return <=< f) z

------ Query
-- | Apply a generic query to the left sibling if one exists.
leftQ :: b -- ^ Value to return of no left sibling exists.
      -> (Zipper a -> b) -> Zipper a -> b
leftQ b f z = moveQ left b f z

-- | Apply a generic query to the right sibling if one exists.
rightQ :: b -- ^ Value to return if no right sibling exists.
       -> (Zipper a -> b) -> Zipper a -> b
rightQ b f z = moveQ right b f z

-- | Apply a generic query to the parent if it exists. 
downQ :: b -- ^ Value to return if no children exist.
      -> (Zipper a -> b) -> Zipper a -> b
downQ b f z = moveQ down b f z

-- | Apply a generic query to the rightmost child if one exists.
upQ :: b -- ^ Value to return if parent does not exist.
    -> (Zipper a -> b) -> Zipper a -> b
upQ b f z = moveQ up b f z

------ Transform
-- | Apply a generic transformer to the left sibling if one exists.  Otherwise, leaves the zipper unchanged.
leftT :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
leftT f z = moveT left right z f z

-- | Apply a generic transformer to the right sibling if one exists.  Otherwise, leaves the zipper unchanged.
rightT :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
rightT f z = moveT right left z f z

-- | Apply a generic transformer to the rightmost child if one exists.  Otherwise, leaves the zipper unchanged.
downT :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
downT f z = moveT down up z f z

-- | Apply a generic transformer to the parent if it exists.  Otherwise, leaves the zipper unchanged.
upT :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
upT f z = g z where
  g z' = moveT right left (h z') g z'
  h z' = moveT up down z' f z'

------ Monad
-- | Apply a generic monadic transformer to the left sibling if one exists.
leftM :: (Monad m) => m (Zipper a) -- ^ Value to return if no left sibling exists.
      -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
leftM b f z = moveM left right b f z

-- | Apply a generic monadic transformer to the right sibling if one exists.
rightM :: (Monad m) => m (Zipper a) -- ^ Value to return if no right sibling exists.
      -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
rightM b f z = moveM right left b f z

-- | Apply a generic monadic transformer to the rightmost child if one exists.
downM :: (Monad m) => m (Zipper a) -- ^ Value to return if no children exist.
      -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
downM b f z = moveM down up b f z

-- | Apply a generic monadic transformer to the parent if it exists.
upM :: (Monad m) => m (Zipper a) -- ^ Value to return if parent does not exist.
      -> (Zipper a -> m (Zipper a)) -> Zipper a -> m (Zipper a)
upM b f z = g z where
  g z' = moveM right left (h z') g z'
  h z' = moveM up down b f z'

------ Movement
-- | Move to the leftmost sibling.
leftmost :: Zipper a -> Zipper a
leftmost z = leftQ z leftmost z

-- | Move to the rightmost sibling.
rightmost :: Zipper a -> Zipper a
rightmost z = rightQ z rightmost z

---- Map traversals

{- TODO: Should zmapQ and friends could be defined in terms of ZipperQ and friends?
type ZipperQ r = forall a. Zipper a -> r
type ZipperT   = forall a. Zipper a -> Zipper a
type ZipperM m = forall a. Zipper a -> m (Zipper a)
-}

-- | Apply a generic query to the immediate children.
zmapQ :: GenericQ b -> Zipper a -> [b]
zmapQ f z = reverse $ downQ [] g z where
  g z' = query f z' : leftQ [] g z'

-- | Apply a generic transformation to the immediate children.
zmapT :: GenericT -> Zipper a -> Zipper a
zmapT f z = downT g z where
  g z' = trans f (leftT g z')

-- | Apply a generic monadic transformation to the immediate children.
zmapM :: (Monad m) => GenericM m -> Zipper a -> m (Zipper a)
zmapM f z = downM (return z) g z where
  g z' = leftM (return z') (transM f) z'

-- | Apply a generic monadic transformation to at least one child that does not fail.
zmapMp :: (MonadPlus m) => GenericM m -> Zipper a -> m (Zipper a)
zmapMp f z = downQ mzero (g . leftmost) z where
  g z' = (transM f z' >>= (return . fromJust . up)) `mplus` rightQ mzero g z'
  -- TODO: there should be a cleaner way than (return . fromJust . up)

---- Tree traversals

-- | Apply a generic transformation everywhere in a bottom-up manner.
zeverywhere :: GenericT -> Zipper a -> Zipper a
zeverywhere f z = trans f (downT g z) where
  g z' = leftT g (zeverywhere f z')

-- | Apply a generic transformation everywhere in a top-down manner.
zeverywhere' :: GenericT -> Zipper a -> Zipper a
zeverywhere' f z =
  downQ (g x) (zeverywhere' f . leftmost) x where
    x = trans f z
    g z' = rightQ (upQ z' g z') (zeverywhere' f) z'

-- | Apply a generic monadic transformation once at the topmost leftmost successful location.
zsomewhere :: (MonadPlus m) => GenericM m -> Zipper a -> m (Zipper a)
zsomewhere f z = transM f z `mplus` downM mzero (g . leftmost) z where
  g z' = transM f z `mplus` rightM mzero (zsomewhere f) z'

-- | Repeatedly apply a monadic 'Maybe' generic transformation at the
-- top-most, left-most position that the transformation returns
-- 'Just'.  Behaves like iteratively applying 'zsomewhere' but is
-- more efficient because it re-evaluates the transformation
-- at only the parents of the last successful application.
zreduce :: GenericM Maybe -> Zipper a -> Zipper a
zreduce f z =
  case transM f z of
    Nothing ->
      downQ (g z) (zreduce f . leftmost) z where
        g z' = rightQ (upQ z' g z') (zreduce f) z'
    Just x  -> zreduce f (reduceAncestors f x x)

reduceAncestors ::
  GenericM Maybe -> Zipper a -> Zipper a -> Zipper a
reduceAncestors f z def = upQ def g z where
  g z' = reduceAncestors f z' def' where
    def' = case transM f z' of
             Nothing -> def
             Just x  -> reduceAncestors f x x

-- | Apply a generic contextual computation everywhere in a bottom-up manner.
zextend :: (forall b . Zipper (d a) -> b) -> Zipper (d a) -> Zipper (d a)
zextend f z = transC f (downT g z) where
  g z' = leftT g (zextend f z')

{-
zeverywhere'' :: GenericT -> Zipper a -> Zipper a
zeverywhere'' f z = downT (g . zeverywhere f . leftmost) (trans f z) where
  g z' = rightT g (zeverywhere f z')
zeverywhere' :: GenericT -> Zipper a -> Zipper a
zeverywhere' f z = g1 z where
  g1 z' = downQ (g2 z') g1 z
  g2 z' = let x = trans f z' in leftQ (g3 x) g1 x
  g3 z' = upQ z' g2 z'
-}

{-
zeverywhere2 :: GenericQ GenericT
             -> Zipper a -> Zipper b -> Zipper b
zeverywhere2 f z1 z2 = g z1 z2 where
  g z1' z2' = trans (query f z1') (downT2 h z1' z2')
  h z1' z2' = leftT2 h z1' (g z1' z2')

downT2, leftT2 ::
  (Zipper a -> Zipper b -> Zipper b)
    -> Zipper a -> Zipper b -> Zipper b
downT2 f z1 z2 = downT (downQ (const z2) f z1) z2
leftT2 f z1 z2 = leftT (leftQ (const z2) f z1) z2
-}
