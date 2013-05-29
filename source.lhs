> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Text.Parsec.Char
> import qualified Text.ParserCombinators.Parsec.Token

> import Debug.Trace

> import Control.Comonad

> data Expr a = Plus a (Expr a) (Expr a) | Num a Int deriving Show

> instance Comonad Expr where
>     extract (Plus x _ _) = x
>     extract (Num x _)    = x 

> tagRoot :: Expr a -> a -> Expr a
> tagRoot (Plus _ e1 e2) x = Plus x e1 e2
> tagRoot (Num _ n)      x = Num x n

> instance Functor Expr where
>     fmap f (Plus x e1 e2) = Plus (f x) (fmap f e1) (fmap f e2)
>     fmap f (Num x n) = Num (f x) n

> data ExprZ a = ExprZ (Path a) (Expr a) deriving Show
> data Path a = L a (Path a) (Expr a) | R a (Path a) (Expr a) | Top deriving Show

> left (ExprZ p (Plus n l r)) = ExprZ (L n p r) l 
> left (ExprZ p (Num a n))    = ExprZ p (Num a n)

> right (ExprZ p (Plus n l r)) = ExprZ (R n p l) r
> right (ExprZ p (Num a n))    = ExprZ p (Num a n)

> up (ExprZ (L a p r) l) = ExprZ p (Plus a l r)
> up (ExprZ (R a p l) r) = ExprZ p (Plus a l r)
> up (ExprZ Top e)       = ExprZ Top e

> isTop (ExprZ Top _) = True
> isTop _             = False

> isLeaf (ExprZ _ (Num _ _)) = True
> isLeaf _                   = False

> insertP :: FlipFlag a => ExprZ a -> Expr a -> ExprZ a
> insertP (ExprZ p e) e' = ExprZ p (tagRoot e' (flipFlag $ extract e))

> class FlipFlag t where
>     flipFlag :: t -> t

> instance FlipFlag Annotation where
>     flipFlag (ps, flag) = (ps, not flag)

> instance Functor Path where
>     fmap f Top = Top
>     fmap f (L x p t) = L (f x) (fmap f p) (fmap f t)
>     fmap f (R x p t) = R (f x) (fmap f p) (fmap f t)

> instance Functor ExprZ where
>     fmap f (ExprZ p t) = ExprZ (fmap f p) (fmap f t)

> instance Comonad ExprZ where
>     extract (ExprZ p (Num x n)) = x
>     extract (ExprZ p (Plus x _ _)) = x

>     extend k tz = fmap k (ExprZ (cjoinP tz) (cjoinT tz))

> cjoinT :: ExprZ t -> Expr (ExprZ t)
> cjoinT t@(ExprZ p (Num x n)) = Num t n
> cjoinT t = Plus t (cjoinT . left $ t) (cjoinT . right $ t)

> cjoinP :: ExprZ t -> Path (ExprZ t)
> cjoinP (ExprZ Top t) = Top
> cjoinP z@(ExprZ (L _ _ _) _) = L (up z) (cjoinP (up z)) (cjoinT (right . up $ z)) 
> cjoinP z@(ExprZ (R _ _ _) _) = R (up z) (cjoinP (up z)) (cjoinT (left . up $ z)) 

> type Annotation =  (((Line, Column), (Line, Column)), Bool)

> tagPos p = do pos  <- getPosition
>               x    <- p 
>               pos' <- getPosition
>               return $ tagRoot x ((((sourceLine pos, sourceColumn pos), 
>                                           (sourceLine pos', sourceColumn pos')), False))

> expr = tagPos $ (try exprNum) <|> exprPlus

> exprPlus = do string "("
>               spaces
>               e1 <- expr
>               spaces
>               string "+"
>               spaces
>               e2 <- expr
>               spaces
>               string ")"
>               return $ Plus undefined e1 e2 

> exprNum = do n <- many1 digit
>              return $ Num undefined (read n)

         
> doParse :: String -> Expr Annotation
> doParse input = let p = do { e <- expr; eof; return e }
>                 in case (runParser p () "" input) of
>                      Left err  -> error (show err)
>                      Right x   -> x
>                 

> ppr (Plus _ e1 e2) = "(" ++ ppr e1 ++ " + " ++ ppr e2 ++ ")"
> ppr (Num _ n)      = show n

> inBounds x (l, u) = x >= l && x < u

> takeBounds ((ll, lc), (ul, uc)) inp = takeBounds' ((ll, lc), (ul, uc)) [] inp

> takeBounds' ((ll, lc), (ul, uc)) tk inp =
>     if (ll == ul && lc == uc) then (reverse tk, inp)
>     else case inp of []             -> (reverse tk, inp)
>                      ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
>                      ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)


> pprint input z = pprint' (1, 1) (lines input) z

> pprint' :: (Int, Int) -> [String] -> ExprZ Annotation -> String
> pprint' (l, c) []      _ = ""
> pprint' (l, c) ([]:[]) z = ""
> pprint' (l, c) ([]:xs) z = pprint' (l+1, 0) xs z ++ "\n"
> pprint' (l, c) inp z@(ExprZ p e)
>      | (inBounds (l, c) (fst $ extract z)) = 
>                    if (snd $ extract z) then ppr e
>                    else 
>                         case e of 
>                           Num ((lb, ub), _) _ -> let (tk, rest) = takeBounds (lb, ub) inp
>                                                  in tk
>                           Plus ((lb, ub), _) _ _ ->
>                                                   let lfb = fst $ extract $ left z
>                                                       rfb = fst $ extract $ right z
>                                                       (p1, rest1) = takeBounds (lb, fst $ lfb) inp
>                                                       p2 = pprint' (fst $ lfb) rest1 (left z)
>                                                       (_, inp') = takeBounds (fst $ lfb, snd $ lfb) rest1
>                                                       (p3, rest2) = takeBounds (snd $ lfb, fst $ rfb) inp'
>                                                       p4 = pprint' (fst $ rfb) rest2 (right z)
>                                                       (_, inp'') = takeBounds (fst $ rfb, snd $ rfb) rest2
>                                                       (p5, rest3) = takeBounds (snd $ rfb, ub) inp''
>                                                   in p1 ++ p2 ++ p3 ++ p4 ++ p5
>      | otherwise = pprint' (l, c) inp (up z)

> foo = let input = "((1 +   2) +  3  )"
>           x = ExprZ Top $ doParse input
>           x' = right x
>           y = doParse "(3 + 4)"
>           z = insertP x' y 
>           w = doParse "(9 + (4 + 3))"
>           z' = insertP (left $ up $ z) w
>           z'' = up $ z'
>       in pprint input z''

 instance (Num a, Num b) => Num (a, b) where
     (a, b) + (x, y) = (a + x, b + y)
     (a, b) * (x, y) = (a * x, b * y)
     (a, b) - (x, y) = (a - x, b - y)