> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Text.Parsec.Char
> import qualified Text.ParserCombinators.Parsec.Token

> import Data.Maybe
> import Data.Data
> import Data.Generics.Zipper

> import Debug.Trace

> import Control.Comonad

> data Expr a = Plus a (Expr a) (Expr a) | Num a Int deriving (Show, Data, Typeable)

> instance Comonad Expr where
>     extract (Plus x _ _) = x
>     extract (Num x _)    = x 

> tagRoot :: Expr a -> a -> Expr a
> tagRoot (Plus _ e1 e2) x = Plus x e1 e2
> tagRoot (Num _ n)      x = Num x n

> instance Functor Expr where
>     fmap f (Plus x e1 e2) = Plus (f x) (fmap f e1) (fmap f e2)
>     fmap f (Num x n) = Num (f x) n


> insertP :: forall a . (Typeable a, FlipFlag a) => Zipper (Expr a) -> (Expr a) -> Zipper (Expr a)
> insertP z e' = let e = ((fromJust . getHole $ z) :: Expr a)
>                in setHole (tagRoot e' (flipFlag $ extract e)) z

> class FlipFlag t where
>     flipFlag :: t -> t

> instance FlipFlag Annotation where
>     flipFlag (ps, flag) = (ps, not flag)

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


> maybeC :: (b -> Maybe b) -> b -> b
> maybeC f x = case (f x) of Nothing -> x
>                            Just x' -> x'

> upF    = maybeC up
> leftF  = maybeC left
> rightF = maybeC right

> pprint input z = pprint' (1, 1) (lines input) z

> pprint' :: (Int, Int) -> [String] -> Zipper (Expr Annotation) -> String
> pprint' (l, c) []      _ = ""
> pprint' (l, c) ([]:[]) z = ""
> pprint' (l, c) ([]:xs) z = pprint' (l+1, 0) xs z ++ "\n"
> pprint' (l, c) inp z 
>      | (inBounds (l, c) (fst $ extract $ ((fromJust $ getHole z)::(Expr Annotation)))) = 
>             let e = fromJust $ getHole z
>             in     if (snd $ extract e) then ppr e
>                    else 
>                         case e of 
>                           Num ((lb, ub), _) _ -> let (tk, rest) = takeBounds (lb, ub) inp
>                                                  in tk
>                           Plus ((lb, ub), _) _ _ ->
>                                                   let lfb = fst $ extract $ ((fromJust $ getHole $ leftF z)::(Expr Annotation))
>                                                       rfb = fst $ extract $ ((fromJust $ getHole $ rightF z)::(Expr Annotation))
>                                                       (p1, rest1) = takeBounds (lb, fst $ lfb) inp
>                                                       p2 = pprint' (fst $ lfb) rest1 (leftF z)
>                                                       (_, inp') = takeBounds (fst $ lfb, snd $ lfb) rest1
>                                                       (p3, rest2) = takeBounds (snd $ lfb, fst $ rfb) inp'
>                                                       p4 = pprint' (fst $ rfb) rest2 (rightF z)
>                                                       (_, inp'') = takeBounds (fst $ rfb, snd $ rfb) rest2
>                                                       (p5, rest3) = takeBounds (snd $ rfb, ub) inp''
>                                                   in p1 ++ p2 ++ p3 ++ p4 ++ p5
>      | otherwise = pprint' (l, c) inp (upF z)

> foo = let input = "((1 +   2) +  3  )"
>           x = toZipper (doParse input)
>           x' = rightF x
>           y = doParse "(3 + 4)"
>           z = insertP x' y 
>           w = doParse "(9 + (4 + 3))"
>           z' = insertP (leftF $ upF $ z) w
>           z'' = upF $ z'
>       in pprint input z''

 instance (Num a, Num b) => Num (a, b) where
     (a, b) + (x, y) = (a + x, b + y)
     (a, b) * (x, y) = (a * x, b * y)
     (a, b) - (x, y) = (a - x, b - y)