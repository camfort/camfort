> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE DeriveDataTypeable #-}

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

> flipFlag (ps, flag) = (ps, not flag)


> insertP :: Zipper (Expr Annotation) -> (Expr Annotation) -> Zipper (Expr Annotation)
> insertP z e' = let e = ((fromJust . getHole $ z) :: Expr Annotation)
>                in setHole (tagRoot e' (flipFlag $ extract e)) z

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



> maybeC :: (b -> Maybe b) -> b -> b
> maybeC f x = case (f x) of Nothing -> x
>                            Just x' -> x'

> upF    = maybeC up

> leftNode x = fromJust $ down' x >>= right
> rightNode x = fromJust $ down x

> rightNode' x = fromJust $ down' x >>= right >>= right


> getExpr z = (fromJust $ getHole $ z)::(Expr Annotation)
> getBounds z = fst $ extract $ ((fromJust $ getHole $ z)::(Expr Annotation))

> pprint input z = pprint' (1, 1) (lines input) z

> pprint' :: (Int, Int) -> [String] -> Zipper (Expr Annotation) -> String
> pprint' (l, c) []      _ = ""
> pprint' (l, c) ([]:[]) z = ""
> pprint' (l, c) ([]:xs) z = pprint' (l+1, 0) xs z ++ "\n"
> pprint' (l, c) inp z 
>      | inBounds (l, c) (getBounds z) = 
>             let e = fromJust $ getHole z
>             in     if (snd $ extract e) then ppr e
>                    else 
>                         case e of 
>                           Num ((lb, ub), _) _ -> fst $ takeBounds (lb, ub) inp
>                           Plus ((lb, ub), _) _ _ ->
>                                                   let lfb = getBounds (leftNode z)
>                                                       rfb = getBounds (rightNode z)
>                                                       (p1, rest1) = takeBounds (lb, fst $ lfb) inp
>                                                       p2 = pprint' (fst $ lfb) rest1 (leftNode z)
>                                                       (_, inp') = takeBounds (fst $ lfb, snd $ lfb) rest1
>                                                       (p3, rest2) = takeBounds (snd $ lfb, fst $ rfb) inp'
>                                                       p4 = pprint' (fst $ rfb) rest2 (rightNode z)
>                                                       (_, inp'') = takeBounds (fst $ rfb, snd $ rfb) rest2
>                                                       (p5, rest3) = takeBounds (snd $ rfb, ub) inp''
>                                                   in p1 ++ p2 ++ p3 ++ p4 ++ p5
>      | otherwise = pprint' (l, c) inp (upF z) -- go up the tree if current position is not within the root node 


> inBounds x (l, u) = x >= l && x < u

> takeBounds ((ll, lc), (ul, uc)) inp = takeBounds' ((ll, lc), (ul, uc)) [] inp
> takeBounds' ((ll, lc), (ul, uc)) tk inp =
>     if (ll == ul && lc == uc) then (reverse tk, inp)
>     else case inp of []             -> (reverse tk, inp)
>                      ([]:ys)        -> takeBounds' ((ll+1, 0), (ul, uc)) ('\n':tk) ys
>                      ((x:xs):ys)    -> takeBounds' ((ll, lc+1), (ul, uc)) (x:tk) (xs:ys)

> reprint :: String -> Zipper (Expr Annotation) -> String
> reprint input z = reprintP (1, 1) (lines input) z 

> reprintP :: (Line, Column) -> [String] -> Zipper (Expr Annotation) -> String
> reprintP cursor inp z = case getHole z of
>                           Just e -> 
>                                     let ((lb, ub), flag) = extract e
>                                     in  if inBounds cursor (lb, ub) then 
>                                           if flag then ppr e
>                                           else  case down' z of
>                                                   Just cz -> reprintR cursor ub inp cz 
>                                                   Nothing -> ""
>                                         else maybe "" (reprintP cursor inp) (up z)
>                           Nothing -> ""


> reprintR :: (Line, Column) -> (Line, Column) -> [String] -> Zipper (Expr Annotation) -> String
> reprintR cursor parentUb inp z = case (getHole z)::(Maybe (Expr Annotation)) of
>                                    Just e -> 
>                                        let ((lb, ub), _) = extract e
>                                            (p1, rest1)   = takeBounds (cursor, lb) inp
>                                            p2            = reprintP lb rest1 z
>                                            inp'          = snd $ takeBounds (lb, ub) rest1
>                                        in p1 ++ p2 ++ case right z of
>                                                                    Just rz -> reprintR ub parentUb inp' rz 
>                                                                    Nothing -> fst $ takeBounds (ub, parentUb) inp'
>                                    Nothing -> case right z of 
>                                               Just rz -> reprintR cursor parentUb inp rz 
>                                               Nothing -> fst $ takeBounds (cursor, parentUb) inp

> type SrcLoc = (Line, Column)

> reprintD :: String -> Zipper (Expr Annotation) -> String
> reprintD input z = let input' = lines input
>                    in reprintA (1, 1) (length input', 1 + (length $ last input')) input' z

> doHole :: SrcLoc -> [String] -> Zipper (d Annotation) -> (String, SrcLoc)
> doHole cursor inp z = case (getHole z)::(Maybe (Expr Annotation)) of
>                           Just e  -> let ((lb, ub), flag) = extract e
>                                          (p1, rest1) = takeBounds (cursor, lb) inp
>                                      in  if flag then (p1 ++ ppr e, ub)
>                                          else case (down' z) of
>                                                    Just cz -> (p1 ++ reprintA lb ub rest1 cz, ub)
>                                                    Nothing -> let (p2, _) = takeBounds (lb, ub) rest1
>                                                               in (p1, ub)
>                           Nothing -> case (down' z) of 
>                                        Just cz -> (reprintA cursor cursor inp cz, cursor)
>                                        Nothing -> ("", cursor)

> reprintA :: SrcLoc -> SrcLoc -> [String] -> Zipper (d Annotation) -> String
> reprintA cursor end inp z = let (p1, cursor') = doHole cursor inp z
>                                 (p2, inp')    = takeBounds (cursor, cursor') inp
>                             in p1 ++ case (right z) of 
>                                         Just rz -> reprintA cursor' end inp' rz
>                                         Nothing -> fst $ takeBounds (cursor', end) inp'
>                                                    
>                                               

> x = let input = "((1 +   2) +  3  )" in (toZipper (doParse input))::(Zipper (Expr Annotation))

> fooN = let input = "(1 + 2)"
>            z = toZipper (doParse input)
>        in ((getHole z)::(Maybe (Expr Annotation)), reprint input z)

> foo = let input = "((   1 +   2) +  3  )"
>           x = (toZipper (doParse input))::(Zipper (Expr Annotation))
>           x' = rightNode x
>           y = doParse "(3 + 4)"
>           z = insertP x' y 
>           w = doParse "(9 + (4 + 3))"
>           z' = insertP (rightNode $ leftNode $ upF $ z) w
>           z'' = upF $ upF $ z'
>       in (reprint input z', reprintD input z'', pprint input z') -- , maybe False (const True) (down' x >>= down' >>= down' >>= down' >>= down'))

      case ((down' z'') >>= right) of
            Nothing -> "nothing"
            Just a -> show $ getExpr a -- reprint' input z'' -- pprint input z'
