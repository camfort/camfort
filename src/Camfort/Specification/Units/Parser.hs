{-# OPTIONS_GHC -w #-}
-- -*- Mode: Haskell -*-
module Camfort.Specification.Units.Parser ( unitParser
                                     , UnitStatement(..)
                                     , UnitOfMeasure(..)
                                     , UnitPower(..)
                                     ) where

import Data.Char (isLetter, isNumber, isAlphaNum, toLower)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (UnitStatement)
	| HappyAbsSyn5 (Maybe String)
	| HappyAbsSyn6 (UnitOfMeasure)
	| HappyAbsSyn9 (UnitPower)
	| HappyAbsSyn10 (Integer)
	| HappyAbsSyn11 (String)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38 :: () => Int -> ({-HappyReduction (Maybe) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (Maybe) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

action_0 (12) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (12) = happyShift action_2
action_1 _ = happyFail

action_2 (13) = happyShift action_8
action_2 (14) = happyShift action_9
action_2 (21) = happyShift action_11
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 (8) = happyGoto action_7
action_2 _ = happyFail

action_3 (23) = happyAccept
action_3 _ = happyFail

action_4 (13) = happyShift action_8
action_4 (14) = happyShift action_9
action_4 (19) = happyShift action_10
action_4 (21) = happyShift action_11
action_4 (6) = happyGoto action_5
action_4 (7) = happyGoto action_6
action_4 (8) = happyGoto action_7
action_4 _ = happyFail

action_5 (19) = happyShift action_20
action_5 (5) = happyGoto action_19
action_5 _ = happyReduce_4

action_6 (13) = happyShift action_8
action_6 (18) = happyShift action_18
action_6 (21) = happyShift action_13
action_6 (8) = happyGoto action_17
action_6 _ = happyReduce_5

action_7 (17) = happyShift action_16
action_7 _ = happyReduce_10

action_8 _ = happyReduce_13

action_9 _ = happyReduce_6

action_10 (13) = happyShift action_15
action_10 _ = happyFail

action_11 (13) = happyShift action_8
action_11 (21) = happyShift action_13
action_11 (22) = happyShift action_14
action_11 (7) = happyGoto action_12
action_11 (8) = happyGoto action_7
action_11 _ = happyFail

action_12 (13) = happyShift action_8
action_12 (18) = happyShift action_18
action_12 (21) = happyShift action_13
action_12 (22) = happyShift action_31
action_12 (8) = happyGoto action_17
action_12 _ = happyFail

action_13 (13) = happyShift action_8
action_13 (21) = happyShift action_13
action_13 (7) = happyGoto action_12
action_13 (8) = happyGoto action_7
action_13 _ = happyFail

action_14 _ = happyReduce_7

action_15 (20) = happyShift action_30
action_15 _ = happyFail

action_16 (14) = happyShift action_26
action_16 (15) = happyShift action_27
action_16 (16) = happyShift action_28
action_16 (21) = happyShift action_29
action_16 (9) = happyGoto action_23
action_16 (10) = happyGoto action_24
action_16 (11) = happyGoto action_25
action_16 _ = happyFail

action_17 (17) = happyShift action_16
action_17 _ = happyReduce_8

action_18 (13) = happyShift action_8
action_18 (21) = happyShift action_13
action_18 (8) = happyGoto action_22
action_18 _ = happyFail

action_19 _ = happyReduce_1

action_20 (13) = happyShift action_21
action_20 _ = happyFail

action_21 _ = happyReduce_3

action_22 (17) = happyShift action_16
action_22 _ = happyReduce_9

action_23 _ = happyReduce_11

action_24 _ = happyReduce_14

action_25 _ = happyReduce_17

action_26 _ = happyReduce_20

action_27 _ = happyReduce_19

action_28 (14) = happyShift action_26
action_28 (15) = happyShift action_27
action_28 (11) = happyGoto action_34
action_28 _ = happyFail

action_29 (14) = happyShift action_26
action_29 (15) = happyShift action_27
action_29 (16) = happyShift action_28
action_29 (10) = happyGoto action_33
action_29 (11) = happyGoto action_25
action_29 _ = happyFail

action_30 (13) = happyShift action_8
action_30 (14) = happyShift action_9
action_30 (21) = happyShift action_11
action_30 (6) = happyGoto action_32
action_30 (7) = happyGoto action_6
action_30 (8) = happyGoto action_7
action_30 _ = happyFail

action_31 _ = happyReduce_12

action_32 _ = happyReduce_2

action_33 (18) = happyShift action_35
action_33 (22) = happyShift action_36
action_33 _ = happyFail

action_34 _ = happyReduce_18

action_35 (14) = happyShift action_26
action_35 (15) = happyShift action_27
action_35 (16) = happyShift action_28
action_35 (10) = happyGoto action_37
action_35 (11) = happyGoto action_25
action_35 _ = happyFail

action_36 _ = happyReduce_15

action_37 (22) = happyShift action_38
action_37 _ = happyFail

action_38 _ = happyReduce_16

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (UnitAssignment happy_var_3 happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (UnitAlias happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyTerminal (TId happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Just happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 (Nothing
	)

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn6
		 (Unitless
	)

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn6
		 (Unitless
	)

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (UnitProduct happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (UnitQuotient happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (UnitExponentiation happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn6
		 (UnitBasic happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (UnitPowerInteger happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (UnitPowerInteger happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (UnitPowerRational happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (read happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (read $ '-' : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn11
		 ("1"
	)

happyNewToken action sts stk [] =
	action 23 23 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TId "unit" -> cont 12;
	TId happy_dollar_dollar -> cont 13;
	TNum "1" -> cont 14;
	TNum happy_dollar_dollar -> cont 15;
	TMinus -> cont 16;
	TExponentiation -> cont 17;
	TDivision -> cont 18;
	TDoubleColon -> cont 19;
	TEqual -> cont 20;
	TLeftPar -> cont 21;
	TRightPar -> cont 22;
	_ -> happyError' (tk:tks)
	}

happyError_ 23 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = (>>=)
happyReturn :: () => a -> Maybe a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Maybe a
happyError' = happyError

parseUnit tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data UnitStatement =
   UnitAssignment (Maybe String) UnitOfMeasure
 | UnitAlias String UnitOfMeasure

instance Show UnitStatement where
  show (UnitAssignment (Just s) uom) = "= unit (" ++ show uom ++ ") :: " ++ s
  show (UnitAssignment Nothing uom) = "= unit (" ++ show uom ++ ")"
  show (UnitAlias s uom) = "= unit :: " ++ s ++ " = " ++ show uom

data UnitOfMeasure =
   Unitless
 | UnitBasic String
 | UnitProduct UnitOfMeasure UnitOfMeasure
 | UnitQuotient UnitOfMeasure UnitOfMeasure
 | UnitExponentiation UnitOfMeasure UnitPower

instance Show UnitOfMeasure where
  show Unitless = "1"
  show (UnitBasic s) = s
  show (UnitProduct uom1 uom2) = show uom1 ++ " " ++ show uom2
  show (UnitQuotient uom1 uom2) = show uom1 ++ " / " ++ show uom2
  show (UnitExponentiation uom exp) = show uom ++ "** (" ++ show exp ++ ")"

data UnitPower =
   UnitPowerInteger Integer
 | UnitPowerRational Integer Integer

instance Show UnitPower where
  show (UnitPowerInteger i) = show i
  show (UnitPowerRational i1 i2) = show i1 ++ "/" ++ show i2

data Token =
   TUnit
 | TDoubleColon
 | TExponentiation
 | TDivision
 | TMinus
 | TEqual
 | TLeftPar
 | TRightPar
 | TId String
 | TNum String
 deriving (Show)

lexer :: String -> Maybe [ Token ]
lexer ('=':xs) = lexer' xs
lexer _ = Nothing

addToTokens :: Token -> String -> Maybe [ Token ]
addToTokens tok rest = do
 tokens <- lexer' rest
 return $ tok : tokens

lexer' :: String -> Maybe [ Token ]
lexer' [] = Just []
lexer' (' ':xs) = lexer' xs
lexer' ('\t':xs) = lexer' xs
lexer' (':':':':xs) = addToTokens TDoubleColon xs
lexer' ('*':'*':xs) = addToTokens TExponentiation xs
lexer' ('/':xs) = addToTokens TDivision xs
lexer' ('-':xs) = addToTokens TMinus xs
lexer' ('=':xs) = addToTokens TEqual xs
lexer' ('(':xs) = addToTokens TLeftPar xs
lexer' (')':xs) = addToTokens TRightPar xs
lexer' (x:xs)
 | isLetter x = aux (\c -> isAlphaNum c || c `elem` ['\'','_','-']) TId
 | isNumber x = aux isNumber TNum
 | otherwise = Nothing
 where
   aux p cons =
     let (target, rest) = span p xs
     in lexer' rest >>= (\tokens -> return $ cons (x:target) : tokens)
lexer' _ = Nothing

unitParser :: String -> Maybe UnitStatement
unitParser src = do
 tokens <- lexer $ map toLower src
 parseUnit tokens

happyError :: [ Token ] -> Maybe a
happyError _ = Nothing
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/usr/local/lib/ghc-7.10.2/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

