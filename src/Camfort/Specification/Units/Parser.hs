{-# OPTIONS_GHC -w #-}
-- -*- Mode: Haskell -*-

{-# LANGUAGE DeriveDataTypeable #-}
module Camfort.Specification.Units.Parser ( unitParser
                                     , UnitStatement(..)
                                     , UnitOfMeasure(..)
                                     , UnitPower(..)
                                     ) where

import Camfort.Analysis.CommentAnnotator
import Data.Data
import Data.List
import Data.Char (isLetter, isNumber, isAlphaNum, toLower)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (UnitStatement)
	| HappyAbsSyn5 (Maybe [String])
	| HappyAbsSyn6 ([String])
	| HappyAbsSyn7 (UnitOfMeasure)
	| HappyAbsSyn10 (UnitPower)
	| HappyAbsSyn11 (Integer)
	| HappyAbsSyn12 (String)

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
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44 :: () => Int -> ({-HappyReduction (Either AnnotationParseError) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)

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
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24 :: () => ({-HappyReduction (Either AnnotationParseError) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either AnnotationParseError) HappyAbsSyn)

action_0 (13) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (13) = happyShift action_2
action_1 _ = happyFail

action_2 (14) = happyShift action_8
action_2 (15) = happyShift action_9
action_2 (23) = happyShift action_11
action_2 (7) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 _ = happyFail

action_3 (25) = happyAccept
action_3 _ = happyFail

action_4 (14) = happyShift action_8
action_4 (15) = happyShift action_9
action_4 (21) = happyShift action_10
action_4 (23) = happyShift action_11
action_4 (7) = happyGoto action_5
action_4 (8) = happyGoto action_6
action_4 (9) = happyGoto action_7
action_4 _ = happyFail

action_5 (20) = happyShift action_21
action_5 (21) = happyShift action_22
action_5 (5) = happyGoto action_20
action_5 _ = happyReduce_4

action_6 (14) = happyShift action_8
action_6 (23) = happyShift action_19
action_6 (9) = happyGoto action_18
action_6 _ = happyReduce_8

action_7 (19) = happyShift action_17
action_7 _ = happyReduce_14

action_8 _ = happyReduce_17

action_9 _ = happyReduce_9

action_10 (14) = happyShift action_16
action_10 _ = happyFail

action_11 (14) = happyShift action_8
action_11 (15) = happyShift action_14
action_11 (23) = happyShift action_11
action_11 (24) = happyShift action_15
action_11 (7) = happyGoto action_12
action_11 (8) = happyGoto action_13
action_11 (9) = happyGoto action_7
action_11 _ = happyFail

action_12 (20) = happyShift action_21
action_12 _ = happyFail

action_13 (14) = happyShift action_8
action_13 (23) = happyShift action_19
action_13 (24) = happyShift action_35
action_13 (9) = happyGoto action_18
action_13 _ = happyReduce_8

action_14 (24) = happyShift action_34
action_14 _ = happyReduce_9

action_15 _ = happyReduce_11

action_16 (22) = happyShift action_33
action_16 _ = happyFail

action_17 (15) = happyShift action_29
action_17 (16) = happyShift action_30
action_17 (18) = happyShift action_31
action_17 (23) = happyShift action_32
action_17 (10) = happyGoto action_26
action_17 (11) = happyGoto action_27
action_17 (12) = happyGoto action_28
action_17 _ = happyFail

action_18 (19) = happyShift action_17
action_18 _ = happyReduce_12

action_19 (14) = happyShift action_8
action_19 (15) = happyShift action_9
action_19 (23) = happyShift action_11
action_19 (7) = happyGoto action_12
action_19 (8) = happyGoto action_13
action_19 (9) = happyGoto action_7
action_19 _ = happyFail

action_20 _ = happyReduce_1

action_21 (14) = happyShift action_8
action_21 (23) = happyShift action_19
action_21 (9) = happyGoto action_25
action_21 _ = happyFail

action_22 (14) = happyShift action_24
action_22 (6) = happyGoto action_23
action_22 _ = happyReduce_7

action_23 _ = happyReduce_3

action_24 (17) = happyShift action_39
action_24 _ = happyReduce_5

action_25 (19) = happyShift action_17
action_25 _ = happyReduce_13

action_26 _ = happyReduce_15

action_27 _ = happyReduce_18

action_28 _ = happyReduce_21

action_29 _ = happyReduce_24

action_30 _ = happyReduce_23

action_31 (15) = happyShift action_29
action_31 (16) = happyShift action_30
action_31 (12) = happyGoto action_38
action_31 _ = happyFail

action_32 (15) = happyShift action_29
action_32 (16) = happyShift action_30
action_32 (18) = happyShift action_31
action_32 (11) = happyGoto action_37
action_32 (12) = happyGoto action_28
action_32 _ = happyFail

action_33 (14) = happyShift action_8
action_33 (15) = happyShift action_9
action_33 (23) = happyShift action_11
action_33 (7) = happyGoto action_36
action_33 (8) = happyGoto action_6
action_33 (9) = happyGoto action_7
action_33 _ = happyFail

action_34 _ = happyReduce_10

action_35 _ = happyReduce_16

action_36 (20) = happyShift action_21
action_36 _ = happyReduce_2

action_37 (20) = happyShift action_41
action_37 (24) = happyShift action_42
action_37 _ = happyFail

action_38 _ = happyReduce_22

action_39 (14) = happyShift action_24
action_39 (6) = happyGoto action_40
action_39 _ = happyReduce_7

action_40 _ = happyReduce_6

action_41 (15) = happyShift action_29
action_41 (16) = happyShift action_30
action_41 (18) = happyShift action_31
action_41 (11) = happyGoto action_43
action_41 (12) = happyGoto action_28
action_41 _ = happyFail

action_42 _ = happyReduce_19

action_43 (24) = happyShift action_44
action_43 _ = happyFail

action_44 _ = happyReduce_20

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (UnitAssignment happy_var_3 happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (UnitAlias happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
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
happyReduction_5 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 _
	(HappyTerminal happy_var_2)
	(HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  6 happyReduction_7
happyReduction_7  =  HappyAbsSyn6
		 ([]
	)

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (Unitless
	)

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 _
	_
	_
	 =  HappyAbsSyn7
		 (Unitless
	)

happyReduce_11 = happySpecReduce_2  7 happyReduction_11
happyReduction_11 _
	_
	 =  HappyAbsSyn7
		 (Unitless
	)

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (UnitProduct happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (UnitQuotient happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (UnitExponentiation happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 (HappyTerminal (TId happy_var_1))
	 =  HappyAbsSyn7
		 (UnitBasic happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (UnitPowerInteger happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (UnitPowerInteger happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 10 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (UnitPowerRational happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (read happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (read $ '-' : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn12
		 ("1"
	)

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TId "unit" -> cont 13;
	TId happy_dollar_dollar -> cont 14;
	TNum "1" -> cont 15;
	TNum happy_dollar_dollar -> cont 16;
	TComma -> cont 17;
	TMinus -> cont 18;
	TExponentiation -> cont 19;
	TDivision -> cont 20;
	TDoubleColon -> cont 21;
	TEqual -> cont 22;
	TLeftPar -> cont 23;
	TRightPar -> cont 24;
	_ -> happyError' (tk:tks)
	}

happyError_ 25 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Either AnnotationParseError a -> (a -> Either AnnotationParseError b) -> Either AnnotationParseError b
happyThen = (>>=)
happyReturn :: () => a -> Either AnnotationParseError a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either AnnotationParseError a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Either AnnotationParseError a
happyError' = happyError

parseUnit tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data UnitStatement =
   UnitAssignment (Maybe [String]) UnitOfMeasure
 | UnitAlias String UnitOfMeasure
  deriving Data

instance Show UnitStatement where
  show (UnitAssignment (Just ss) uom) = "= unit (" ++ show uom ++ ") :: " ++ (intercalate "," ss)
  show (UnitAssignment Nothing uom) = "= unit (" ++ show uom ++ ")"
  show (UnitAlias s uom) = "= unit :: " ++ s ++ " = " ++ show uom

data UnitOfMeasure =
   Unitless
 | UnitBasic String
 | UnitProduct UnitOfMeasure UnitOfMeasure
 | UnitQuotient UnitOfMeasure UnitOfMeasure
 | UnitExponentiation UnitOfMeasure UnitPower
  deriving Data

instance Show UnitOfMeasure where
  show Unitless = "1"
  show (UnitBasic s) = s
  show (UnitProduct uom1 uom2) = show uom1 ++ " " ++ show uom2
  show (UnitQuotient uom1 uom2) = show uom1 ++ " / " ++ show uom2
  show (UnitExponentiation uom exp) = show uom ++ "** (" ++ show exp ++ ")"

data UnitPower =
   UnitPowerInteger Integer
 | UnitPowerRational Integer Integer
 deriving Data

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

lexer :: String -> Either AnnotationParseError [ Token ]
lexer ('=':xs) = lexer' xs
lexer _ = Left NotAnnotation

addToTokens :: Token -> String -> Either AnnotationParseError [ Token ]
addToTokens tok rest = do
 tokens <- lexer' rest
 return $ tok : tokens

lexer' :: String -> Either AnnotationParseError [ Token ]
lexer' [] = Right []
lexer' ['\n']  = Right []
lexer' ['\r', '\n']  = Right []
lexer' ['\r']  = Right [] -- windows
lexer' (' ':xs) = lexer' xs
lexer' ('\t':xs) = lexer' xs
lexer' (':':':':xs) = addToTokens TDoubleColon xs
lexer' ('*':'*':xs) = addToTokens TExponentiation xs
lexer' (',':xs) = addToTokens TComma xs
lexer' ('/':xs) = addToTokens TDivision xs
lexer' ('-':xs) = addToTokens TMinus xs
lexer' ('=':xs) = addToTokens TEqual xs
lexer' ('(':xs) = addToTokens TLeftPar xs
lexer' (')':xs) = addToTokens TRightPar xs
lexer' (x:xs)
 | isLetter x = aux (\c -> isAlphaNum c || c `elem` ['\'','_','-']) TId
 | isNumber x = aux isNumber TNum
 | otherwise = failWith $ "Not valid unit syntax at " ++ show (x:xs)
 where
   aux p cons =
     let (target, rest) = span p xs
     in lexer' rest >>= (\tokens -> return $ cons (x:target) : tokens)

unitParser :: String -> Either AnnotationParseError UnitStatement
unitParser src = do
 tokens <- lexer $ map toLower src
 parseUnit tokens

happyError :: [ Token ] -> Either AnnotationParseError a
happyError t = failWith $ "Could not parse specification at: " ++ show t
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

