{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Camfort.Analysis.CommentAnnotatorSpec (spec) where

import Test.Hspec

import Data.Data
import Control.Monad.Writer.Strict

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position

import Camfort.Analysis.CommentAnnotator

p = SrcSpan (Position 0 1 1) (Position 0 1 1)

spec =
  describe "Comment annotator" $ do
    it "annotates with no comment blocks" $
      runWriter (annotateComments (\_ -> Right "") pf) `shouldBe` (pf, [])

    it "links & annotates single comment block" $
      runWriter (annotateComments (\_ -> Right "hello") pf2) `shouldBe` (pf2e, [])

    it "link multiple comments to single statement" $
      runWriter (annotateComments (\s -> Right $ "!!!" ++ s) pf3) `shouldBe` (pf3e, [])

    it "link comments to separate targets" $
      runWriter (annotateComments (\s -> Right $ "!!!" ++ s) pf4) `shouldBe` (pf4e, [])

    it "generates warnings when there is a partial match" $ do
      let parser _ = Left $ ProbablyAnnotation "This is a warning."
                     :: Either AnnotationParseError String
      shouldBe (runWriter (annotateComments parser pf5))
               (pf5e, [ "Error (1:1)-(1:1): This is a warning."
                      , "Error (1:1)-(1:1): This is a warning." ])

data A = A
  { annLink :: Maybe (Block A)
  , annAST :: Maybe String }
  deriving (Data, Eq, Show)

instance ASTEmbeddable A String where
  annotateWithAST a str = a { annAST = Just str }

instance Linkable A where
  link a block = a { annLink = Just block }

-- Some helper functions
varGen x = ExpValue ea p (ValVariable x)
intGen i = ExpValue ea p (ValInteger (show i))
wrapBlocks bs = ProgramFile (MetaInfo { miVersion = Fortran90, miFilename = "<unknown>" }) [ pu ]
  where
    pu = PUModule ea p "my_module" bs Nothing

-- Test cases

ea = A Nothing Nothing

pf = wrapBlocks bs
bs = [ BlStatement ea p Nothing (StPause ea p Nothing) ]

pf2 = wrapBlocks bs2
bs2 =
  [ BlComment ea p (Comment "something")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf2e = wrapBlocks bs2e
bs2e =
  [ BlComment (A (Just (bs2e !! 1)) (Just "hello")) p (Comment "something")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf3 = wrapBlocks bs3
bs3 =
  [ BlComment ea p (Comment "mistral")
  , BlComment ea p (Comment "orhan")
  , BlComment ea p (Comment "jean-pierre")
  , BlComment ea p (Comment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf3e = wrapBlocks bs3e
bs3e =
  [ BlComment (A (Just (last bs3e)) (Just "!!!mistral")) p (Comment "mistral")
  , BlComment (A (Just (last bs3e)) (Just "!!!orhan")) p (Comment "orhan")
  , BlComment (A (Just (last bs3e)) (Just "!!!jean-pierre")) p (Comment "jean-pierre")
  , BlComment (A (Just (last bs3e)) (Just "!!!contrastin")) p (Comment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf4 = wrapBlocks bs4
bs4 =
  [ BlComment ea p (Comment "mistral")
  , BlComment ea p (Comment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing)
  , BlComment ea p (Comment "dominic")
  , BlComment ea p (Comment "orchard")
  , BlStatement ea p Nothing (StExpressionAssign ea p (varGen "x") (intGen 42)) ]

pf4e = wrapBlocks bs4e
bs4e =
  [ BlComment (A (Just (bs4e !! 2)) (Just "!!!mistral")) p (Comment "mistral")
  , BlComment (A (Just (bs4e !! 2)) (Just "!!!contrastin")) p (Comment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing)
  , BlComment (A (Just (last bs4e)) (Just "!!!dominic")) p (Comment "dominic")
  , BlComment (A (Just (last bs4e)) (Just "!!!orchard")) p (Comment "orchard")
  , BlStatement ea p Nothing (StExpressionAssign ea p (varGen "x") (intGen 42)) ]

pf5 = wrapBlocks bs5
bs5 =
  [ BlComment ea p (Comment "comment 1")
  , BlComment ea p (Comment "comment 2")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf5e = wrapBlocks bs5e
bs5e =
  [ BlComment (A (Just (last bs5e)) Nothing) p (Comment "comment 1")
  , BlComment (A (Just (last bs5e)) Nothing) p (Comment "comment 2")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]
