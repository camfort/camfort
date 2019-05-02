{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Camfort.Analysis.CommentAnnotatorSpec (spec) where

import Test.Hspec

import Data.Data
import Control.Monad.Identity (runIdentity)
import Control.Monad.Writer.Strict

import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position

import Camfort.Analysis.CommentAnnotator
import Camfort.Specification.Parser (mkParser, parseError, SpecParser)

p = SrcSpan (Position 0 1 1 "" Nothing) (Position 0 1 1 "" Nothing)

annotateWith :: (String -> String) -> ProgramFile A -> ProgramFile A
annotateWith s = runIdentity . annotateComments trivialParser ignore
  where trivialParser = mkParser (Right . s) []
        ignore        = const . const . pure $ ()

spec =
  describe "Comment annotator" $ do
    it "annotates with no comment blocks" $
      annotateWith (const "") pf `shouldBe` pf

    it "links & annotates single comment block" $
      annotateWith (const "hello") pf2 `shouldBe` pf2e

    it "link multiple comments to single statement" $
      annotateWith ("!!!"++) pf3 `shouldBe` pf3e

    it "link comments to separate targets" $
      annotateWith ("!!!"++) pf4 `shouldBe` pf4e

    it "allows handling of parse errors" $ do
      let parser :: SpecParser String String
          parser = mkParser (const $ Left "This is a warning.") []
      shouldBe (runWriter (annotateComments parser (\srcSpan err -> tell [(srcSpan, err)]) pf5))
               (pf5e, [ (initSrcSpan, parseError "This is a warning.")
                      , (initSrcSpan, parseError "This is a warning.")])

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

mkComment :: String -> Comment a
mkComment = Comment . ("= "++)

ea = A Nothing Nothing

pf = wrapBlocks bs
bs = [ BlStatement ea p Nothing (StPause ea p Nothing) ]

pf2 = wrapBlocks bs2
bs2 =
  [ BlComment ea p (mkComment "something")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf2e = wrapBlocks bs2e
bs2e =
  [ BlComment (A (Just (bs2e !! 1)) (Just "hello")) p (mkComment "something")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf3 = wrapBlocks bs3
bs3 =
  [ BlComment ea p (mkComment "mistral")
  , BlComment ea p (mkComment "orhan")
  , BlComment ea p (mkComment "jean-pierre")
  , BlComment ea p (mkComment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf3e = wrapBlocks bs3e
bs3e =
  [ BlComment (A (Just (last bs3e)) (Just "!!!mistral")) p (mkComment "mistral")
  , BlComment (A (Just (last bs3e)) (Just "!!!orhan")) p (mkComment "orhan")
  , BlComment (A (Just (last bs3e)) (Just "!!!jean-pierre")) p (mkComment "jean-pierre")
  , BlComment (A (Just (last bs3e)) (Just "!!!contrastin")) p (mkComment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf4 = wrapBlocks bs4
bs4 =
  [ BlComment ea p (mkComment "mistral")
  , BlComment ea p (mkComment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing)
  , BlComment ea p (mkComment "dominic")
  , BlComment ea p (mkComment "orchard")
  , BlStatement ea p Nothing (StExpressionAssign ea p (varGen "x") (intGen 42)) ]

pf4e = wrapBlocks bs4e
bs4e =
  [ BlComment (A (Just (bs4e !! 2)) (Just "!!!mistral")) p (mkComment "mistral")
  , BlComment (A (Just (bs4e !! 2)) (Just "!!!contrastin")) p (mkComment "contrastin")
  , BlStatement ea p Nothing (StPause ea p Nothing)
  , BlComment (A (Just (last bs4e)) (Just "!!!dominic")) p (mkComment "dominic")
  , BlComment (A (Just (last bs4e)) (Just "!!!orchard")) p (mkComment "orchard")
  , BlStatement ea p Nothing (StExpressionAssign ea p (varGen "x") (intGen 42)) ]

pf5 = wrapBlocks bs5
bs5 =
  [ BlComment ea p (mkComment "comment 1")
  , BlComment ea p (mkComment "comment 2")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]

pf5e = wrapBlocks bs5e
bs5e =
  [ BlComment (A (Just (last bs5e)) Nothing) p (mkComment "comment 1")
  , BlComment (A (Just (last bs5e)) Nothing) p (mkComment "comment 2")
  , BlStatement ea p Nothing (StPause ea p Nothing) ]
