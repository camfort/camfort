{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Camfort.Specification.Hoare.TranslateSpec (spec) where

import           Control.Exception                     (Exception (..))
import           Data.Foldable                         (traverse_)

import           Control.Lens
import           Data.Map                              (Map)
import qualified Data.Map                              as Map


import           Camfort.Specification.Hoare.Parser
import           Camfort.Specification.Hoare.Parser.Types
import           Camfort.Specification.Hoare.Syntax
import           Camfort.Specification.Hoare.Translate
import           Camfort.Specification.Hoare.Types
import           Camfort.Specification.Parser          (runParser)
import qualified Camfort.Specification.Parser          as Parser
import qualified Language.Fortran.AST                  as F

import           Language.Verification                 (Var(..), varName, Location)

import           Test.Hspec                            hiding (Spec)
import qualified Test.Hspec                            as Test


-- TODO: This currently just prints things, make it actually do some Hspec
-- stuff!

spec :: IO ()
spec = do
  let (&&&) = (,)

      allVars :: [SomeVar F.Name]
      allVars =
        [ Some (Var "x" :: Var F.Name Integer)
        , Some (Var "y" :: Var F.Name Integer)
        , Some (Var "z" :: Var F.Name Integer)
        ]

      tests =
        [ "! static_assert pre(\"x\" = \"y\")" &&& allVars
        , "! static_assert invariant(\"x + 3\" < \"y - z\" & \"x + 3\" > \"y + z\")" &&& allVars
        , "! static_assert post(\"x + 3\" < \"y - z\" & \"x + 3\" > \"y + z\" | \"x\" >= \"7\")" &&& allVars
        , "! static_assert seq(\"x + 3\" < \"y - z\" -> \"x + 3\" > \"y + z\" -> \"x\" >= \"7\")" &&& allVars
        ]

  traverse_ (uncurry demonstrate) tests


demonstrate :: String -> [SomeVar F.Name] -> IO ()
demonstrate input vars =
  do putStrLn $ "Using input: " ++ input
     putStrLn $ " - Parsing..."
     ps <- either (fail $ "failed to parse " ++ input) return (parse input)
     putStrLn $ " - Translating..."
     ts <- either (fail . displayException) return (translateSpec vars ps)
     putStrLn $ " - Done!\n"
     putStrLn (show ts)
     putStrLn ""

translateSpec :: [SomeVar F.Name] -> PrimSpec ann -> Either (TranslateError ann) (Specification (TransFormula Bool))
translateSpec vars ps =
  runMonadTranslate (specFormula translateFormula ps)
  (defaultTranslateEnv & teVarsInScope .~ varsToMap vars)

parse :: String -> Either (Parser.SpecParseError HoareParseError) (PrimSpec ())
parse = runParser hoareParser

varsToMap :: [SomeVar String] -> Map SourceName (SomeVar NamePair)
varsToMap = Map.fromList . fmap varPair
  where
    varPair :: SomeVar String -> (SourceName, SomeVar NamePair)
    varPair (Some (Var l :: Var String a)) =
      ( SourceName l
      , Some (Var (NamePair (UniqueName l) (SourceName l)) :: Var NamePair a))
