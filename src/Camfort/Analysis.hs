{- |
Module      :  Camfort.Analysis
Description :  Helpers for refactoring and analysis.
Copyright   :  (c) 2017, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish
License     :  Apache-2.0

Maintainer  :  dom.orchard@gmail.com
Stability   :  experimental
-}

module Camfort.Analysis
  (
    -- * Refactoring
    Refactoring
  , runRefactoring
    -- * Analysis
  , Analysis
  , mkAnalysis
  , runAnalysis
  , runAnalysisWithSummary
  ) where

-- ** Refactoring

-- | A @Refactoring s a a'@ refactors data of
-- type @a@ into data of type @a'@, and produces
-- information of type @s@.
type Refactoring s a a' = a -> (s, a')

-- | Run the given 'Refactoring' on some input data.
runRefactoring :: Refactoring s a a' -> a -> (s, a')
runRefactoring = id

-- ** Analysis

-- | An Analysis is a 'Refactoring' that only
-- produces additional information.
type Analysis s a = Refactoring s a ()

-- | Convert a function to an 'Analysis'.
mkAnalysis :: (a -> s) -> Analysis s a
mkAnalysis f x = (f x, ())

-- | Run the given 'Analysis' on some input data.
runAnalysis :: Analysis s a -> a -> s
runAnalysis a = fst . runRefactoring a

-- | Run the given 'Analysis' on multiple data, and summarise the results.
runAnalysisWithSummary :: (Monoid s) => Analysis s a -> [a] -> s
runAnalysisWithSummary analysis = mconcat . fmap (runAnalysis analysis)
