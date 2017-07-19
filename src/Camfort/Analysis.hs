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

-- | A @Refactoring r a a'@ refactors data of
-- type @a@ into data of type @a'@, and produces
-- a report of type @r@.
type Refactoring r a a' = a -> (r, a')

-- | Run the given 'Refactoring' on some input data.
runRefactoring :: Refactoring r a a' -> a -> (r, a')
runRefactoring = id

-- ** Analysis

-- | An Analysis is a 'Refactoring' that only
-- produces a report.
type Analysis r a = Refactoring r a ()

-- | Convert a function to an 'Analysis'.
mkAnalysis :: (a -> r) -> Analysis r a
mkAnalysis f x = (f x, ())

-- | Run the given 'Analysis' on some input data.
runAnalysis :: Analysis r a -> a -> r
runAnalysis a = fst . runRefactoring a

-- | Run the given 'Analysis' on multiple data, and summarise the results.
runAnalysisWithSummary :: (Monoid r) => Analysis r a -> [a] -> r
runAnalysisWithSummary analysis = mconcat . fmap (runAnalysis analysis)
