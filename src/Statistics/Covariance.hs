-- |
-- Module      :  Statistics.Covariance
-- Description :  Estimate covariance matrices from sample data
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Sep 14 13:02:15 2021.
module Statistics.Covariance
  ( -- * Empirical estimator
    empiricalCovariance,

    -- * Shrinkage based estimators

    -- | See the overview on shrinkage estimators provided by
    -- [scikit-learn](https://scikit-learn.org/dev/modules/covariance.html#shrunk-covariance).
    module Statistics.Covariance.LedoitWolf,
    module Statistics.Covariance.RaoBlackwellLedoitWolf,
    module Statistics.Covariance.OracleApproximatingShrinkage,

    -- * Helper functions
    scale,
    rescaleWith,
  )
where

import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as L
import qualified Numeric.LinearAlgebra.Devel as L
import Statistics.Covariance.LedoitWolf
import Statistics.Covariance.OracleApproximatingShrinkage
import Statistics.Covariance.RaoBlackwellLedoitWolf
import qualified Statistics.Sample as S

-- | Empirical or sample covariance.
--
-- Classical maximum-likelihood estimator; asymptotically unbiased but sensitive
-- to outliers.
--
-- Re-export of the empirical covariance 'L.meanCov' provided by
-- [hmatrix](https://hackage.haskell.org/package/hmatrix).
--
-- NOTE: This function may call 'error'.
empiricalCovariance ::
  -- | Data matrix of dimension NxP, where N is the number of observations, and
  -- P is the number of parameters.
  L.Matrix Double ->
  L.Herm Double
empiricalCovariance = snd . L.meanCov

scaleWith ::
  -- Vector of means (length P).
  L.Vector Double ->
  -- Vector of standard deviations (length P)
  L.Vector Double ->
  -- Data matrix of dimension N x P.
  L.Matrix Double ->
  -- Data matrix with means 0 and variance 1.0.
  L.Matrix Double
scaleWith ms ss = L.mapMatrixWithIndex (\(_, j) x -> (x - ms VS.! j) / (ss VS.! j))

-- | Center and scales columns.
--
-- Normalize a data matrix to have means 0 and standard deviations/variances
-- 1.0. The estimated covariance matrix of a scaled data matrix is a correlation
-- matrix, which is easier to estimate.
scale ::
  -- | Data matrix of dimension NxP, where N is the number of observations, and
  -- P is the number of parameters.
  L.Matrix Double ->
  -- | (Means, Standard deviations, Centered and scaled matrix)
  (L.Vector Double, L.Vector Double, L.Matrix Double)
scale xs = (ms, ss, scaleWith ms ss xs)
  where
    msVs = map S.meanVariance $ L.toColumns xs
    ms = L.fromList $ map fst msVs
    ss = L.fromList $ map (sqrt . snd) msVs

-- | Convert a correlation matrix with given standard deviations to original
-- scale.
rescaleWith ::
  -- | Vector of standard deviations (length P)
  L.Vector Double ->
  -- | Correlation matrix.
  L.Matrix Double ->
  -- | Covariance matrix.
  L.Matrix Double
rescaleWith ss = L.mapMatrixWithIndex (\(i, j) x -> x * (ss VS.! i) * (ss VS.! j))
