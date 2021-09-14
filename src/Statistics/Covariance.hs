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
  ( empiricalCovariance,

    -- * Shrinkage based estimators
    module Statistics.Covariance.LedoitWolf,
    module Statistics.Covariance.RaoBlackwellLedoitWolf,
    module Statistics.Covariance.OracleApproximatingShrinkage,
  )
where

import qualified Numeric.LinearAlgebra as L
import Statistics.Covariance.LedoitWolf
import Statistics.Covariance.OracleApproximatingShrinkage
import Statistics.Covariance.RaoBlackwellLedoitWolf

-- | Empirical or sample covariance.
--
-- Classical maximum-likelihood estimator; asymptotically unbiased but sensitive
-- to outliers.
--
-- Re-export of the empirical covariance 'L.meanCov' provided by
-- [hmatrix](https://hackage.haskell.org/package/hmatrix).
--
-- NOTE: This function may call 'error'.
empiricalCovariance :: L.Matrix Double -> L.Herm Double
empiricalCovariance = snd . L.meanCov
